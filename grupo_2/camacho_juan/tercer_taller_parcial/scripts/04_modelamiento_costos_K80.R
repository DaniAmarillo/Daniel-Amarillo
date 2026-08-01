# ==============================================================================
# TALLER 3 - MINERÍA DE DATOS
# ETAPA 4: MODELAMIENTO DE COSTOS PARA COLELITIASIS (K80) - V2
# ==============================================================================
#
# Unidad de análisis:
#   Beneficiario con al menos un registro K80.
#
# Variable respuesta:
#   Costo total positivo acumulado del beneficiario durante el periodo
#   observado en la base.
#
# Modelos comparados:
#   1. GLM Gamma con enlace log
#   2. Regresión lognormal con corrección de Duan
#   3. Línea base: mediana del entrenamiento
#
# Selección:
#   El modelo se elige en validación por menor MAE.
#   La prueba se utiliza una sola vez para la evaluación final.
#
# IMPORTANTE:
#   - No vuelve a leer db_2026.csv.
#   - Utiliza data/processed/taller3_k80.sqlite.
# ==============================================================================

local({

# 0. CONFIGURACIÓN -------------------------------------------------------------

rm(list = ls())
invisible(gc(full = TRUE))

options(
  scipen = 999,
  stringsAsFactors = FALSE
)

set.seed(2016325)

paquetes <- c(
  "DBI",
  "RSQLite",
  "data.table",
  "ggplot2",
  "here"
)

faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes) > 0L) {
  stop(
    "Faltan estos paquetes:\n",
    paste(faltantes, collapse = ", "),
    "\n\nInstálelos con:\ninstall.packages(c(",
    paste(sprintf('"%s"', faltantes), collapse = ", "),
    "))"
  )
}

library(DBI)
library(RSQLite)
library(data.table)
library(ggplot2)
library(here)

dir.create(
  here("resultados"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("modelos"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("figuras"),
  recursive = TRUE,
  showWarnings = FALSE
)

ruta_sqlite <- here(
  "data",
  "processed",
  "taller3_k80.sqlite"
)

if (!file.exists(ruta_sqlite)) {
  stop(
    "No se encontró:\n",
    ruta_sqlite,
    "\nFinalice primero la etapa 2."
  )
}

con <- dbConnect(
  RSQLite::SQLite(),
  ruta_sqlite
)

on.exit({
  if (DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con)
  }
}, add = TRUE)

dbExecute(con, "PRAGMA temp_store = FILE")
dbExecute(con, "PRAGMA cache_size = -50000")
dbExecute(con, "PRAGMA busy_timeout = 60000")

if (
  !"variables_beneficiario_k80" %in%
    dbListTables(con)
) {
  stop(
    "No existe la tabla variables_beneficiario_k80."
  )
}


# 1. CARGA DE BENEFICIARIOS K80 ------------------------------------------------

consulta <- "
SELECT
  chave_funcional,
  particion,
  costo_total_positivo,
  edad,
  sexo,
  tipo_beneficiario,
  unidad_predominante,
  n_procedimientos,
  n_consultas,
  n_examenes,
  n_terapias,
  n_internaciones_registro,
  n_urgencias,
  n_registros_uci,
  n_registros_internado,
  n_registros_anestesia,
  dias_observacion,
  tuvo_gastroenterologia,
  tuvo_cirugia,
  tuvo_imagenologia,
  tuvo_procedimiento_biliar,
  prop_consultas,
  prop_examenes,
  prop_terapias,
  prop_internaciones,
  prop_urgencias

FROM variables_beneficiario_k80

WHERE target_k80 = 1
  AND costo_total_positivo > 0
"

datos <- as.data.table(
  dbGetQuery(
    con,
    consulta
  )
)

if (nrow(datos) != 306L) {
  warning(
    "Se esperaban 306 beneficiarios K80 con costo positivo, ",
    "pero se obtuvieron ",
    nrow(datos),
    "."
  )
}

datos[, particion :=
  as.character(particion)
]

conteo_particiones <- datos[
  ,
  .(
    n = .N,
    costo_minimo =
      min(costo_total_positivo),
    costo_mediano =
      median(costo_total_positivo),
    costo_promedio =
      mean(costo_total_positivo),
    costo_maximo =
      max(costo_total_positivo)
  ),
  by = particion
]

cat("\n============================================================\n")
cat("ETAPA 4: MODELAMIENTO DE COSTOS K80\n")
cat("============================================================\n")
print(conteo_particiones)


# 2. PREPROCESAMIENTO APRENDIDO EN ENTRENAMIENTO ------------------------------

variables_categoricas <- c(
  "sexo",
  "tipo_beneficiario",
  "unidad_predominante"
)

variables_numericas <- c(
  "edad",
  "log_n_procedimientos",
  "log_n_urgencias",
  "log_n_internaciones",
  "log_dias_observacion",
  "prop_internaciones",
  "prop_examenes",
  "tuvo_uci",
  "tuvo_anestesia",
  "tuvo_cirugia",
  "tuvo_imagenologia",
  "tuvo_procedimiento_biliar"
)

predictores <- c(
  variables_numericas,
  variables_categoricas
)


crear_derivadas <- function(dt) {

  salida <- copy(dt)

  columnas_conteo <- c(
    "n_procedimientos",
    "n_urgencias",
    "n_internaciones_registro",
    "dias_observacion",
    "n_registros_uci",
    "n_registros_anestesia"
  )

  for (variable in columnas_conteo) {
    salida[
      is.na(get(variable)) |
        !is.finite(get(variable)) |
        get(variable) < 0,
      (variable) := 0
    ]
  }

  salida[, log_n_procedimientos :=
    log1p(n_procedimientos)
  ]

  salida[, log_n_urgencias :=
    log1p(n_urgencias)
  ]

  salida[, log_n_internaciones :=
    log1p(n_internaciones_registro)
  ]

  salida[, log_dias_observacion :=
    log1p(dias_observacion)
  ]

  salida[, tuvo_uci :=
    as.integer(n_registros_uci > 0)
  ]

  salida[, tuvo_anestesia :=
    as.integer(n_registros_anestesia > 0)
  ]

  indicadores <- c(
    "tuvo_uci",
    "tuvo_anestesia",
    "tuvo_cirugia",
    "tuvo_imagenologia",
    "tuvo_procedimiento_biliar"
  )

  for (variable in indicadores) {
    salida[
      is.na(get(variable)),
      (variable) := 0L
    ]
  }

  proporciones <- c(
    "prop_internaciones",
    "prop_examenes"
  )

  for (variable in proporciones) {
    salida[
      is.na(get(variable)) |
        !is.finite(get(variable)),
      (variable) := 0
    ]
  }

  salida[, log_costo :=
    log(costo_total_positivo)
  ]

  salida
}


ajustar_preprocesador <- function(
    entrenamiento,
    frecuencia_minima = 5L) {

  entrenamiento <- crear_derivadas(
    entrenamiento
  )

  medianas <- vapply(
    variables_numericas,
    function(variable) {

      valores <- as.numeric(
        entrenamiento[[variable]]
      )

      valores <- valores[
        is.finite(valores)
      ]

      if (length(valores) == 0L) {
        return(0)
      }

      as.numeric(
        median(valores)
      )
    },
    numeric(1)
  )

  niveles <- lapply(
    variables_categoricas,
    function(variable) {

      valores <- as.character(
        entrenamiento[[variable]]
      )

      valores[
        is.na(valores) |
          trimws(valores) == ""
      ] <- "SIN INFORMACION"

      conteo <- table(valores)

      frecuentes <- names(
        conteo[
          conteo >= frecuencia_minima
        ]
      )

      unique(
        c(
          frecuentes,
          "SIN INFORMACION",
          "OTROS"
        )
      )
    }
  )

  names(niveles) <- variables_categoricas

  list(
    medianas = medianas,
    niveles = niveles
  )
}


aplicar_preprocesador <- function(
    dt,
    preprocesador) {

  salida <- crear_derivadas(
    dt
  )

  for (variable in variables_numericas) {

    valores <- as.numeric(
      salida[[variable]]
    )

    valores[
      is.na(valores) |
        !is.finite(valores)
    ] <- preprocesador$medianas[[variable]]

    salida[
      ,
      (variable) := valores
    ]
  }

  for (variable in variables_categoricas) {

    valores <- as.character(
      salida[[variable]]
    )

    valores[
      is.na(valores) |
        trimws(valores) == ""
    ] <- "SIN INFORMACION"

    niveles_validos <-
      preprocesador$niveles[[variable]]

    valores[
      !valores %in% niveles_validos
    ] <- "OTROS"

    salida[
      ,
      (variable) := factor(
        valores,
        levels = niveles_validos
      )
    ]
  }

  salida
}


train_raw <- datos[
  particion == "entrenamiento"
]

val_raw <- datos[
  particion == "validacion"
]

test_raw <- datos[
  particion == "prueba"
]

preprocesador <- ajustar_preprocesador(
  train_raw
)

train <- aplicar_preprocesador(
  train_raw,
  preprocesador
)

val <- aplicar_preprocesador(
  val_raw,
  preprocesador
)

test <- aplicar_preprocesador(
  test_raw,
  preprocesador
)


# 3. FÓRMULA ------------------------------------------------------------------

formula_costos <- as.formula(
  paste(
    "costo_total_positivo ~",
    paste(
      predictores,
      collapse = " + "
    )
  )
)

formula_log <- as.formula(
  paste(
    "log_costo ~",
    paste(
      predictores,
      collapse = " + "
    )
  )
)


# 4. AJUSTE DE MODELOS ---------------------------------------------------------

cat("\nAjustando GLM Gamma...\n")

modelo_gamma <- glm(
  formula = formula_costos,
  data = as.data.frame(train),
  family = Gamma(
    link = "log"
  ),
  control = glm.control(
    maxit = 200
  )
)

if (!isTRUE(modelo_gamma$converged)) {
  warning(
    "El GLM Gamma no declaró convergencia."
  )
}

cat("Ajustando modelo lognormal...\n")

modelo_lognormal <- lm(
  formula = formula_log,
  data = as.data.frame(train)
)

factor_duan <- mean(
  exp(
    residuals(
      modelo_lognormal
    )
  ),
  na.rm = TRUE
)

pred_gamma <- function(nuevos) {
  pmax(
    as.numeric(
      predict(
        modelo_gamma,
        newdata = as.data.frame(nuevos),
        type = "response"
      )
    ),
    .Machine$double.eps
  )
}

pred_lognormal <- function(nuevos) {
  pmax(
    exp(
      as.numeric(
        predict(
          modelo_lognormal,
          newdata = as.data.frame(nuevos)
        )
      )
    ) * factor_duan,
    .Machine$double.eps
  )
}

mediana_train <- median(
  train$costo_total_positivo
)


# 5. MÉTRICAS -----------------------------------------------------------------

metricas_costo <- function(
    observado,
    predicho,
    modelo,
    conjunto) {

  error <- predicho - observado

  sse <- sum(
    error^2
  )

  sst <- sum(
    (
      observado -
        mean(observado)
    )^2
  )

  data.table(
    modelo = modelo,
    conjunto = conjunto,
    n = length(observado),
    costo_observado_promedio =
      mean(observado),
    costo_predicho_promedio =
      mean(predicho),
    mae =
      mean(abs(error)),
    rmse =
      sqrt(mean(error^2)),
    rmsle =
      sqrt(
        mean(
          (
            log1p(predicho) -
              log1p(observado)
          )^2
        )
      ),
    smape =
      mean(
        2 * abs(error) /
          pmax(
            abs(observado) +
              abs(predicho),
            .Machine$double.eps
          )
      ),
    r2 =
      1 - sse / sst,
    sesgo_medio =
      mean(error),
    razon_totales =
      sum(predicho) /
        sum(observado)
  )
}


evaluar_conjunto <- function(
    dt,
    nombre_conjunto) {

  y <- dt$costo_total_positivo

  p_gamma <- pred_gamma(
    dt
  )

  p_lognormal <- pred_lognormal(
    dt
  )

  p_base <- rep(
    mediana_train,
    nrow(dt)
  )

  metricas <- rbindlist(
    list(
      metricas_costo(
        y,
        p_gamma,
        "GLM Gamma",
        nombre_conjunto
      ),
      metricas_costo(
        y,
        p_lognormal,
        "Regresión lognormal",
        nombre_conjunto
      ),
      metricas_costo(
        y,
        p_base,
        "Línea base: mediana",
        nombre_conjunto
      )
    )
  )

  predicciones <- rbindlist(
    list(
      data.table(
        chave_funcional =
          dt$chave_funcional,
        conjunto =
          nombre_conjunto,
        modelo =
          "GLM Gamma",
        observado =
          y,
        predicho =
          p_gamma
      ),
      data.table(
        chave_funcional =
          dt$chave_funcional,
        conjunto =
          nombre_conjunto,
        modelo =
          "Regresión lognormal",
        observado =
          y,
        predicho =
          p_lognormal
      ),
      data.table(
        chave_funcional =
          dt$chave_funcional,
        conjunto =
          nombre_conjunto,
        modelo =
          "Línea base: mediana",
        observado =
          y,
        predicho =
          p_base
      )
    )
  )

  list(
    metricas = metricas,
    predicciones = predicciones
  )
}


resultado_val <- evaluar_conjunto(
  val,
  "Validación"
)

resultado_test <- evaluar_conjunto(
  test,
  "Prueba"
)

metricas <- rbindlist(
  list(
    resultado_val$metricas,
    resultado_test$metricas
  )
)

predicciones <- rbindlist(
  list(
    resultado_val$predicciones,
    resultado_test$predicciones
  )
)

seleccion <- metricas[
  conjunto == "Validación" &
    modelo != "Línea base: mediana"
][
  order(
    mae,
    rmsle
  )
][
  1
]

modelo_seleccionado <-
  seleccion$modelo[[1]]

predicciones_seleccionadas <- predicciones[
  modelo == modelo_seleccionado
]


# 6. COEFICIENTES --------------------------------------------------------------

coef_gamma <- summary(
  modelo_gamma
)$coefficients

coeficientes_gamma <- data.table(
  termino =
    rownames(coef_gamma),
  estimacion =
    coef_gamma[, 1],
  error_estandar =
    coef_gamma[, 2],
  estadistico =
    coef_gamma[, 3],
  valor_p =
    coef_gamma[, 4]
)

coeficientes_gamma[
  ,
  factor_multiplicativo :=
    exp(estimacion)
]

coef_log <- summary(
  modelo_lognormal
)$coefficients

coeficientes_lognormal <- data.table(
  termino =
    rownames(coef_log),
  estimacion =
    coef_log[, 1],
  error_estandar =
    coef_log[, 2],
  estadistico =
    coef_log[, 3],
  valor_p =
    coef_log[, 4]
)

coeficientes_lognormal[
  ,
  factor_multiplicativo :=
    exp(estimacion)
]


# 7. PERFILES DESCRIPTIVOS DE COSTO -------------------------------------------

datos_perfiles <- copy(datos)

datos_perfiles[
  ,
  grupo_edad := cut(
    edad,
    breaks = c(
      -Inf,
      29,
      44,
      59,
      74,
      Inf
    ),
    labels = c(
      "Menos de 30",
      "30-44",
      "45-59",
      "60-74",
      "75 o más"
    )
  )
]

datos_perfiles[
  ,
  tuvo_internacion :=
    as.integer(
      n_registros_internado > 0 |
        n_internaciones_registro > 0
    )
]

resumir_perfil <- function(
    dt,
    variable,
    nombre_variable) {

  resultado <- dt[
    ,
    .(
      n = .N,
      costo_promedio =
        mean(costo_total_positivo),
      costo_mediano =
        as.numeric(
          median(costo_total_positivo)
        ),
      costo_q1 =
        as.numeric(
          quantile(
            costo_total_positivo,
            0.25,
            names = FALSE
          )
        ),
      costo_q3 =
        as.numeric(
          quantile(
            costo_total_positivo,
            0.75,
            names = FALSE
          )
        )
    ),
    by = .(
      categoria = as.character(
        get(variable)
      )
    )
  ]

  resultado[
    ,
    perfil :=
      nombre_variable
  ]

  resultado
}

perfiles_costo <- rbindlist(
  list(
    resumir_perfil(
      datos_perfiles,
      "sexo",
      "Sexo"
    ),
    resumir_perfil(
      datos_perfiles,
      "grupo_edad",
      "Grupo de edad"
    ),
    resumir_perfil(
      datos_perfiles,
      "tuvo_cirugia",
      "Cirugía"
    ),
    resumir_perfil(
      datos_perfiles,
      "tuvo_internacion",
      "Internación"
    ),
    resumir_perfil(
      datos_perfiles,
      "unidad_predominante",
      "Unidad predominante"
    )
  ),
  fill = TRUE
)

perfiles_costo[
  perfil == "Cirugía",
  categoria := fifelse(
    categoria == "1",
    "Sí",
    "No"
  )
]

perfiles_costo[
  perfil == "Internación",
  categoria := fifelse(
    categoria == "1",
    "Sí",
    "No"
  )
]


# 8. CALIBRACIÓN POR QUINTILES -------------------------------------------------

calibracion <- copy(
  predicciones_seleccionadas[
    conjunto == "Prueba"
  ]
)

calibracion[
  ,
  quintil_predicho :=
    cut(
      frank(
        predicho,
        ties.method = "average"
      ) / .N,
      breaks = c(
        0,
        0.2,
        0.4,
        0.6,
        0.8,
        1
      ),
      include.lowest = TRUE,
      labels = paste0(
        "Q",
        1:5
      )
    )
]

calibracion_resumen <- calibracion[
  ,
  .(
    n = .N,
    costo_observado_promedio =
      mean(observado),
    costo_predicho_promedio =
      mean(predicho),
    razon_predicho_observado =
      mean(predicho) /
        mean(observado)
  ),
  by = quintil_predicho
]


# 9. GRÁFICOS -----------------------------------------------------------------

grafico_observado_predicho <- ggplot(
  predicciones_seleccionadas[
    conjunto == "Prueba"
  ],
  aes(
    x = observado,
    y = predicho
  )
) +
  geom_point(
    alpha = 0.65
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = 2
  ) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title =
      "Costo observado frente a costo predicho",
    subtitle =
      paste(
        "Modelo seleccionado:",
        modelo_seleccionado
      ),
    x =
      "Costo observado (escala log10)",
    y =
      "Costo predicho (escala log10)"
  ) +
  theme_minimal(
    base_size = 12
  )

ggsave(
  here(
    "figuras",
    "10_observado_vs_predicho_costo_k80.png"
  ),
  grafico_observado_predicho,
  width = 8,
  height = 5.5,
  dpi = 300
)

residuos_prueba <- copy(
  predicciones_seleccionadas[
    conjunto == "Prueba"
  ]
)

residuos_prueba[
  ,
  residuo_log :=
    log1p(observado) -
      log1p(predicho)
]

grafico_residuos <- ggplot(
  residuos_prueba,
  aes(
    x = predicho,
    y = residuo_log
  )
) +
  geom_point(
    alpha = 0.65
  ) +
  geom_hline(
    yintercept = 0,
    linetype = 2
  ) +
  scale_x_log10() +
  labs(
    title =
      "Residuos del modelo de costos",
    subtitle =
      "Residuo en escala logarítmica",
    x =
      "Costo predicho (escala log10)",
    y =
      "log(1 + observado) - log(1 + predicho)"
  ) +
  theme_minimal(
    base_size = 12
  )

ggsave(
  here(
    "figuras",
    "11_residuos_modelo_costo_k80.png"
  ),
  grafico_residuos,
  width = 8,
  height = 5.5,
  dpi = 300
)

grafico_distribucion <- ggplot(
  datos,
  aes(
    x = costo_total_positivo
  )
) +
  geom_histogram(
    bins = 35
  ) +
  scale_x_log10() +
  labs(
    title =
      "Distribución del costo acumulado en beneficiarios K80",
    subtitle =
      "Escala logarítmica",
    x =
      "Costo total positivo",
    y =
      "Número de beneficiarios"
  ) +
  theme_minimal(
    base_size = 12
  )

ggsave(
  here(
    "figuras",
    "12_distribucion_costos_k80.png"
  ),
  grafico_distribucion,
  width = 8,
  height = 5.5,
  dpi = 300
)


# 10. EXPORTACIÓN --------------------------------------------------------------

fwrite(
  metricas,
  here(
    "resultados",
    "40_metricas_modelos_costo_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  seleccion,
  here(
    "resultados",
    "41_seleccion_modelo_costo_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  predicciones[
    conjunto == "Prueba"
  ],
  here(
    "resultados",
    "42_predicciones_costo_prueba_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  coeficientes_gamma,
  here(
    "resultados",
    "43_coeficientes_gamma_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  coeficientes_lognormal,
  here(
    "resultados",
    "44_coeficientes_lognormal_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  perfiles_costo,
  here(
    "resultados",
    "45_perfiles_costo_observado_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  calibracion_resumen,
  here(
    "resultados",
    "46_calibracion_modelo_costo_k80.csv"
  ),
  bom = TRUE
)

saveRDS(
  modelo_gamma,
  here(
    "modelos",
    "modelo_gamma_costo_k80.rds"
  ),
  compress = "xz"
)

saveRDS(
  modelo_lognormal,
  here(
    "modelos",
    "modelo_lognormal_costo_k80.rds"
  ),
  compress = "xz"
)

resultados_costos <- list(
  unidad_analisis =
    "Beneficiario con registro K80",
  variable_respuesta =
    paste(
      "Costo total positivo acumulado durante",
      "el periodo observado"
    ),
  conteo_particiones =
    conteo_particiones,
  metricas =
    metricas,
  seleccion =
    seleccion,
  coeficientes_gamma =
    coeficientes_gamma,
  coeficientes_lognormal =
    coeficientes_lognormal,
  perfiles_costo =
    perfiles_costo,
  calibracion =
    calibracion_resumen,
  factor_duan =
    factor_duan,
  predictores =
    predictores,
  informacion_sesion =
    sessionInfo()
)

saveRDS(
  resultados_costos,
  here(
    "resultados",
    "resultados_costos_k80.rds"
  ),
  compress = "xz"
)


# 11. CIERRE ------------------------------------------------------------------

cat("\n============================================================\n")
cat("ETAPA 4 FINALIZADA CORRECTAMENTE\n")
cat("============================================================\n\n")

cat("Modelo seleccionado en validación:\n")
print(seleccion)

cat("\nMétricas finales en prueba:\n")
print(
  metricas[
    conjunto == "Prueba"
  ]
)

cat("\nArchivos principales:\n")
cat("- resultados/40_metricas_modelos_costo_k80.csv\n")
cat("- resultados/41_seleccion_modelo_costo_k80.csv\n")
cat("- resultados/45_perfiles_costo_observado_k80.csv\n")
cat("- resultados/resultados_costos_k80.rds\n")
cat("- figuras/10_observado_vs_predicho_costo_k80.png\n")
cat("- figuras/11_residuos_modelo_costo_k80.png\n")
cat("- figuras/12_distribucion_costos_k80.png\n")
cat("============================================================\n")

})
