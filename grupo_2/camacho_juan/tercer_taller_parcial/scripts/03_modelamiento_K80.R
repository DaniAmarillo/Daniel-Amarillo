# ==============================================================================
# TALLER 3 - MINERÍA DE DATOS
# ETAPA 3: MODELAMIENTO PREDICTIVO PARA COLELITIASIS (K80) - V2
# ==============================================================================
#
# Modelos:
#   1. Regresión logística regularizada (glmnet)
#   2. Random Forest probabilístico (ranger)
#
# Análisis:
#   A. Principal: todos los beneficiarios
#   B. Sensibilidad: solo beneficiarios con algún CID registrado
#
# Criterio principal de comparación:
#   PR-AUC, debido al fuerte desbalance de clases.
#
# El umbral de clasificación se selecciona exclusivamente en validación
# maximizando F1. La prueba se usa una sola vez para la evaluación final.
#
# IMPORTANTE:
#   - No vuelve a leer db_2026.csv.
#   - Lee la tabla analítica desde data/processed/taller3_k80.sqlite.
#   - No utiliza costos, códigos CID ni procedimientos biliares como predictores.
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
  "Matrix",
  "glmnet",
  "ranger",
  "pROC",
  "PRROC",
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
library(Matrix)
library(glmnet)
library(ranger)
library(pROC)
library(PRROC)
library(ggplot2)
library(here)

dir.create(here("resultados"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("modelos"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("figuras"), recursive = TRUE, showWarnings = FALSE)

ruta_sqlite <- here(
  "data",
  "processed",
  "taller3_k80.sqlite"
)

if (!file.exists(ruta_sqlite)) {
  stop(
    "No se encontró la base analítica:\n",
    ruta_sqlite,
    "\nEjecute y finalice primero la etapa 2."
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
dbExecute(con, "PRAGMA cache_size = -100000")
dbExecute(con, "PRAGMA busy_timeout = 60000")

if (!"variables_beneficiario_k80" %in% dbListTables(con)) {
  stop(
    "La tabla variables_beneficiario_k80 no existe en SQLite."
  )
}


# 1. VARIABLES DEL MODELO ------------------------------------------------------

# Variables generales de utilización y demografía.
# Se excluyen deliberadamente:
#   - target_k80
#   - tiene_cid_registrado
#   - n_registros_con_cid
#   - n_registros_k80
#   - costos
#   - internación, cirugía y procedimientos biliares
# porque pueden revelar diagnóstico/tratamiento posterior.

variables_consulta <- c(
  "chave_funcional",
  "target_k80",
  "tiene_cid_registrado",
  "particion",
  "edad",
  "sexo",
  "tipo_beneficiario",
  "estado_predominante",
  "unidad_predominante",
  "sexo_inconsistente",
  "tipo_beneficiario_inconsistente",
  "n_procedimientos",
  "n_consultas",
  "n_examenes",
  "n_terapias",
  "n_urgencias",
  "dias_observacion",
  "prop_consultas",
  "prop_examenes",
  "prop_terapias",
  "prop_urgencias",
  "n_unidad_faltante",
  "n_especialidad_faltante"
)

variables_categoricas <- c(
  "sexo",
  "tipo_beneficiario",
  "estado_predominante",
  "unidad_predominante"
)

variables_numericas_modelo <- c(
  "edad",
  "log_n_procedimientos",
  "log_n_consultas",
  "log_n_examenes",
  "log_n_terapias",
  "log_n_urgencias",
  "log_dias_observacion",
  "prop_consultas",
  "prop_examenes",
  "prop_terapias",
  "prop_urgencias",
  "prop_unidad_faltante",
  "prop_especialidad_faltante",
  "sexo_inconsistente",
  "tipo_beneficiario_inconsistente"
)

predictores_modelo <- c(
  variables_numericas_modelo,
  variables_categoricas
)

campos_sql <- paste(
  variables_consulta,
  collapse = ", "
)


# 2. FUNCIONES DE CARGA --------------------------------------------------------

leer_datos <- function(
    particion,
    solo_cid = FALSE,
    target = NULL,
    limite = NULL,
    ordenar_muestra = FALSE) {

  condiciones <- sprintf(
    "particion = '%s'",
    particion
  )

  if (solo_cid) {
    condiciones <- paste(
      condiciones,
      "tiene_cid_registrado = 1",
      sep = " AND "
    )
  }

  if (!is.null(target)) {
    condiciones <- paste(
      condiciones,
      sprintf("target_k80 = %d", target),
      sep = " AND "
    )
  }

  orden <- ""

  if (ordenar_muestra) {
    # Orden pseudoaleatorio determinista basado en rowid.
    orden <- paste0(
      " ORDER BY ABS((",
      "rowid * 1103515245 + 2016325",
      ") % 2147483647)"
    )
  }

  limite_sql <- ""

  if (!is.null(limite)) {
    limite_sql <- sprintf(
      " LIMIT %d",
      as.integer(limite)
    )
  }

  consulta <- sprintf(
    paste0(
      "SELECT %s ",
      "FROM variables_beneficiario_k80 ",
      "WHERE %s%s%s"
    ),
    campos_sql,
    condiciones,
    orden,
    limite_sql
  )

  as.data.table(
    dbGetQuery(
      con,
      consulta
    )
  )
}


cargar_analisis <- function(
    nombre,
    solo_cid,
    razon_negativos = 50L) {

  cat("\n------------------------------------------------------------\n")
  cat("Cargando análisis:", nombre, "\n")
  cat("------------------------------------------------------------\n")

  positivos_train <- leer_datos(
    particion = "entrenamiento",
    solo_cid = solo_cid,
    target = 1L
  )

  if (solo_cid) {

    negativos_train <- leer_datos(
      particion = "entrenamiento",
      solo_cid = TRUE,
      target = 0L
    )

  } else {

    n_negativos <- min(
      as.integer(
        razon_negativos * nrow(positivos_train)
      ),
      dbGetQuery(
        con,
        paste0(
          "SELECT COUNT(*) AS n ",
          "FROM variables_beneficiario_k80 ",
          "WHERE particion = 'entrenamiento' ",
          "AND target_k80 = 0"
        )
      )$n[[1]]
    )

    negativos_train <- leer_datos(
      particion = "entrenamiento",
      solo_cid = FALSE,
      target = 0L,
      limite = n_negativos,
      ordenar_muestra = TRUE
    )
  }

  entrenamiento <- rbindlist(
    list(
      positivos_train,
      negativos_train
    ),
    use.names = TRUE
  )

  set.seed(2016325)
  entrenamiento <- entrenamiento[
    sample(.N)
  ]

  validacion <- leer_datos(
    particion = "validacion",
    solo_cid = solo_cid
  )

  prueba <- leer_datos(
    particion = "prueba",
    solo_cid = solo_cid
  )

  cat(
    "Entrenamiento:",
    format(nrow(entrenamiento), big.mark = "."),
    "| positivos:",
    sum(entrenamiento$target_k80),
    "\n"
  )

  cat(
    "Validación:",
    format(nrow(validacion), big.mark = "."),
    "| positivos:",
    sum(validacion$target_k80),
    "\n"
  )

  cat(
    "Prueba:",
    format(nrow(prueba), big.mark = "."),
    "| positivos:",
    sum(prueba$target_k80),
    "\n"
  )

  list(
    entrenamiento = entrenamiento,
    validacion = validacion,
    prueba = prueba
  )
}


# 3. PREPROCESAMIENTO SIN FUGA -------------------------------------------------

construir_variables_derivadas <- function(dt) {

  salida <- copy(dt)

  columnas_conteo <- c(
    "n_procedimientos",
    "n_consultas",
    "n_examenes",
    "n_terapias",
    "n_urgencias",
    "dias_observacion",
    "n_unidad_faltante",
    "n_especialidad_faltante"
  )

  for (columna in columnas_conteo) {
    salida[
      is.na(get(columna)) |
        !is.finite(get(columna)) |
        get(columna) < 0,
      (columna) := 0
    ]
  }

  salida[, log_n_procedimientos :=
    log1p(n_procedimientos)
  ]

  salida[, log_n_consultas :=
    log1p(n_consultas)
  ]

  salida[, log_n_examenes :=
    log1p(n_examenes)
  ]

  salida[, log_n_terapias :=
    log1p(n_terapias)
  ]

  salida[, log_n_urgencias :=
    log1p(n_urgencias)
  ]

  salida[, log_dias_observacion :=
    log1p(dias_observacion)
  ]

  salida[, prop_unidad_faltante :=
    n_unidad_faltante /
      pmax(n_procedimientos, 1)
  ]

  salida[, prop_especialidad_faltante :=
    n_especialidad_faltante /
      pmax(n_procedimientos, 1)
  ]

  columnas_prop <- c(
    "prop_consultas",
    "prop_examenes",
    "prop_terapias",
    "prop_urgencias",
    "prop_unidad_faltante",
    "prop_especialidad_faltante"
  )

  for (columna in columnas_prop) {
    salida[
      is.na(get(columna)) |
        !is.finite(get(columna)),
      (columna) := 0
    ]

    salida[
      get(columna) < 0,
      (columna) := 0
    ]
  }

  salida
}


ajustar_preprocesador <- function(
    entrenamiento,
    frecuencia_minima = 20L) {

  entrenamiento <- construir_variables_derivadas(
    entrenamiento
  )

  medianas <- vapply(
    variables_numericas_modelo,
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
        median(valores, na.rm = TRUE)
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

      conservar <- names(
        conteo[
          conteo >= frecuencia_minima
        ]
      )

      unique(
        c(
          conservar,
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
    datos,
    preprocesador) {

  salida <- construir_variables_derivadas(
    datos
  )

  for (variable in variables_numericas_modelo) {

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

    niveles_validos <- preprocesador$niveles[[variable]]

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


crear_matriz <- function(
    datos,
    columnas_referencia = NULL) {

  formula_modelo <- as.formula(
    paste(
      "~",
      paste(
        predictores_modelo,
        collapse = " + "
      ),
      "- 1"
    )
  )

  matriz <- Matrix::sparse.model.matrix(
    formula_modelo,
    data = as.data.frame(
      datos[
        ,
        ..predictores_modelo
      ]
    )
  )

  if (is.null(columnas_referencia)) {
    return(matriz)
  }

  faltantes <- setdiff(
    columnas_referencia,
    colnames(matriz)
  )

  if (length(faltantes) > 0L) {

    ceros <- Matrix(
      0,
      nrow = nrow(matriz),
      ncol = length(faltantes),
      sparse = TRUE
    )

    colnames(ceros) <- faltantes

    matriz <- cbind(
      matriz,
      ceros
    )
  }

  sobrantes <- setdiff(
    colnames(matriz),
    columnas_referencia
  )

  if (length(sobrantes) > 0L) {
    matriz <- matriz[
      ,
      setdiff(
        colnames(matriz),
        sobrantes
      ),
      drop = FALSE
    ]
  }

  matriz[
    ,
    columnas_referencia,
    drop = FALSE
  ]
}


# 4. MÉTRICAS Y UMBRAL ---------------------------------------------------------

division_segura <- function(a, b) {
  ifelse(
    b == 0,
    NA_real_,
    a / b
  )
}


seleccionar_umbral_f1 <- function(
    y,
    probabilidad) {

  orden <- order(
    probabilidad,
    decreasing = TRUE
  )

  y_ord <- y[orden]
  p_ord <- probabilidad[orden]

  tp <- cumsum(y_ord == 1L)
  fp <- cumsum(y_ord == 0L)

  total_positivos <- sum(y == 1L)
  fn <- total_positivos - tp

  precision <- tp / pmax(tp + fp, 1)
  sensibilidad <- tp / pmax(tp + fn, 1)

  f1 <- 2 * precision * sensibilidad /
    pmax(
      precision + sensibilidad,
      .Machine$double.eps
    )

  indice <- which.max(f1)

  list(
    umbral = p_ord[[indice]],
    f1 = f1[[indice]],
    precision = precision[[indice]],
    sensibilidad = sensibilidad[[indice]]
  )
}


calcular_metricas <- function(
    y,
    probabilidad,
    umbral,
    analisis,
    modelo,
    conjunto) {

  prediccion <- as.integer(
    probabilidad >= umbral
  )

  tp <- sum(
    y == 1L &
      prediccion == 1L
  )

  tn <- sum(
    y == 0L &
      prediccion == 0L
  )

  fp <- sum(
    y == 0L &
      prediccion == 1L
  )

  fn <- sum(
    y == 1L &
      prediccion == 0L
  )

  sensibilidad <- division_segura(
    tp,
    tp + fn
  )

  especificidad <- division_segura(
    tn,
    tn + fp
  )

  precision <- division_segura(
    tp,
    tp + fp
  )

  f1 <- division_segura(
    2 * precision * sensibilidad,
    precision + sensibilidad
  )

  roc_obj <- pROC::roc(
    response = y,
    predictor = probabilidad,
    levels = c(0, 1),
    direction = "<",
    quiet = TRUE
  )

  pr_obj <- PRROC::pr.curve(
    scores.class0 = probabilidad[y == 1L],
    scores.class1 = probabilidad[y == 0L],
    curve = TRUE
  )

  data.table(
    analisis = analisis,
    modelo = modelo,
    conjunto = conjunto,
    n = length(y),
    positivos = sum(y),
    prevalencia = mean(y),
    umbral = umbral,
    accuracy = division_segura(
      tp + tn,
      length(y)
    ),
    sensibilidad = sensibilidad,
    especificidad = especificidad,
    precision = precision,
    f1 = f1,
    roc_auc = as.numeric(
      pROC::auc(roc_obj)
    ),
    pr_auc = pr_obj$auc.integral,
    tp = tp,
    tn = tn,
    fp = fp,
    fn = fn
  )
}


crear_curvas <- function(
    y,
    probabilidad,
    analisis,
    modelo) {

  roc_obj <- pROC::roc(
    response = y,
    predictor = probabilidad,
    levels = c(0, 1),
    direction = "<",
    quiet = TRUE
  )

  roc_coords <- as.data.table(
    pROC::coords(
      roc_obj,
      x = "all",
      ret = c(
        "specificity",
        "sensitivity"
      ),
      transpose = FALSE
    )
  )

  curva_roc <- data.table(
    analisis = analisis,
    modelo = modelo,
    tasa_falsos_positivos =
      1 - roc_coords$specificity,
    sensibilidad =
      roc_coords$sensitivity
  )

  pr_obj <- PRROC::pr.curve(
    scores.class0 = probabilidad[y == 1L],
    scores.class1 = probabilidad[y == 0L],
    curve = TRUE
  )

  curva_pr <- data.table(
    analisis = analisis,
    modelo = modelo,
    recall = pr_obj$curve[, 1],
    precision = pr_obj$curve[, 2]
  )

  list(
    roc = curva_roc,
    pr = curva_pr
  )
}


# 5. ENTRENAMIENTO DE UN ANÁLISIS ---------------------------------------------

ejecutar_analisis <- function(
    nombre,
    solo_cid,
    razon_negativos = 50L) {

  datos <- cargar_analisis(
    nombre = nombre,
    solo_cid = solo_cid,
    razon_negativos = razon_negativos
  )

  preprocesador <- ajustar_preprocesador(
    datos$entrenamiento
  )

  train <- aplicar_preprocesador(
    datos$entrenamiento,
    preprocesador
  )

  val <- aplicar_preprocesador(
    datos$validacion,
    preprocesador
  )

  test <- aplicar_preprocesador(
    datos$prueba,
    preprocesador
  )

  y_train <- as.integer(
    train$target_k80
  )

  y_val <- as.integer(
    val$target_k80
  )

  y_test <- as.integer(
    test$target_k80
  )

  # ---------------------------------------------------------------------------
  # 5.1 Regresión logística regularizada
  # ---------------------------------------------------------------------------

  cat("\nEntrenando glmnet para", nombre, "...\n")

  x_train <- crear_matriz(
    train
  )

  columnas_x <- colnames(
    x_train
  )

  x_val <- crear_matriz(
    val,
    columnas_referencia = columnas_x
  )

  x_test <- crear_matriz(
    test,
    columnas_referencia = columnas_x
  )

  peso_positivo <- sum(
    y_train == 0L
  ) / sum(
    y_train == 1L
  )

  pesos <- ifelse(
    y_train == 1L,
    peso_positivo,
    1
  )

  set.seed(2016325)

  foldid <- integer(
    length(y_train)
  )

  indices_pos <- which(
    y_train == 1L
  )

  indices_neg <- which(
    y_train == 0L
  )

  foldid[indices_pos] <- sample(
    rep(
      1:5,
      length.out = length(indices_pos)
    )
  )

  foldid[indices_neg] <- sample(
    rep(
      1:5,
      length.out = length(indices_neg)
    )
  )

  modelo_glmnet <- cv.glmnet(
    x = x_train,
    y = y_train,
    family = "binomial",
    alpha = 0.5,
    weights = pesos,
    foldid = foldid,
    type.measure = "auc",
    standardize = TRUE,
    parallel = FALSE
  )

  prob_val_glmnet <- as.numeric(
    predict(
      modelo_glmnet,
      newx = x_val,
      s = "lambda.1se",
      type = "response"
    )
  )

  umbral_glmnet <- seleccionar_umbral_f1(
    y_val,
    prob_val_glmnet
  )

  prob_test_glmnet <- as.numeric(
    predict(
      modelo_glmnet,
      newx = x_test,
      s = "lambda.1se",
      type = "response"
    )
  )

  # ---------------------------------------------------------------------------
  # 5.2 Random Forest
  # ---------------------------------------------------------------------------

  cat("Entrenando Random Forest para", nombre, "...\n")

  datos_rf_train <- as.data.frame(
    train[
      ,
      c(
        "target_k80",
        predictores_modelo
      ),
      with = FALSE
    ]
  )

  datos_rf_train$target_k80 <- factor(
    datos_rf_train$target_k80,
    levels = c(0, 1)
  )

  datos_rf_val <- as.data.frame(
    val[
      ,
      ..predictores_modelo
    ]
  )

  datos_rf_test <- as.data.frame(
    test[
      ,
      ..predictores_modelo
    ]
  )

  set.seed(2016325)

  modelo_rf <- ranger(
    formula = target_k80 ~ .,
    data = datos_rf_train,
    probability = TRUE,
    num.trees = 500,
    mtry = max(
      2L,
      floor(
        sqrt(
          length(predictores_modelo)
        )
      )
    ),
    min.node.size = 10,
    sample.fraction = 0.80,
    replace = TRUE,
    class.weights = c(
      "0" = 1,
      "1" = peso_positivo
    ),
    importance = "permutation",
    respect.unordered.factors = "order",
    num.threads = max(
      1L,
      min(
        2L,
        parallel::detectCores(
          logical = FALSE
        )
      )
    ),
    seed = 2016325
  )

  prob_val_rf <- predict(
    modelo_rf,
    data = datos_rf_val
  )$predictions[, "1"]

  umbral_rf <- seleccionar_umbral_f1(
    y_val,
    prob_val_rf
  )

  prob_test_rf <- predict(
    modelo_rf,
    data = datos_rf_test
  )$predictions[, "1"]

  # ---------------------------------------------------------------------------
  # 5.3 Métricas
  # ---------------------------------------------------------------------------

  metricas <- rbindlist(
    list(
      calcular_metricas(
        y_val,
        prob_val_glmnet,
        umbral_glmnet$umbral,
        nombre,
        "Regresión logística regularizada",
        "Validación"
      ),
      calcular_metricas(
        y_test,
        prob_test_glmnet,
        umbral_glmnet$umbral,
        nombre,
        "Regresión logística regularizada",
        "Prueba"
      ),
      calcular_metricas(
        y_val,
        prob_val_rf,
        umbral_rf$umbral,
        nombre,
        "Random Forest",
        "Validación"
      ),
      calcular_metricas(
        y_test,
        prob_test_rf,
        umbral_rf$umbral,
        nombre,
        "Random Forest",
        "Prueba"
      )
    ),
    use.names = TRUE
  )

  umbrales <- data.table(
    analisis = nombre,
    modelo = c(
      "Regresión logística regularizada",
      "Random Forest"
    ),
    umbral = c(
      umbral_glmnet$umbral,
      umbral_rf$umbral
    ),
    f1_validacion = c(
      umbral_glmnet$f1,
      umbral_rf$f1
    ),
    precision_validacion = c(
      umbral_glmnet$precision,
      umbral_rf$precision
    ),
    sensibilidad_validacion = c(
      umbral_glmnet$sensibilidad,
      umbral_rf$sensibilidad
    )
  )

  # ---------------------------------------------------------------------------
  # 5.4 Importancias
  # ---------------------------------------------------------------------------

  coeficientes <- as.matrix(
    coef(
      modelo_glmnet,
      s = "lambda.1se"
    )
  )

  importancia_glmnet <- data.table(
    variable = rownames(
      coeficientes
    ),
    coeficiente = as.numeric(
      coeficientes[, 1]
    )
  )[
    variable != "(Intercept)"
  ]

  importancia_glmnet[
    ,
    importancia_absoluta :=
      abs(coeficiente)
  ]

  setorder(
    importancia_glmnet,
    -importancia_absoluta
  )

  importancia_glmnet[
    ,
    `:=`(
      analisis = nombre,
      modelo =
        "Regresión logística regularizada"
    )
  ]

  importancia_rf <- data.table(
    variable = names(
      modelo_rf$variable.importance
    ),
    importancia = as.numeric(
      modelo_rf$variable.importance
    )
  )

  setorder(
    importancia_rf,
    -importancia
  )

  importancia_rf[
    ,
    `:=`(
      analisis = nombre,
      modelo = "Random Forest"
    )
  ]

  # ---------------------------------------------------------------------------
  # 5.5 Curvas y errores
  # ---------------------------------------------------------------------------

  curvas_glmnet <- crear_curvas(
    y_test,
    prob_test_glmnet,
    nombre,
    "Regresión logística regularizada"
  )

  curvas_rf <- crear_curvas(
    y_test,
    prob_test_rf,
    nombre,
    "Random Forest"
  )

  predicciones_prueba <- rbindlist(
    list(
      data.table(
        chave_funcional =
          test$chave_funcional,
        target_k80 =
          y_test,
        analisis =
          nombre,
        modelo =
          "Regresión logística regularizada",
        probabilidad =
          prob_test_glmnet,
        umbral =
          umbral_glmnet$umbral,
        prediccion =
          as.integer(
            prob_test_glmnet >=
              umbral_glmnet$umbral
          )
      ),
      data.table(
        chave_funcional =
          test$chave_funcional,
        target_k80 =
          y_test,
        analisis =
          nombre,
        modelo =
          "Random Forest",
        probabilidad =
          prob_test_rf,
        umbral =
          umbral_rf$umbral,
        prediccion =
          as.integer(
            prob_test_rf >=
              umbral_rf$umbral
          )
      )
    )
  )

  errores <- predicciones_prueba[
    target_k80 != prediccion
  ]

  errores[
    ,
    tipo_error := fifelse(
      target_k80 == 1L,
      "Falso negativo",
      "Falso positivo"
    )
  ]

  # Liberación explícita de matrices grandes.
  rm(
    x_train,
    x_val,
    x_test,
    datos_rf_train,
    datos_rf_val,
    datos_rf_test
  )

  invisible(gc(full = TRUE))

  list(
    metricas = metricas,
    umbrales = umbrales,
    importancia_glmnet = importancia_glmnet,
    importancia_rf = importancia_rf,
    curva_roc = rbindlist(
      list(
        curvas_glmnet$roc,
        curvas_rf$roc
      )
    ),
    curva_pr = rbindlist(
      list(
        curvas_glmnet$pr,
        curvas_rf$pr
      )
    ),
    predicciones_prueba =
      predicciones_prueba,
    errores = errores,
    modelos = list(
      glmnet = modelo_glmnet,
      random_forest = modelo_rf,
      preprocesador = preprocesador,
      columnas_matriz = columnas_x
    ),
    tamanos = data.table(
      analisis = nombre,
      conjunto = c(
        "Entrenamiento usado",
        "Validación",
        "Prueba"
      ),
      n = c(
        nrow(train),
        nrow(val),
        nrow(test)
      ),
      positivos = c(
        sum(y_train),
        sum(y_val),
        sum(y_test)
      )
    )
  )
}


# 6. EJECUCIÓN ----------------------------------------------------------------

cat("\n============================================================\n")
cat("ETAPA 3: MODELAMIENTO PREDICTIVO K80\n")
cat("============================================================\n")

resultado_principal <- ejecutar_analisis(
  nombre = "Principal",
  solo_cid = FALSE,
  razon_negativos = 50L
)

resultado_sensibilidad <- ejecutar_analisis(
  nombre = "Sensibilidad CID observado",
  solo_cid = TRUE,
  razon_negativos = 50L
)


# 7. CONSOLIDACIÓN -------------------------------------------------------------

metricas <- rbindlist(
  list(
    resultado_principal$metricas,
    resultado_sensibilidad$metricas
  ),
  use.names = TRUE
)

umbrales <- rbindlist(
  list(
    resultado_principal$umbrales,
    resultado_sensibilidad$umbrales
  ),
  use.names = TRUE
)

importancia_glmnet <- rbindlist(
  list(
    resultado_principal$importancia_glmnet,
    resultado_sensibilidad$importancia_glmnet
  ),
  fill = TRUE
)

importancia_rf <- rbindlist(
  list(
    resultado_principal$importancia_rf,
    resultado_sensibilidad$importancia_rf
  ),
  fill = TRUE
)

curvas_roc <- rbindlist(
  list(
    resultado_principal$curva_roc,
    resultado_sensibilidad$curva_roc
  )
)

curvas_pr <- rbindlist(
  list(
    resultado_principal$curva_pr,
    resultado_sensibilidad$curva_pr
  )
)

predicciones_prueba <- rbindlist(
  list(
    resultado_principal$predicciones_prueba,
    resultado_sensibilidad$predicciones_prueba
  )
)

errores_prueba <- rbindlist(
  list(
    resultado_principal$errores,
    resultado_sensibilidad$errores
  )
)

tamanos_muestras <- rbindlist(
  list(
    resultado_principal$tamanos,
    resultado_sensibilidad$tamanos
  )
)

# Selección por PR-AUC en validación dentro de cada análisis.
seleccion_modelo <- metricas[
  conjunto == "Validación"
][
  order(
    analisis,
    -pr_auc,
    -f1
  )
][
  ,
  .SD[1],
  by = analisis
]


# 8. FIGURAS ------------------------------------------------------------------

grafico_roc <- ggplot(
  curvas_roc,
  aes(
    x = tasa_falsos_positivos,
    y = sensibilidad,
    linetype = modelo
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = 3
  ) +
  facet_wrap(
    ~ analisis
  ) +
  coord_equal() +
  labs(
    title = "Curvas ROC en el conjunto de prueba",
    x = "Tasa de falsos positivos",
    y = "Sensibilidad",
    linetype = "Modelo"
  ) +
  theme_minimal(
    base_size = 12
  )

ggsave(
  here(
    "figuras",
    "07_curvas_roc_modelos_k80.png"
  ),
  grafico_roc,
  width = 10,
  height = 5.5,
  dpi = 300
)

grafico_pr <- ggplot(
  curvas_pr,
  aes(
    x = recall,
    y = precision,
    linetype = modelo
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  facet_wrap(
    ~ analisis,
    scales = "free_y"
  ) +
  labs(
    title = "Curvas precisión-recall en el conjunto de prueba",
    x = "Sensibilidad (recall)",
    y = "Precisión",
    linetype = "Modelo"
  ) +
  theme_minimal(
    base_size = 12
  )

ggsave(
  here(
    "figuras",
    "08_curvas_pr_modelos_k80.png"
  ),
  grafico_pr,
  width = 10,
  height = 5.5,
  dpi = 300
)

metricas_grafico <- melt(
  metricas[
    conjunto == "Prueba"
  ],
  id.vars = c(
    "analisis",
    "modelo"
  ),
  measure.vars = c(
    "pr_auc",
    "roc_auc",
    "f1",
    "sensibilidad",
    "especificidad"
  ),
  variable.name = "metrica",
  value.name = "valor"
)

grafico_metricas <- ggplot(
  metricas_grafico,
  aes(
    x = modelo,
    y = valor,
    fill = metrica
  )
) +
  geom_col(
    position = "dodge"
  ) +
  facet_wrap(
    ~ analisis
  ) +
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  labs(
    title = "Desempeño final de los clasificadores",
    x = NULL,
    y = "Valor",
    fill = "Métrica"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    axis.text.x = element_text(
      angle = 15,
      hjust = 1
    )
  )

ggsave(
  here(
    "figuras",
    "09_metricas_modelos_k80.png"
  ),
  grafico_metricas,
  width = 11,
  height = 6,
  dpi = 300
)


# 9. EXPORTACIÓN ---------------------------------------------------------------

fwrite(
  metricas,
  here(
    "resultados",
    "30_metricas_modelos_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  umbrales,
  here(
    "resultados",
    "31_umbrales_modelos_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  importancia_glmnet,
  here(
    "resultados",
    "32_importancia_glmnet_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  importancia_rf,
  here(
    "resultados",
    "33_importancia_random_forest_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  seleccion_modelo,
  here(
    "resultados",
    "34_seleccion_modelo_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  errores_prueba,
  here(
    "resultados",
    "35_errores_prueba_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  tamanos_muestras,
  here(
    "resultados",
    "36_tamanos_muestras_modelos_k80.csv"
  ),
  bom = TRUE
)

# Las predicciones completas se guardan en RDS para no inflar el repositorio.
saveRDS(
  predicciones_prueba,
  here(
    "resultados",
    "predicciones_prueba_k80.rds"
  ),
  compress = "xz"
)

saveRDS(
  resultado_principal$modelos$glmnet,
  here(
    "modelos",
    "modelo_glmnet_k80_principal.rds"
  ),
  compress = "xz"
)

saveRDS(
  resultado_principal$modelos$random_forest,
  here(
    "modelos",
    "modelo_rf_k80_principal.rds"
  ),
  compress = "xz"
)

saveRDS(
  resultado_sensibilidad$modelos$glmnet,
  here(
    "modelos",
    "modelo_glmnet_k80_sensibilidad.rds"
  ),
  compress = "xz"
)

saveRDS(
  resultado_sensibilidad$modelos$random_forest,
  here(
    "modelos",
    "modelo_rf_k80_sensibilidad.rds"
  ),
  compress = "xz"
)

objetos_modelamiento <- list(
  metricas = metricas,
  umbrales = umbrales,
  seleccion_modelo = seleccion_modelo,
  importancia_glmnet = importancia_glmnet,
  importancia_rf = importancia_rf,
  curvas_roc = curvas_roc,
  curvas_pr = curvas_pr,
  tamanos_muestras = tamanos_muestras,
  predictores = predictores_modelo,
  nota_metodologica = paste(
    "El modelo principal utiliza todos los beneficiarios.",
    "El análisis de sensibilidad se restringe a beneficiarios",
    "con al menos un CID registrado.",
    "El entrenamiento principal usa todos los positivos y una",
    "muestra determinista de 50 negativos por positivo.",
    "Validación y prueba conservan la prevalencia natural."
  ),
  informacion_sesion = sessionInfo()
)

saveRDS(
  objetos_modelamiento,
  here(
    "resultados",
    "resultados_modelamiento_k80.rds"
  ),
  compress = "xz"
)


# 10. RESULTADO EN CONSOLA -----------------------------------------------------

cat("\n============================================================\n")
cat("ETAPA 3 FINALIZADA CORRECTAMENTE\n")
cat("============================================================\n\n")

cat("Modelos seleccionados por PR-AUC en validación:\n")
print(
  seleccion_modelo[
    ,
    .(
      analisis,
      modelo,
      pr_auc,
      roc_auc,
      f1,
      sensibilidad,
      especificidad,
      umbral
    )
  ]
)

cat("\nMétricas finales en prueba:\n")
print(
  metricas[
    conjunto == "Prueba",
    .(
      analisis,
      modelo,
      pr_auc,
      roc_auc,
      precision,
      f1,
      sensibilidad,
      especificidad,
      tp,
      fp,
      fn
    )
  ]
)

cat("\nArchivos principales generados:\n")
cat("- resultados/30_metricas_modelos_k80.csv\n")
cat("- resultados/34_seleccion_modelo_k80.csv\n")
cat("- resultados/35_errores_prueba_k80.csv\n")
cat("- resultados/resultados_modelamiento_k80.rds\n")
cat("- figuras/07_curvas_roc_modelos_k80.png\n")
cat("- figuras/08_curvas_pr_modelos_k80.png\n")
cat("- figuras/09_metricas_modelos_k80.png\n")
cat("============================================================\n")

})
