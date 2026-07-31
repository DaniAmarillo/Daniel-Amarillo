# Análisis de componentes para LSA
# Taller 4 - Minería de Datos
# Paso 5: reducción dimensional mediante Truncated SVD

library(dplyr)
library(Matrix)
library(irlba)

# 1. Ruta del modelo TF-IDF

ruta_modelo <- file.path(
  "objetos_busqueda",
  "modelo_tfidf_completo.rds"
)

carpeta_objetos <- "objetos_busqueda"

if (!file.exists(ruta_modelo)) {
  stop(
    paste0(
      "No se encontró el archivo ",
      ruta_modelo,
      ". Ejecute primero construir_tfidf.R."
    )
  )
}

if (!dir.exists(carpeta_objetos)) {
  dir.create(carpeta_objetos)
}

# 2. Carga de la matriz TF-IDF

objeto_tfidf <- readRDS(
  ruta_modelo
)

if (!"matriz_tfidf" %in% names(objeto_tfidf)) {
  stop(
    "El objeto TF-IDF no contiene la matriz necesaria."
  )
}

matriz_tfidf <- objeto_tfidf$matriz_tfidf
corpus_articulos <- objeto_tfidf$corpus

cat("\n1. Matriz TF-IDF cargada correctamente\n")
cat("Documentos:", nrow(matriz_tfidf), "\n")
cat("Términos:", ncol(matriz_tfidf), "\n")
cat("Formato:", class(matriz_tfidf)[1], "\n")

# 3. Validación de la dimensión máxima

rango_maximo <- min(
  nrow(matriz_tfidf),
  ncol(matriz_tfidf)
) - 1

componentes_maximos <- min(
  200,
  rango_maximo
)

if (componentes_maximos < 2) {
  stop(
    "La matriz no tiene suficiente rango para aplicar Truncated SVD."
  )
}

cat("\n2. Configuración del análisis\n")
cat(
  "Máximo de componentes evaluados:",
  componentes_maximos,
  "\n"
)

# 4. Energía total de la matriz original

energia_total <- sum(
  matriz_tfidf ^ 2
)

if (
  is.na(energia_total) ||
  energia_total <= 0
) {
  stop(
    "No fue posible calcular la energía total de la matriz TF-IDF."
  )
}

cat("\n3. Energía total de la matriz\n")
cat("Energía:", round(energia_total, 6), "\n")

# 5. Truncated SVD

cat("\n4. Calculando Truncated SVD\n")

tiempo_svd <- system.time(
  modelo_svd_amplio <- irlba::irlba(
    A = matriz_tfidf,
    nv = componentes_maximos,
    nu = componentes_maximos,
    maxit = 1000,
    tol = 1e-5
  )
)

cat("Truncated SVD calculado correctamente\n")
cat(
  "Tiempo total:",
  round(tiempo_svd[["elapsed"]], 4),
  "segundos\n"
)

# 6. Información conservada por componente

valores_singulares <- modelo_svd_amplio$d

energia_componentes <- valores_singulares ^ 2

proporcion_individual <- energia_componentes /
  energia_total

proporcion_acumulada <- cumsum(
  proporcion_individual
)

tabla_componentes <- data.frame(
  componente = seq_along(
    valores_singulares
  ),
  valor_singular = valores_singulares,
  energia = energia_componentes,
  proporcion_individual = proporcion_individual,
  proporcion_acumulada = proporcion_acumulada,
  porcentaje_acumulado = proporcion_acumulada * 100,
  stringsAsFactors = FALSE
)

cat("\n5. Primeros componentes\n")

print(
  tabla_componentes %>%
    slice_head(n = 20) %>%
    mutate(
      valor_singular = round(
        valor_singular,
        6
      ),
      proporcion_individual = round(
        proporcion_individual,
        6
      ),
      porcentaje_acumulado = round(
        porcentaje_acumulado,
        2
      )
    )
)

# 7. Comparación de dimensiones candidatas

componentes_candidatos <- c(
  25,
  50,
  75,
  100,
  125,
  150,
  175,
  200
)

componentes_candidatos <- componentes_candidatos[
  componentes_candidatos <= componentes_maximos
]

comparacion_componentes <- data.frame(
  componentes = componentes_candidatos,
  porcentaje_informacion = vapply(
    componentes_candidatos,
    function(k) {
      tabla_componentes$porcentaje_acumulado[k]
    },
    numeric(1)
  ),
  stringsAsFactors = FALSE
)

# 8. Memoria estimada de las representaciones reducidas

comparacion_componentes <- comparacion_componentes %>%
  mutate(
    celdas_representacion = nrow(
      matriz_tfidf
    ) * componentes,
    memoria_estimada_mb = round(
      celdas_representacion * 8 /
        1024 ^ 2,
      4
    ),
    reduccion_columnas_porcentaje = round(
      (
        1 -
          componentes /
          ncol(matriz_tfidf)
      ) * 100,
      2
    )
  )

cat("\n6. Comparación de dimensiones candidatas\n")

print(
  comparacion_componentes %>%
    mutate(
      porcentaje_informacion = round(
        porcentaje_informacion,
        2
      )
    )
)

# 9. Búsqueda de puntos de referencia

buscar_componentes_objetivo <- function(
    objetivo
) {
  posicion <- which(
    proporcion_acumulada >= objetivo
  )[1]
  
  if (is.na(posicion)) {
    return(NA_integer_)
  }
  
  posicion
}

componentes_50 <- buscar_componentes_objetivo(
  0.50
)

componentes_60 <- buscar_componentes_objetivo(
  0.60
)

componentes_70 <- buscar_componentes_objetivo(
  0.70
)

componentes_80 <- buscar_componentes_objetivo(
  0.80
)

objetivos_informacion <- data.frame(
  objetivo = c(
    "50 %",
    "60 %",
    "70 %",
    "80 %"
  ),
  componentes_necesarios = c(
    componentes_50,
    componentes_60,
    componentes_70,
    componentes_80
  ),
  stringsAsFactors = FALSE
)

cat("\n7. Componentes necesarios por objetivo\n")
print(objetivos_informacion)

# 10. Cambio marginal entre componentes

ganancia_por_bloque <- comparacion_componentes %>%
  arrange(componentes) %>%
  mutate(
    ganancia_informacion = c(
      porcentaje_informacion[1],
      diff(porcentaje_informacion)
    ),
    componentes_agregados = c(
      componentes[1],
      diff(componentes)
    ),
    ganancia_por_componente = ganancia_informacion /
      componentes_agregados
  )

cat("\n8. Ganancia de información por bloque\n")

print(
  ganancia_por_bloque %>%
    mutate(
      porcentaje_informacion = round(
        porcentaje_informacion,
        2
      ),
      ganancia_informacion = round(
        ganancia_informacion,
        2
      ),
      ganancia_por_componente = round(
        ganancia_por_componente,
        4
      )
    )
)

# 11. Dimensión provisional

dimension_provisional <- componentes_candidatos[
  which.min(
    abs(
      comparacion_componentes$porcentaje_informacion -
        60
    )
  )
]

if (
  length(dimension_provisional) == 0 ||
  is.na(dimension_provisional)
) {
  dimension_provisional <- min(
    100,
    componentes_maximos
  )
}

cat("\n9. Dimensión provisional sugerida\n")
cat(
  "Componentes:",
  dimension_provisional,
  "\n"
)

porcentaje_provisional <-
  comparacion_componentes$porcentaje_informacion[
    comparacion_componentes$componentes ==
      dimension_provisional
  ]

cat(
  "Información conservada:",
  round(
    porcentaje_provisional,
    2
  ),
  "%\n"
)

cat(
  "Esta dimensión todavía debe validarse mediante ",
  "la calidad de los rankings y Precision@5.\n"
)

# 12. Resumen de reducción dimensional

resumen_reduccion <- data.frame(
  indicador = c(
    "Método",
    "Dimensión original",
    "Documentos",
    "Términos originales",
    "Máximo de componentes evaluados",
    "Dimensión provisional",
    "Información conservada provisional",
    "Tiempo de ajuste en segundos",
    "Tipo de matriz original"
  ),
  valor = c(
    "Truncated SVD para LSA",
    paste(
      nrow(matriz_tfidf),
      "x",
      ncol(matriz_tfidf)
    ),
    as.character(
      nrow(matriz_tfidf)
    ),
    as.character(
      ncol(matriz_tfidf)
    ),
    as.character(
      componentes_maximos
    ),
    as.character(
      dimension_provisional
    ),
    paste0(
      round(
        porcentaje_provisional,
        2
      ),
      "%"
    ),
    as.character(
      round(
        tiempo_svd[["elapsed"]],
        4
      )
    ),
    class(matriz_tfidf)[1]
  ),
  stringsAsFactors = FALSE
)

cat("\n10. Resumen del análisis LSA\n")
print(resumen_reduccion)

# 13. Guardado del modelo amplio

objeto_analisis_lsa <- list(
  modelo_svd = modelo_svd_amplio,
  tabla_componentes = tabla_componentes,
  comparacion_componentes = comparacion_componentes,
  objetivos_informacion = objetivos_informacion,
  ganancia_por_bloque = ganancia_por_bloque,
  dimension_provisional = dimension_provisional,
  energia_total = energia_total,
  tiempo_svd = tiempo_svd,
  dimension_original = dim(
    matriz_tfidf
  )
)

saveRDS(
  objeto_analisis_lsa,
  file.path(
    carpeta_objetos,
    "analisis_componentes_lsa.rds"
  )
)

write.csv(
  tabla_componentes,
  "informacion_componentes_lsa.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  comparacion_componentes,
  "comparacion_componentes_lsa.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  objetivos_informacion,
  "objetivos_informacion_lsa.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  ganancia_por_bloque,
  "ganancia_componentes_lsa.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  resumen_reduccion,
  "resumen_analisis_lsa.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\n11. Análisis de componentes finalizado correctamente\n")
cat("Archivos generados:\n")
cat("1. objetos_busqueda/analisis_componentes_lsa.rds\n")
cat("2. informacion_componentes_lsa.csv\n")
cat("3. comparacion_componentes_lsa.csv\n")
cat("4. objetivos_informacion_lsa.csv\n")
cat("5. ganancia_componentes_lsa.csv\n")
cat("6. resumen_analisis_lsa.csv\n")