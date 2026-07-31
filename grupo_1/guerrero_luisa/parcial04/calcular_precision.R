# Cálculo de Precision@5

archivo_evaluacion <- "plantilla_evaluacion_manual.csv"

if (!file.exists(archivo_evaluacion)) {
  stop(
    paste0(
      "No se encontró el archivo ",
      archivo_evaluacion,
      ". Verifica que esté dentro de la carpeta taller_4."
    )
  )
}

# Cargar la evaluación manual

evaluacion <- read.csv(
  archivo_evaluacion,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8-BOM",
  check.names = FALSE
)

cat("\nArchivo cargado correctamente.\n")
cat("Número de filas:", nrow(evaluacion), "\n")
cat("Número de columnas:", ncol(evaluacion), "\n")

# Validar las columnas necesarias

columnas_necesarias <- c(
  "consulta",
  "estrategia",
  "posicion",
  "paper_id",
  "title",
  "topic_label",
  "puntaje",
  "relevante",
  "observacion"
)

columnas_faltantes <- setdiff(
  columnas_necesarias,
  names(evaluacion)
)

if (length(columnas_faltantes) > 0) {
  stop(
    paste(
      "Faltan las siguientes columnas:",
      paste(columnas_faltantes, collapse = ", ")
    )
  )
}

# Limpiar las valoraciones

evaluacion$relevante <- trimws(
  tolower(evaluacion$relevante)
)

evaluacion$es_relevante <- ifelse(
  evaluacion$relevante %in% c("si", "sí"),
  1,
  ifelse(
    evaluacion$relevante == "no",
    0,
    NA
  )
)

if (any(is.na(evaluacion$es_relevante))) {
  filas_invalidas <- evaluacion[
    is.na(evaluacion$es_relevante),
    c(
      "consulta",
      "estrategia",
      "posicion",
      "title",
      "relevante"
    )
  ]
  
  print(filas_invalidas)
  
  stop(
    "Hay filas sin una valoración válida. Usa únicamente Si o No."
  )
}

cat("\nDistribución de las valoraciones:\n")

print(
  table(
    evaluacion$estrategia,
    evaluacion$es_relevante
  )
)

# Verificar que haya cinco resultados por consulta y estrategia

conteo_resultados <- aggregate(
  posicion ~ consulta + estrategia,
  data = evaluacion,
  FUN = length
)

names(conteo_resultados)[3] <- "cantidad_resultados"

cat("\nCantidad de resultados por consulta y estrategia:\n")
print(conteo_resultados)

if (any(conteo_resultados$cantidad_resultados != 5)) {
  stop(
    paste0(
      "Cada consulta y estrategia debe contener exactamente ",
      "cinco resultados para calcular Precision@5."
    )
  )
}

# Calcular Precision@5

precision_5 <- aggregate(
  es_relevante ~ consulta + estrategia,
  data = evaluacion,
  FUN = function(x) sum(x) / 5
)

names(precision_5)[3] <- "precision_5"

cantidad_relevantes <- aggregate(
  es_relevante ~ consulta + estrategia,
  data = evaluacion,
  FUN = sum
)

names(cantidad_relevantes)[3] <- "relevantes_top5"

precision_5 <- merge(
  precision_5,
  cantidad_relevantes,
  by = c(
    "consulta",
    "estrategia"
  )
)

precision_5 <- precision_5[
  order(
    precision_5$consulta,
    precision_5$estrategia
  ),
]

cat("\nPrecision@5 por consulta y estrategia:\n")
print(precision_5)

# Calcular el promedio global por estrategia

resumen_global <- aggregate(
  precision_5 ~ estrategia,
  data = precision_5,
  FUN = mean
)

names(resumen_global)[2] <- "precision_5_promedio"

cat("\nPromedio global por estrategia:\n")
print(resumen_global)

# Comparar TF-IDF y LSA

consultas <- unique(precision_5$consulta)

comparacion <- data.frame(
  consulta = consultas,
  stringsAsFactors = FALSE
)

for (estrategia_actual in unique(precision_5$estrategia)) {
  datos_estrategia <- precision_5[
    precision_5$estrategia == estrategia_actual,
    c(
      "consulta",
      "precision_5"
    )
  ]
  
  names(datos_estrategia)[2] <- estrategia_actual
  
  comparacion <- merge(
    comparacion,
    datos_estrategia,
    by = "consulta",
    all.x = TRUE
  )
}

if (
  "TF-IDF" %in% names(comparacion) &&
  "LSA" %in% names(comparacion)
) {
  comparacion$diferencia_LSA_TFIDF <-
    comparacion$LSA - comparacion$`TF-IDF`
  
  comparacion$mejor_estrategia <- ifelse(
    comparacion$LSA > comparacion$`TF-IDF`,
    "LSA",
    ifelse(
      comparacion$`TF-IDF` > comparacion$LSA,
      "TF-IDF",
      "Empate"
    )
  )
}

cat("\nComparación entre TF-IDF y LSA:\n")
print(comparacion)

# Identificar resultados no relevantes

resultados_no_relevantes <- evaluacion[
  evaluacion$es_relevante == 0,
  c(
    "consulta",
    "estrategia",
    "posicion",
    "paper_id",
    "title",
    "topic_label",
    "puntaje",
    "observacion"
  )
]

cat("\nResultados marcados como no relevantes:\n")
print(resultados_no_relevantes)

# Calcular la posición del primer resultado relevante

grupos <- split(
  evaluacion,
  interaction(
    evaluacion$consulta,
    evaluacion$estrategia,
    drop = TRUE
  )
)

primer_resultado <- lapply(
  grupos,
  function(datos) {
    posiciones_relevantes <- datos$posicion[
      datos$es_relevante == 1
    ]
    
    primera_posicion <- if (
      length(posiciones_relevantes) > 0
    ) {
      min(posiciones_relevantes)
    } else {
      NA
    }
    
    data.frame(
      consulta = datos$consulta[1],
      estrategia = datos$estrategia[1],
      primera_posicion_relevante = primera_posicion,
      stringsAsFactors = FALSE
    )
  }
)

primer_resultado_relevante <- do.call(
  rbind,
  primer_resultado
)

rownames(primer_resultado_relevante) <- NULL

# Calcular Reciprocal Rank y MRR

primer_resultado_relevante$reciprocal_rank <- ifelse(
  is.na(
    primer_resultado_relevante$primera_posicion_relevante
  ),
  0,
  1 /
    primer_resultado_relevante$primera_posicion_relevante
)

mrr_global <- aggregate(
  reciprocal_rank ~ estrategia,
  data = primer_resultado_relevante,
  FUN = mean
)

names(mrr_global)[2] <- "MRR"

cat("\nMean Reciprocal Rank por estrategia:\n")
print(mrr_global)

# Exportar los resultados

write.csv(
  precision_5,
  "precision_5_por_consulta.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  resumen_global,
  "resumen_global_precision.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  comparacion,
  "comparacion_precision_5.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  resultados_no_relevantes,
  "resultados_no_relevantes.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  primer_resultado_relevante,
  "primer_resultado_relevante.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  mrr_global,
  "mrr_global.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\nProceso terminado correctamente.\n")
cat("\nArchivos generados:\n")
cat("- precision_5_por_consulta.csv\n")
cat("- resumen_global_precision.csv\n")
cat("- comparacion_precision_5.csv\n")
cat("- resultados_no_relevantes.csv\n")
cat("- primer_resultado_relevante.csv\n")
cat("- mrr_global.csv\n")