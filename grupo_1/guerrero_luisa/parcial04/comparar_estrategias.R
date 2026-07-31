# Comparación de estrategias de recuperación
# Taller 4 - Minería de Datos
# Paso 7: comparación de rankings TF-IDF y LSA

library(dplyr)
library(stringr)
library(tidyr)

# 1. Rutas de los archivos de resultados

ruta_tfidf <- "resultados_prueba_tfidf.csv"
ruta_lsa <- "resultados_prueba_lsa.csv"

if (!file.exists(ruta_tfidf)) {
  stop(
    paste0(
      "No se encontró ",
      ruta_tfidf,
      ". Ejecute primero buscar_tfidf.R."
    )
  )
}

if (!file.exists(ruta_lsa)) {
  stop(
    paste0(
      "No se encontró ",
      ruta_lsa,
      ". Ejecute primero buscar_lsa.R."
    )
  )
}

# 2. Carga de resultados

resultados_tfidf <- read.csv(
  ruta_tfidf,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

resultados_lsa <- read.csv(
  ruta_lsa,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

cat("\n1. Resultados cargados correctamente\n")
cat("Filas TF-IDF:", nrow(resultados_tfidf), "\n")
cat("Filas LSA:", nrow(resultados_lsa), "\n")

# 3. Validación de columnas

columnas_necesarias <- c(
  "consulta",
  "posicion",
  "paper_id",
  "title",
  "topic_label",
  "puntaje"
)

faltantes_tfidf <- base::setdiff(
  columnas_necesarias,
  names(resultados_tfidf)
)

faltantes_lsa <- base::setdiff(
  columnas_necesarias,
  names(resultados_lsa)
)

if (length(faltantes_tfidf) > 0) {
  stop(
    paste0(
      "Faltan columnas en TF-IDF: ",
      paste(faltantes_tfidf, collapse = ", ")
    )
  )
}

if (length(faltantes_lsa) > 0) {
  stop(
    paste0(
      "Faltan columnas en LSA: ",
      paste(faltantes_lsa, collapse = ", ")
    )
  )
}

# 4. Identificación de las consultas

consultas <- unique(
  c(
    resultados_tfidf$consulta,
    resultados_lsa$consulta
  )
)

cat("\n2. Consultas disponibles\n")
print(consultas)

# 5. Preparación de los rankings

ranking_tfidf <- resultados_tfidf %>%
  transmute(
    consulta,
    paper_id,
    title,
    topic_label,
    posicion_tfidf = posicion,
    puntaje_tfidf = puntaje
  )

ranking_lsa <- resultados_lsa %>%
  transmute(
    consulta,
    paper_id,
    title,
    topic_label,
    posicion_lsa = posicion,
    puntaje_lsa = puntaje
  )

# 6. Comparación artículo por artículo

comparacion_articulos <- full_join(
  ranking_tfidf,
  ranking_lsa,
  by = c(
    "consulta",
    "paper_id"
  ),
  suffix = c("_tfidf", "_lsa")
) %>%
  mutate(
    title = dplyr::coalesce(
      title_tfidf,
      title_lsa
    ),
    
    topic_label = dplyr::coalesce(
      topic_label_tfidf,
      topic_label_lsa
    ),
    
    aparece_tfidf = !is.na(
      posicion_tfidf
    ),
    
    aparece_lsa = !is.na(
      posicion_lsa
    ),
    
    aparece_en_ambos = aparece_tfidf &
      aparece_lsa,
    
    cambio_posicion = ifelse(
      aparece_en_ambos,
      posicion_lsa - posicion_tfidf,
      NA_integer_
    )
  ) %>%
  select(
    consulta,
    paper_id,
    title,
    topic_label,
    posicion_tfidf,
    puntaje_tfidf,
    posicion_lsa,
    puntaje_lsa,
    aparece_tfidf,
    aparece_lsa,
    aparece_en_ambos,
    cambio_posicion
  ) %>%
  arrange(
    consulta,
    posicion_tfidf,
    posicion_lsa
  )

# 7. Solapamiento de los rankings

calcular_resumen_consulta <- function(
    consulta_actual
) {
  tfidf_actual <- ranking_tfidf %>%
    filter(
      consulta == consulta_actual
    ) %>%
    arrange(
      posicion_tfidf
    )
  
  lsa_actual <- ranking_lsa %>%
    filter(
      consulta == consulta_actual
    ) %>%
    arrange(
      posicion_lsa
    )
  
  ids_tfidf <- tfidf_actual$paper_id
  ids_lsa <- lsa_actual$paper_id
  
  articulos_compartidos <- length(
    base::intersect(
      ids_tfidf,
      ids_lsa
    )
  )
  
  union_articulos <- length(
    base::union(
      ids_tfidf,
      ids_lsa
    )
  )
  
  jaccard <- ifelse(
    union_articulos == 0,
    0,
    articulos_compartidos /
      union_articulos
  )
  
  mismas_posiciones <- sum(
    tfidf_actual$paper_id ==
      lsa_actual$paper_id
  )
  
  data.frame(
    consulta = consulta_actual,
    resultados_tfidf = length(
      ids_tfidf
    ),
    resultados_lsa = length(
      ids_lsa
    ),
    articulos_compartidos = articulos_compartidos,
    porcentaje_compartido = round(
      articulos_compartidos /
        max(
          length(ids_tfidf),
          length(ids_lsa)
        ) * 100,
      2
    ),
    similitud_jaccard = round(
      jaccard,
      4
    ),
    mismas_posiciones = mismas_posiciones,
    stringsAsFactors = FALSE
  )
}

resumen_solapamiento <- bind_rows(
  lapply(
    consultas,
    calcular_resumen_consulta
  )
)

cat("\n3. Solapamiento entre rankings\n")
print(resumen_solapamiento)

# 8. Tabla para evaluación manual

evaluacion_manual <- bind_rows(
  resultados_tfidf %>%
    transmute(
      consulta,
      estrategia = "TF-IDF",
      posicion,
      paper_id,
      title,
      topic_label,
      puntaje,
      relevante = "",
      observacion = ""
    ),
  
  resultados_lsa %>%
    transmute(
      consulta,
      estrategia = "LSA",
      posicion,
      paper_id,
      title,
      topic_label,
      puntaje,
      relevante = "",
      observacion = ""
    )
) %>%
  arrange(
    consulta,
    estrategia,
    posicion
  )

# 9. Plantilla de interpretación por consulta

plantilla_interpretacion <- data.frame(
  consulta = consultas,
  
  tipo_consulta = c(
    "Términos directos",
    "Consulta específica",
    "Términos relacionados",
    "Consulta específica con posible resultado incorrecto",
    "Consulta general con resultados poco precisos"
  ),
  
  mejor_estrategia = "",
  
  justificacion = "",
  
  resultado_incorrecto_tfidf = "",
  
  resultado_incorrecto_lsa = "",
  
  comentario_general = "",
  
  stringsAsFactors = FALSE
)

# 10. Comparación de tiempos

ruta_tiempo_tfidf <-
  "tiempo_busqueda_tfidf.csv"

ruta_tiempo_lsa <-
  "tiempo_busqueda_lsa.csv"

comparacion_tiempos <- NULL

if (
  file.exists(ruta_tiempo_tfidf) &&
  file.exists(ruta_tiempo_lsa)
) {
  tiempo_tfidf <- read.csv(
    ruta_tiempo_tfidf,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      estrategia = "TF-IDF"
    )
  
  tiempo_lsa <- read.csv(
    ruta_tiempo_lsa,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      estrategia = "LSA"
    )
  
  comparacion_tiempos <- bind_rows(
    tiempo_tfidf,
    tiempo_lsa
  ) %>%
    select(
      estrategia,
      indicador,
      valor
    )
  
  cat("\n4. Comparación de tiempos\n")
  print(comparacion_tiempos)
}

# 11. Exportación de resultados

write.csv(
  comparacion_articulos,
  "comparacion_articulos_tfidf_lsa.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  resumen_solapamiento,
  "resumen_solapamiento_rankings.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  evaluacion_manual,
  "plantilla_evaluacion_manual.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  plantilla_interpretacion,
  "plantilla_interpretacion_consultas.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

if (!is.null(comparacion_tiempos)) {
  write.csv(
    comparacion_tiempos,
    "comparacion_tiempos_busqueda.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

cat("\n5. Comparación finalizada correctamente\n")
cat("Archivos generados:\n")
cat("1. comparacion_articulos_tfidf_lsa.csv\n")
cat("2. resumen_solapamiento_rankings.csv\n")
cat("3. plantilla_evaluacion_manual.csv\n")
cat("4. plantilla_interpretacion_consultas.csv\n")

if (!is.null(comparacion_tiempos)) {
  cat("5. comparacion_tiempos_busqueda.csv\n")
}