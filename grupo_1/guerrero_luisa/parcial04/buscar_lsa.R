# Búsqueda con LSA y Truncated SVD
# Taller 4 - Minería de Datos
# Paso 6: representación reducida, proyección de consultas y ranking

library(dplyr)
library(stringr)
library(text2vec)
library(Matrix)
library(irlba)

# 1. Rutas de los objetos

ruta_modelo_tfidf <- file.path(
  "objetos_busqueda",
  "modelo_tfidf_completo.rds"
)

ruta_analisis_lsa <- file.path(
  "objetos_busqueda",
  "analisis_componentes_lsa.rds"
)

carpeta_objetos <- "objetos_busqueda"

if (!file.exists(ruta_modelo_tfidf)) {
  stop(
    paste0(
      "No se encontró el archivo ",
      ruta_modelo_tfidf,
      ". Ejecute primero construir_tfidf.R."
    )
  )
}

if (!file.exists(ruta_analisis_lsa)) {
  stop(
    paste0(
      "No se encontró el archivo ",
      ruta_analisis_lsa,
      ". Ejecute primero analizar_componentes_lsa.R."
    )
  )
}

if (!dir.exists(carpeta_objetos)) {
  dir.create(carpeta_objetos)
}

# 2. Carga de los modelos

objeto_tfidf <- readRDS(
  ruta_modelo_tfidf
)

objeto_analisis_lsa <- readRDS(
  ruta_analisis_lsa
)

objetos_tfidf_necesarios <- c(
  "corpus",
  "vectorizador",
  "modelo_tfidf",
  "matriz_tfidf",
  "configuracion"
)

objetos_tfidf_faltantes <- base::setdiff(
  objetos_tfidf_necesarios,
  names(objeto_tfidf)
)

if (length(objetos_tfidf_faltantes) > 0) {
  stop(
    paste0(
      "El objeto TF-IDF no contiene: ",
      paste(
        objetos_tfidf_faltantes,
        collapse = ", "
      )
    )
  )
}

objetos_lsa_necesarios <- c(
  "modelo_svd",
  "dimension_provisional",
  "energia_total",
  "dimension_original"
)

objetos_lsa_faltantes <- base::setdiff(
  objetos_lsa_necesarios,
  names(objeto_analisis_lsa)
)

if (length(objetos_lsa_faltantes) > 0) {
  stop(
    paste0(
      "El análisis LSA no contiene: ",
      paste(
        objetos_lsa_faltantes,
        collapse = ", "
      )
    )
  )
}

corpus_articulos <- objeto_tfidf$corpus
vectorizador <- objeto_tfidf$vectorizador
modelo_tfidf <- objeto_tfidf$modelo_tfidf
matriz_tfidf <- objeto_tfidf$matriz_tfidf
configuracion_texto <- objeto_tfidf$configuracion

modelo_svd_amplio <- objeto_analisis_lsa$modelo_svd
dimension_lsa <- objeto_analisis_lsa$dimension_provisional
energia_total <- objeto_analisis_lsa$energia_total

cat("\n1. Modelos cargados correctamente\n")
cat("Artículos:", nrow(corpus_articulos), "\n")
cat("Términos originales:", ncol(matriz_tfidf), "\n")
cat("Componentes LSA:", dimension_lsa, "\n")

# 3. Extracción de los componentes seleccionados

if (dimension_lsa > length(modelo_svd_amplio$d)) {
  stop(
    "La dimensión seleccionada supera los componentes disponibles."
  )
}

matriz_u <- modelo_svd_amplio$u[
  ,
  seq_len(dimension_lsa),
  drop = FALSE
]

valores_singulares <- modelo_svd_amplio$d[
  seq_len(dimension_lsa)
]

matriz_v <- modelo_svd_amplio$v[
  ,
  seq_len(dimension_lsa),
  drop = FALSE
]

if (nrow(matriz_v) != ncol(matriz_tfidf)) {
  stop(
    paste0(
      "La matriz V del modelo SVD no coincide con ",
      "el número de términos de TF-IDF."
    )
  )
}

# 4. Construcción de la representación LSA de los artículos

matriz_lsa <- sweep(
  matriz_u,
  MARGIN = 2,
  STATS = valores_singulares,
  FUN = "*"
)

if (
  nrow(matriz_lsa) != nrow(corpus_articulos) ||
  ncol(matriz_lsa) != dimension_lsa
) {
  stop(
    "La dimensión de la matriz LSA no coincide con lo esperado."
  )
}

cat("\n2. Representación reducida de los artículos\n")
cat("Filas:", nrow(matriz_lsa), "\n")
cat("Columnas:", ncol(matriz_lsa), "\n")
cat("Tipo de matriz:", class(matriz_lsa)[1], "\n")

# 5. Información conservada

energia_conservada <- sum(
  valores_singulares ^ 2
)

proporcion_conservada <- energia_conservada /
  energia_total

porcentaje_conservado <- round(
  proporcion_conservada * 100,
  2
)

cat("\n3. Información conservada\n")
cat(
  "Porcentaje conservado:",
  porcentaje_conservado,
  "%\n"
)

# 6. Memoria de la representación

memoria_tfidf_mb <- round(
  as.numeric(
    object.size(matriz_tfidf)
  ) / 1024 ^ 2,
  4
)

memoria_lsa_mb <- round(
  as.numeric(
    object.size(matriz_lsa)
  ) / 1024 ^ 2,
  4
)

reduccion_memoria <- round(
  (
    1 -
      memoria_lsa_mb /
      memoria_tfidf_mb
  ) * 100,
  2
)

cat("\n4. Comparación de memoria\n")
cat("TF-IDF:", memoria_tfidf_mb, "MB\n")
cat("LSA:", memoria_lsa_mb, "MB\n")
cat(
  "Reducción aproximada:",
  reduccion_memoria,
  "%\n"
)

# 7. Normas de los documentos en el espacio LSA

normas_documentos_lsa <- sqrt(
  rowSums(
    matriz_lsa ^ 2
  )
)

normas_documentos_lsa[
  is.na(normas_documentos_lsa) |
    normas_documentos_lsa == 0
] <- 1

cat("\n5. Normas de los documentos LSA\n")
cat(
  "Norma mínima:",
  round(
    min(normas_documentos_lsa),
    6
  ),
  "\n"
)

cat(
  "Norma máxima:",
  round(
    max(normas_documentos_lsa),
    6
  ),
  "\n"
)

# 8. Normalización de consultas

normalizar_consulta_lsa <- function(texto) {
  texto <- ifelse(
    is.na(texto),
    "",
    as.character(texto)
  )
  
  texto %>%
    stringr::str_replace_all(
      "<[^>]+>",
      " "
    ) %>%
    stringr::str_replace_all(
      "&nbsp;",
      " "
    ) %>%
    stringr::str_replace_all(
      "&amp;",
      " and "
    ) %>%
    stringr::str_replace_all(
      "&lt;",
      " "
    ) %>%
    stringr::str_replace_all(
      "&gt;",
      " "
    ) %>%
    stringr::str_replace_all(
      "&#?[A-Za-z0-9]+;",
      " "
    ) %>%
    stringr::str_replace_all(
      "[\r\n\t]",
      " "
    ) %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(
      "[^[:alnum:]\\- ]",
      " "
    ) %>%
    stringr::str_replace_all(
      "(?<![[:alnum:]])[0-9]+(?![[:alnum:]])",
      " "
    ) %>%
    stringr::str_squish()
}

# 9. Tokenización de consultas

tokenizar_consulta_lsa <- function(textos) {
  tokens <- text2vec::word_tokenizer(
    textos
  )
  
  lapply(
    tokens,
    function(tokens_documento) {
      tokens_documento <- stringr::str_to_lower(
        tokens_documento
      )
      
      tokens_documento <- tokens_documento[
        stringr::str_length(
          tokens_documento
        ) >= configuracion_texto$longitud_minima_token
      ]
      
      tokens_documento <- tokens_documento[
        !tokens_documento %in%
          configuracion_texto$stopwords
      ]
      
      tokens_documento <- tokens_documento[
        !stringr::str_detect(
          tokens_documento,
          "^[0-9]+$"
        )
      ]
      
      tokens_documento <- tokens_documento[
        stringr::str_detect(
          tokens_documento,
          "[a-z]"
        )
      ]
      
      tokens_documento
    }
  )
}

# 10. Transformación de una consulta a TF-IDF

transformar_consulta_tfidf_lsa <- function(
    consulta
) {
  if (
    is.null(consulta) ||
    length(consulta) == 0 ||
    is.na(consulta) ||
    stringr::str_trim(consulta) == ""
  ) {
    stop(
      "La consulta no puede estar vacía."
    )
  }
  
  consulta_limpia <- normalizar_consulta_lsa(
    consulta
  )
  
  if (consulta_limpia == "") {
    stop(
      "La consulta quedó vacía después del procesamiento."
    )
  }
  
  iterador_consulta <- text2vec::itoken(
    consulta_limpia,
    ids = "consulta",
    tokenizer = tokenizar_consulta_lsa,
    progressbar = FALSE
  )
  
  matriz_consulta_frecuencias <-
    text2vec::create_dtm(
      iterador_consulta,
      vectorizador
    )
  
  if (
    Matrix::nnzero(
      matriz_consulta_frecuencias
    ) == 0
  ) {
    stop(
      paste0(
        "Ningún término de la consulta pertenece al ",
        "vocabulario del buscador."
      )
    )
  }
  
  vector_consulta_tfidf <-
    modelo_tfidf$transform(
      matriz_consulta_frecuencias
    )
  
  if (!inherits(
    vector_consulta_tfidf,
    "sparseMatrix"
  )) {
    vector_consulta_tfidf <- methods::as(
      vector_consulta_tfidf,
      "dgCMatrix"
    )
  }
  
  vector_consulta_tfidf
}

# 11. Proyección de la consulta al espacio LSA

proyectar_consulta_lsa <- function(
    consulta
) {
  vector_consulta_tfidf <-
    transformar_consulta_tfidf_lsa(
      consulta
    )
  
  vector_consulta_lsa <- as.numeric(
    vector_consulta_tfidf %*%
      matriz_v
  )
  
  if (
    length(vector_consulta_lsa) !=
    dimension_lsa
  ) {
    stop(
      "La consulta no fue proyectada a la dimensión LSA esperada."
    )
  }
  
  if (
    all(
      is.na(vector_consulta_lsa) |
      vector_consulta_lsa == 0
    )
  ) {
    stop(
      "La consulta produjo un vector LSA vacío."
    )
  }
  
  vector_consulta_lsa
}

# 12. Similitud coseno en el espacio LSA

calcular_similitud_coseno_lsa <- function(
    vector_consulta_lsa
) {
  norma_consulta <- sqrt(
    sum(
      vector_consulta_lsa ^ 2
    )
  )
  
  if (
    is.na(norma_consulta) ||
    norma_consulta == 0
  ) {
    stop(
      "La consulta produjo una norma LSA igual a cero."
    )
  }
  
  productos <- as.numeric(
    matriz_lsa %*%
      vector_consulta_lsa
  )
  
  similitudes <- productos /
    (
      normas_documentos_lsa *
        norma_consulta
    )
  
  similitudes[
    is.na(similitudes) |
      is.infinite(similitudes)
  ] <- 0
  
  similitudes <- pmax(
    -1,
    pmin(
      1,
      similitudes
    )
  )
  
  similitudes
}

# 13. Creación de fragmentos

crear_fragmento_lsa <- function(
    resumen,
    consulta,
    longitud = 280
) {
  resumen <- ifelse(
    is.na(resumen),
    "",
    as.character(resumen)
  )
  
  resumen <- stringr::str_squish(
    resumen
  )
  
  if (resumen == "") {
    return(
      "Resumen no disponible."
    )
  }
  
  terminos_consulta <- consulta %>%
    normalizar_consulta_lsa() %>%
    stringr::str_split("\\s+") %>%
    unlist() %>%
    unique()
  
  terminos_consulta <- terminos_consulta[
    stringr::str_length(
      terminos_consulta
    ) >= 3
  ]
  
  posicion_inicial <- NA_integer_
  
  for (termino in terminos_consulta) {
    posicion <- stringr::str_locate(
      stringr::str_to_lower(
        resumen
      ),
      stringr::fixed(
        stringr::str_to_lower(
          termino
        )
      )
    )[1]
    
    if (!is.na(posicion)) {
      posicion_inicial <- posicion
      break
    }
  }
  
  if (is.na(posicion_inicial)) {
    return(
      stringr::str_trunc(
        resumen,
        width = longitud,
        side = "right",
        ellipsis = "..."
      )
    )
  }
  
  inicio <- max(
    1,
    posicion_inicial - 80
  )
  
  fin <- min(
    nchar(resumen),
    inicio + longitud
  )
  
  fragmento <- substr(
    resumen,
    inicio,
    fin
  )
  
  if (inicio > 1) {
    fragmento <- paste0(
      "...",
      fragmento
    )
  }
  
  if (fin < nchar(resumen)) {
    fragmento <- paste0(
      fragmento,
      "..."
    )
  }
  
  fragmento
}

# 14. Función principal de búsqueda LSA

buscar_articulos_lsa <- function(
    consulta,
    n_resultados = 10
) {
  if (
    !is.numeric(n_resultados) ||
    length(n_resultados) != 1 ||
    is.na(n_resultados) ||
    n_resultados < 1
  ) {
    stop(
      "El número de resultados debe ser un entero positivo."
    )
  }
  
  n_resultados <- min(
    as.integer(n_resultados),
    nrow(corpus_articulos)
  )
  
  vector_consulta_lsa <- proyectar_consulta_lsa(
    consulta
  )
  
  puntajes <- calcular_similitud_coseno_lsa(
    vector_consulta_lsa
  )
  
  resultados <- corpus_articulos %>%
    mutate(
      puntaje_lsa = puntajes,
      
      citations_ranking = ifelse(
        is.na(citations),
        0,
        citations
      ),
      
      fecha_ranking = suppressWarnings(
        as.Date(publication_date)
      )
    ) %>%
    arrange(
      desc(puntaje_lsa),
      desc(citations_ranking),
      desc(fecha_ranking),
      title_limpio
    ) %>%
    slice_head(
      n = n_resultados
    ) %>%
    mutate(
      posicion = dplyr::row_number(),
      
      puntaje_lsa = round(
        puntaje_lsa,
        6
      ),
      
      fragmento = vapply(
        abstract_limpio,
        crear_fragmento_lsa,
        consulta = consulta,
        FUN.VALUE = character(1)
      ),
      
      enlace_doi = ifelse(
        is.na(url) |
          stringr::str_trim(url) == "",
        paste0(
          "https://doi.org/",
          doi
        ),
        url
      ),
      
      estrategia = paste0(
        "LSA con ",
        dimension_lsa,
        " componentes + similitud coseno"
      )
    ) %>%
    select(
      posicion,
      paper_id,
      title = title_limpio,
      authors = authors_raw,
      publication_date,
      topic_label,
      doi,
      enlace_doi,
      puntaje = puntaje_lsa,
      fragmento,
      citations,
      estrategia
    )
  
  resultados
}

# 15. Consultas de prueba

consultas_prueba <- c(
  "generative artificial intelligence and large language models",
  "machine learning for disease diagnosis",
  "social media sentiment analysis",
  "cybersecurity intrusion detection",
  "statistical methods for prediction"
)

cat("\n6. Consultas iniciales de prueba LSA\n")

resultados_prueba_lsa <- vector(
  mode = "list",
  length = length(consultas_prueba)
)

for (i in seq_along(consultas_prueba)) {
  consulta_actual <- consultas_prueba[i]
  
  cat(
    "\nConsulta",
    i,
    ":",
    consulta_actual,
    "\n"
  )
  
  resultado_actual <- tryCatch(
    buscar_articulos_lsa(
      consulta = consulta_actual,
      n_resultados = 5
    ),
    error = function(e) {
      cat(
        "Error:",
        conditionMessage(e),
        "\n"
      )
      
      NULL
    }
  )
  
  resultados_prueba_lsa[[i]] <-
    resultado_actual
  
  if (!is.null(resultado_actual)) {
    print(
      resultado_actual %>%
        select(
          posicion,
          title,
          topic_label,
          puntaje
        )
    )
  }
}

# 16. Unión y exportación de resultados

resultados_exportacion_lsa <- dplyr::bind_rows(
  lapply(
    seq_along(resultados_prueba_lsa),
    function(i) {
      resultado <- resultados_prueba_lsa[[i]]
      
      if (is.null(resultado)) {
        return(NULL)
      }
      
      resultado %>%
        mutate(
          consulta = consultas_prueba[i],
          .before = 1
        )
    }
  )
)

if (nrow(resultados_exportacion_lsa) > 0) {
  write.csv(
    resultados_exportacion_lsa,
    "resultados_prueba_lsa.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

# 17. Validación de puntajes

if (nrow(resultados_exportacion_lsa) > 0) {
  puntaje_minimo <- min(
    resultados_exportacion_lsa$puntaje,
    na.rm = TRUE
  )
  
  puntaje_maximo <- max(
    resultados_exportacion_lsa$puntaje,
    na.rm = TRUE
  )
  
  cat("\n7. Validación de puntajes LSA\n")
  cat(
    "Puntaje mínimo:",
    puntaje_minimo,
    "\n"
  )
  cat(
    "Puntaje máximo:",
    puntaje_maximo,
    "\n"
  )
  
  if (
    puntaje_minimo < -1 ||
    puntaje_maximo > 1
  ) {
    stop(
      paste0(
        "Los puntajes LSA se encuentran fuera ",
        "del intervalo esperado entre -1 y 1."
      )
    )
  }
}

# 18. Medición del tiempo de búsqueda

consulta_tiempo <-
  "deep learning methods for image classification"

tiempos_lsa <- replicate(
  20,
  system.time(
    buscar_articulos_lsa(
      consulta = consulta_tiempo,
      n_resultados = 10
    )
  )[["elapsed"]]
)

resumen_tiempo_lsa <- data.frame(
  indicador = c(
    "Número de ejecuciones",
    "Tiempo mínimo en segundos",
    "Tiempo promedio en segundos",
    "Mediana en segundos",
    "Tiempo máximo en segundos"
  ),
  
  valor = c(
    length(tiempos_lsa),
    min(tiempos_lsa),
    mean(tiempos_lsa),
    median(tiempos_lsa),
    max(tiempos_lsa)
  ),
  
  stringsAsFactors = FALSE
)

cat("\n8. Tiempo de respuesta de LSA\n")
print(resumen_tiempo_lsa)

write.csv(
  resumen_tiempo_lsa,
  "tiempo_busqueda_lsa.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 19. Resumen de la representación LSA

resumen_modelo_lsa <- data.frame(
  indicador = c(
    "Método",
    "Tipo de recuperación",
    "Dimensión original",
    "Dimensión reducida",
    "Componentes",
    "Información conservada",
    "Memoria TF-IDF en MB",
    "Memoria LSA en MB",
    "Reducción aproximada de memoria",
    "Procedimiento de consulta",
    "Mecanismo de ranking"
  ),
  
  valor = c(
    "Truncated SVD para LSA",
    "Latente o semántica aproximada",
    paste(
      nrow(matriz_tfidf),
      "x",
      ncol(matriz_tfidf)
    ),
    paste(
      nrow(matriz_lsa),
      "x",
      ncol(matriz_lsa)
    ),
    as.character(
      dimension_lsa
    ),
    paste0(
      porcentaje_conservado,
      "%"
    ),
    as.character(
      memoria_tfidf_mb
    ),
    as.character(
      memoria_lsa_mb
    ),
    paste0(
      reduccion_memoria,
      "%"
    ),
    paste0(
      "Consulta a TF-IDF y luego proyectada ",
      "con la matriz V del mismo modelo SVD"
    ),
    "Similitud coseno en el espacio reducido"
  ),
  
  stringsAsFactors = FALSE
)

cat("\n9. Resumen del modelo LSA\n")
print(resumen_modelo_lsa)

# 20. Guardado del objeto LSA completo

objeto_lsa <- list(
  corpus = corpus_articulos,
  matriz_lsa = matriz_lsa,
  matriz_v = matriz_v,
  valores_singulares = valores_singulares,
  dimension_lsa = dimension_lsa,
  porcentaje_conservado = porcentaje_conservado,
  normas_documentos_lsa = normas_documentos_lsa,
  memoria_tfidf_mb = memoria_tfidf_mb,
  memoria_lsa_mb = memoria_lsa_mb,
  reduccion_memoria = reduccion_memoria
)

saveRDS(
  objeto_lsa,
  file.path(
    carpeta_objetos,
    "modelo_lsa_completo.rds"
  )
)

saveRDS(
  matriz_lsa,
  file.path(
    carpeta_objetos,
    "matriz_lsa.rds"
  )
)

save(
  buscar_articulos_lsa,
  proyectar_consulta_lsa,
  transformar_consulta_tfidf_lsa,
  calcular_similitud_coseno_lsa,
  normalizar_consulta_lsa,
  tokenizar_consulta_lsa,
  crear_fragmento_lsa,
  file = file.path(
    carpeta_objetos,
    "funciones_busqueda_lsa.RData"
  )
)

write.csv(
  resumen_modelo_lsa,
  "resumen_modelo_lsa.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\n10. Construcción del ranking LSA finalizada correctamente\n")
cat("Archivos generados:\n")
cat("1. objetos_busqueda/modelo_lsa_completo.rds\n")
cat("2. objetos_busqueda/matriz_lsa.rds\n")
cat("3. objetos_busqueda/funciones_busqueda_lsa.RData\n")
cat("4. resultados_prueba_lsa.csv\n")
cat("5. tiempo_busqueda_lsa.csv\n")
cat("6. resumen_modelo_lsa.csv\n")