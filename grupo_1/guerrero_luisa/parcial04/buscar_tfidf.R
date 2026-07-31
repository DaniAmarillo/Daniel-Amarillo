# Búsqueda léxica con TF-IDF
# Taller 4 - Minería de Datos
# Paso 4: transformación de consultas, similitud coseno y ranking

library(dplyr)
library(stringr)
library(text2vec)
library(stopwords)
library(Matrix)

# 1. Ruta del modelo TF-IDF

ruta_modelo <- file.path(
  "objetos_busqueda",
  "modelo_tfidf_completo.rds"
)

if (!file.exists(ruta_modelo)) {
  stop(
    paste0(
      "No se encontró el archivo ",
      ruta_modelo,
      ". Ejecute primero construir_tfidf.R."
    )
  )
}

# 2. Carga del modelo y del corpus

objeto_tfidf <- readRDS(
  ruta_modelo
)

objetos_necesarios <- c(
  "corpus",
  "vocabulario",
  "vectorizador",
  "modelo_tfidf",
  "matriz_tfidf",
  "configuracion"
)

objetos_faltantes <- base::setdiff(
  objetos_necesarios,
  names(objeto_tfidf)
)

if (length(objetos_faltantes) > 0) {
  stop(
    paste0(
      "El modelo TF-IDF no contiene los siguientes objetos: ",
      paste(objetos_faltantes, collapse = ", ")
    )
  )
}

corpus_articulos <- objeto_tfidf$corpus
vectorizador <- objeto_tfidf$vectorizador
modelo_tfidf <- objeto_tfidf$modelo_tfidf
matriz_tfidf <- objeto_tfidf$matriz_tfidf
configuracion_texto <- objeto_tfidf$configuracion

cat("\n1. Modelo TF-IDF cargado correctamente\n")
cat("Artículos:", nrow(corpus_articulos), "\n")
cat("Términos:", ncol(matriz_tfidf), "\n")

# 3. Normas de los documentos

normas_documentos <- sqrt(
  Matrix::rowSums(
    matriz_tfidf ^ 2
  )
)

normas_documentos[
  is.na(normas_documentos) |
    normas_documentos == 0
] <- 1

cat("\n2. Normas de los documentos calculadas\n")
cat(
  "Norma mínima:",
  round(min(normas_documentos), 6),
  "\n"
)
cat(
  "Norma máxima:",
  round(max(normas_documentos), 6),
  "\n"
)

# 4. Función de normalización de consultas

normalizar_consulta <- function(texto) {
  texto <- ifelse(
    is.na(texto),
    "",
    as.character(texto)
  )
  
  texto %>%
    stringr::str_replace_all("<[^>]+>", " ") %>%
    stringr::str_replace_all("&nbsp;", " ") %>%
    stringr::str_replace_all("&amp;", " and ") %>%
    stringr::str_replace_all("&lt;", " ") %>%
    stringr::str_replace_all("&gt;", " ") %>%
    stringr::str_replace_all("&#?[A-Za-z0-9]+;", " ") %>%
    stringr::str_replace_all("[\r\n\t]", " ") %>%
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

# 5. Función de tokenización

tokenizar_consulta <- function(textos) {
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
        stringr::str_length(tokens_documento) >=
          configuracion_texto$longitud_minima_token
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

# 6. Función para crear fragmentos

crear_fragmento <- function(
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
    return("Resumen no disponible.")
  }
  
  terminos_consulta <- consulta %>%
    normalizar_consulta() %>%
    stringr::str_split("\\s+") %>%
    unlist() %>%
    unique()
  
  terminos_consulta <- terminos_consulta[
    stringr::str_length(terminos_consulta) >= 3
  ]
  
  posicion_inicial <- NA_integer_
  
  for (termino in terminos_consulta) {
    posicion <- stringr::str_locate(
      stringr::str_to_lower(resumen),
      stringr::fixed(
        stringr::str_to_lower(termino)
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

# 7. Función de transformación de la consulta

transformar_consulta_tfidf <- function(consulta) {
  if (
    is.null(consulta) ||
    length(consulta) == 0 ||
    is.na(consulta) ||
    stringr::str_trim(consulta) == ""
  ) {
    stop("La consulta no puede estar vacía.")
  }
  
  consulta_limpia <- normalizar_consulta(
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
    tokenizer = tokenizar_consulta,
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

# 8. Función de similitud coseno

calcular_similitud_coseno <- function(
    matriz_documentos,
    vector_consulta
) {
  norma_consulta <- sqrt(
    sum(
      vector_consulta ^ 2
    )
  )
  
  if (
    is.na(norma_consulta) ||
    norma_consulta == 0
  ) {
    stop(
      "La consulta produjo un vector con norma igual a cero."
    )
  }
  
  productos <- as.numeric(
    matriz_documentos %*%
      Matrix::t(
        vector_consulta
      )
  )
  
  similitudes <- productos /
    (
      normas_documentos *
        norma_consulta
    )
  
  similitudes[
    is.na(similitudes) |
      is.infinite(similitudes)
  ] <- 0
  
  similitudes <- pmax(
    0,
    pmin(
      1,
      similitudes
    )
  )
  
  similitudes
}

# 9. Función principal de búsqueda

buscar_articulos_tfidf <- function(
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
  
  vector_consulta <- transformar_consulta_tfidf(
    consulta
  )
  
  puntajes <- calcular_similitud_coseno(
    matriz_documentos = matriz_tfidf,
    vector_consulta = vector_consulta
  )
  
  resultados <- corpus_articulos %>%
    mutate(
      puntaje_tfidf = puntajes,
      
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
      desc(puntaje_tfidf),
      desc(citations_ranking),
      desc(fecha_ranking),
      title_limpio
    ) %>%
    slice_head(
      n = n_resultados
    ) %>%
    mutate(
      posicion = dplyr::row_number(),
      
      puntaje_tfidf = round(
        puntaje_tfidf,
        6
      ),
      
      fragmento = vapply(
        abstract_limpio,
        crear_fragmento,
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
      
      estrategia =
        "TF-IDF + similitud coseno"
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
      puntaje = puntaje_tfidf,
      fragmento,
      citations,
      estrategia
    )
  
  resultados
}

# 10. Consultas iniciales de prueba

consultas_prueba <- c(
  "generative artificial intelligence and large language models",
  "machine learning for disease diagnosis",
  "social media sentiment analysis",
  "cybersecurity intrusion detection",
  "statistical methods for prediction"
)

cat("\n3. Consultas iniciales de prueba\n")

resultados_prueba <- vector(
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
    buscar_articulos_tfidf(
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
  
  resultados_prueba[[i]] <- resultado_actual
  
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

# 11. Exportación de resultados

resultados_exportacion <- dplyr::bind_rows(
  lapply(
    seq_along(resultados_prueba),
    function(i) {
      resultado <- resultados_prueba[[i]]
      
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

if (nrow(resultados_exportacion) > 0) {
  write.csv(
    resultados_exportacion,
    "resultados_prueba_tfidf.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

# 12. Validación de los puntajes

if (nrow(resultados_exportacion) > 0) {
  puntaje_minimo <- min(
    resultados_exportacion$puntaje,
    na.rm = TRUE
  )
  
  puntaje_maximo <- max(
    resultados_exportacion$puntaje,
    na.rm = TRUE
  )
  
  cat("\n4. Validación de los puntajes\n")
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
    puntaje_minimo < 0 ||
    puntaje_maximo > 1
  ) {
    stop(
      "Los puntajes se encuentran fuera del intervalo esperado entre 0 y 1."
    )
  }
}

# 13. Medición del tiempo de respuesta

consulta_tiempo <-
  "deep learning methods for image classification"

tiempos <- replicate(
  20,
  system.time(
    buscar_articulos_tfidf(
      consulta = consulta_tiempo,
      n_resultados = 10
    )
  )[["elapsed"]]
)

resumen_tiempo <- data.frame(
  indicador = c(
    "Número de ejecuciones",
    "Tiempo mínimo en segundos",
    "Tiempo promedio en segundos",
    "Mediana en segundos",
    "Tiempo máximo en segundos"
  ),
  
  valor = c(
    length(tiempos),
    min(tiempos),
    mean(tiempos),
    median(tiempos),
    max(tiempos)
  ),
  
  stringsAsFactors = FALSE
)

cat("\n5. Tiempo de respuesta de TF-IDF\n")
print(resumen_tiempo)

write.csv(
  resumen_tiempo,
  "tiempo_busqueda_tfidf.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 14. Guardado de funciones

save(
  buscar_articulos_tfidf,
  transformar_consulta_tfidf,
  calcular_similitud_coseno,
  normalizar_consulta,
  tokenizar_consulta,
  crear_fragmento,
  normas_documentos,
  file = file.path(
    "objetos_busqueda",
    "funciones_busqueda_tfidf.RData"
  )
)

cat("\n6. Prueba del ranking TF-IDF finalizada correctamente\n")
cat("Archivos generados:\n")
cat("1. resultados_prueba_tfidf.csv\n")
cat("2. tiempo_busqueda_tfidf.csv\n")
cat("3. objetos_busqueda/funciones_busqueda_tfidf.RData\n")