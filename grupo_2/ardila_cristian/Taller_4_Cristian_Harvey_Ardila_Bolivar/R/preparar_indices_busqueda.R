#------------------------------------------------------------
# Preparación de la colección de búsqueda
# Funciones para consulta y reconstrucción de índices
#------------------------------------------------------------

#------------------------------------------------------------
# 1. Funciones utilizadas por la aplicación para consultar
#------------------------------------------------------------

# Genera las funciones serializables de consulta y reconstruye TF-IDF/LSA
# únicamente cuando cambia papers. Los RDS se validan antes de reemplazarse.

crear_funciones_busqueda <- function() { # Construir entorno serializable con funciones de consulta
  funciones <- new.env(parent = baseenv()) # Aislar funciones para guardarlas juntas en un RDS
  
  funciones$procesar_consulta <- function(consulta, modelo) { # Aplicar a la consulta la misma limpieza del corpus
    if (
      length(consulta) != 1 ||
      is.na(consulta) ||
      !nzchar(trimws(consulta))
    ) {
      stop("La consulta debe contener texto.")
    }
    
    tibble::tibble(
      texto_limpio = consulta |> # Normalizar guiones, controles, mayúsculas y espacios
        stringr::str_replace_all(
          modelo$parametros$patron_guiones,
          "-"
        ) |>
        stringr::str_replace_all("[[:cntrl:]]+", " ") |>
        stringr::str_to_lower() |>
        stringr::str_squish()
    ) |>
      tidytext::unnest_tokens( # Separar la consulta en términos comparables con el vocabulario
        output = token,
        input = texto_limpio,
        token = "regex",
        pattern = modelo$parametros$patron_tokenizacion
      ) |>
      dplyr::mutate(
        token = stringr::str_remove_all(token, "^-+|-+$")
      ) |>
      dplyr::filter( # Retener términos informativos y compatibles con el modelo
        token != "",
        stringr::str_detect(token, "[[:alpha:]]"),
        !token %in% modelo$parametros$stopwords,
        stringr::str_length(token) >=
          modelo$parametros$longitud_minima_token
      )
  }
  environment(funciones$procesar_consulta) <- funciones # Permitir llamadas internas al recargar el entorno
  
  funciones$vectorizar_consulta_tfidf <- function(consulta, modelo) { # Convertir la consulta en el vector TF-IDF del corpus
    consulta_tfidf <- procesar_consulta(consulta, modelo) |> # Procesar, contar y ponderar términos de la consulta
      dplyr::count(token, name = "n") |>
      dplyr::semi_join(modelo$vocabulario, by = "token") |>
      dplyr::mutate(tf = n / sum(n)) |>
      dplyr::left_join(modelo$idf_vocabulario, by = "token") |>
      dplyr::mutate(tf_idf = tf * idf)
    
    if (nrow(consulta_tfidf) == 0) {
      stop("La consulta no contiene términos presentes en la colección.")
    }
    
    vector_consulta <- numeric(ncol(modelo$matriz_tfidf)) # Crear vector con una posición por término del vocabulario
    names(vector_consulta) <- colnames(modelo$matriz_tfidf)
    posiciones <- match(consulta_tfidf$token, names(vector_consulta)) # Localizar términos de la consulta en las columnas
    vector_consulta[posiciones] <- consulta_tfidf$tf_idf
    vector_consulta
  }
  environment(funciones$vectorizar_consulta_tfidf) <- funciones
  
  funciones$formatear_ranking <- function(puntajes, modelo, n_resultados) { # Ordenar y enriquecer resultados con metadatos
    tibble::tibble(
      paper_id = as.integer(rownames(modelo$matriz_tfidf)),
      puntaje = puntajes
    ) |>
      dplyr::left_join(modelo$metadatos, by = "paper_id") |>
      dplyr::arrange( # Resolver empates por fecha descendente y paper_id ascendente
        dplyr::desc(puntaje),
        dplyr::desc(publication_date),
        paper_id
      ) |>
      dplyr::slice_head(n = n_resultados) |>
      dplyr::mutate(
        posicion = dplyr::row_number(),
        fragmento = stringr::str_trunc(
          dplyr::coalesce(abstract, ""),
          width = 300
        )
      ) |>
      dplyr::select(
        posicion,
        paper_id,
        title,
        authors_raw,
        publication_date,
        topic_label,
        doi,
        url,
        puntaje,
        fragmento
      )
  }
  environment(funciones$formatear_ranking) <- funciones
  
  funciones$buscar_tfidf <- function(consulta, n_resultados = 10, modelo) { # Recuperar documentos por coseno TF-IDF
    if (!n_resultados %in% modelo$parametros$n_resultados_disponibles) {
      stop("El número de resultados debe ser 5, 10 o 20.")
    }
    
    # Alinear la consulta con el vocabulario.
    vector_consulta <- vectorizar_consulta_tfidf(consulta, modelo)
    similitudes <- proxy::simil( # Comparar la consulta contra todos los documentos
      modelo$matriz_tfidf,
      matrix(vector_consulta, nrow = 1),
      method = "cosine"
    ) |>
      as.numeric()
    
    formatear_ranking(similitudes, modelo, n_resultados)
  }
  environment(funciones$buscar_tfidf) <- funciones
  
  funciones$buscar_lsa <- function(consulta, n_resultados = 10, modelo) { # Recuperar documentos en el espacio LSA
    if (!n_resultados %in% modelo$parametros$n_resultados_disponibles) {
      stop("El número de resultados debe ser 5, 10 o 20.")
    }
    
    # Construir o actualizar el objeto vector_consulta_tfidf utilizado en esta etapa.
    vector_consulta_tfidf <- vectorizar_consulta_tfidf(consulta, modelo)
    vector_consulta_lsa <- matrix( # Proyectar consulta TF-IDF sobre los vectores de términos
      vector_consulta_tfidf,
      nrow = 1
    ) %*% modelo$svd$vectores_terminos
    
    # Calcular similitud coseno con todos los artículos.
    similitudes <- proxy::simil(
      modelo$matriz_lsa,
      vector_consulta_lsa,
      method = "cosine"
    ) |>
      as.numeric()
    
    formatear_ranking(similitudes, modelo, n_resultados)
  }
  environment(funciones$buscar_lsa) <- funciones
  
  funciones
}

#------------------------------------------------------------
# 2. Reconstrucción de matrices y archivos RDS
#------------------------------------------------------------

preparar_indices_busqueda <- function( # Reconstruir índices únicamente cuando cambia la tabla papers
  ruta_sqlite = "revista_q1_2025.sqlite",
  tabla_principal = "papers",
  archivo_modelo = file.path("modelos", "modelo_busqueda_taller4.rds"),
  archivo_funciones = file.path("modelos", "funciones_busqueda_taller4.rds"),
  min_documentos_termino = 2L,
  max_proporcion_documentos = 0.90,
  k_lsa = NULL
) {
  # Fijar reproducibilidad.
  set.seed(2026) # Mantener reproducibilidad de la preparación
  
  # Abrir la conexión a SQLite.
  con <- DBI::dbConnect(RSQLite::SQLite(), ruta_sqlite) # Abrir conexión temporal a SQLite
  # Cerrar la conexión para evitar bloqueos.
  on.exit(DBI::dbDisconnect(con), add = TRUE) # Garantizar cierre incluso si ocurre un error
  
  # Ejecutar la consulta SQL y recuperar los datos.
  papers <- DBI::dbGetQuery( # Leer el corpus completo después de la inserción
    con,
    paste0(
      "SELECT * FROM ",
      DBI::dbQuoteIdentifier(con, tabla_principal)
    )
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      publication_date = as.Date(publication_date),
      dplyr::across(
        dplyr::any_of(c(
          "title", "abstract", "keywords", "authors_raw",
          "topic_label", "doi", "url"
        )),
        as.character
      ),
      dplyr::across(dplyr::where(is.character), stringr::str_squish)
    )
  
  corpus_articulos <- papers |> # Integrar título, resumen y palabras clave por artículo
    dplyr::mutate(
      texto_documento = stringr::str_squish(
        paste(
          dplyr::coalesce(title, ""),
          dplyr::coalesce(abstract, ""),
          dplyr::coalesce(keywords, "")
        )
      )
    ) |>
    dplyr::filter(texto_documento != "") |>
    dplyr::select(
      paper_id, title, authors_raw, publication_date, topic_label,
      doi, url, abstract, keywords, texto_documento
    )
  
  tokens_sin_filtrar <- corpus_articulos |> # Normalizar y tokenizar todos los documentos
    dplyr::transmute(
      paper_id,
      texto_limpio = texto_documento |>
        stringr::str_replace_all("[‐‑‒–—−]", "-") |>
        stringr::str_replace_all("[[:cntrl:]]+", " ") |>
        stringr::str_to_lower() |>
        stringr::str_squish()
    ) |>
    tidytext::unnest_tokens(
      output = token,
      input = texto_limpio,
      token = "regex",
      pattern = "[^[:alnum:]-]+"
    ) |>
    dplyr::mutate(
      token = stringr::str_remove_all(token, "^-+|-+$")
    ) |>
    dplyr::filter(
      token != "",
      stringr::str_detect(token, "[[:alpha:]]")
    )
  
  marcadores_idioma <- tibble::tibble( # Palabras frecuentes usadas para identificar idioma predominante
    idioma = rep(c("Inglés", "Español"), each = 12),
    token = c(
      "the", "of", "and", "to", "in", "for",
      "a", "is", "with", "on", "by", "from",
      "el", "la", "los", "las", "de", "y",
      "en", "para", "con", "un", "una", "por"
    )
  )
  
  idioma_predominante <- tokens_sin_filtrar |> # Comparar frecuencia de marcadores ingleses y españoles
    dplyr::count(token, name = "frecuencia") |>
    dplyr::inner_join(marcadores_idioma, by = "token") |>
    dplyr::group_by(idioma) |>
    dplyr::summarise(
      coincidencias = sum(frecuencia),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(coincidencias)) |>
    dplyr::slice_head(n = 1) |>
    dplyr::pull(idioma)
  
  if (length(idioma_predominante) == 0) {
    # Seleccionar el idioma dominante del corpus.
    idioma_predominante <- "Inglés"
  }
  
  stopwords_corpus <- if (idioma_predominante == "Español") { # Elegir lista de palabras vacías según el corpus
    tm::stopwords("spanish")
  } else {
    tm::stopwords("english")
  }
  
  tokens_articulos <- tokens_sin_filtrar |> # Eliminar stopwords y términos de un carácter
    dplyr::filter(
      !token %in% stopwords_corpus,
      stringr::str_length(token) > 1
    )
  
  conteo_terminos <- tokens_articulos |> # Contar cada término dentro de cada artículo
    dplyr::count(paper_id, token, name = "n")
  
  frecuencia_documental <- conteo_terminos |> # Calcular en cuántos documentos aparece cada término
    dplyr::count(token, name = "n_documentos") |>
    dplyr::mutate(
      proporcion_documentos = n_documentos / nrow(corpus_articulos)
    )
  
  vocabulario_tfidf <- frecuencia_documental |> # Aplicar umbrales mínimo y máximo del vocabulario
    dplyr::filter(
      n_documentos >= min_documentos_termino,
      proporcion_documentos <= max_proporcion_documentos
    )
  
  tfidf_articulos <- conteo_terminos |> # Calcular pesos TF-IDF con el vocabulario filtrado
    dplyr::semi_join(vocabulario_tfidf, by = "token") |>
    tidytext::bind_tf_idf(
      term = token,
      document = paper_id,
      n = n
    )
  
  dtm_tfidf <- tfidf_articulos |> # Construir matriz documento-término dispersa
    tidytext::cast_dtm(
      document = paper_id,
      term = token,
      value = tf_idf
    )
  
  matriz_tfidf <- as.matrix(dtm_tfidf) # Convertir a matriz densa para SVD y similitud
  idf_vocabulario <- tfidf_articulos |> # Guardar IDF necesario para nuevas consultas
    dplyr::distinct(token, idf)
  
  if (is.null(k_lsa)) { # Conservar la dimensión elegida previamente salvo indicación contraria
    # Construir o actualizar el objeto k_lsa utilizado en esta etapa.
    k_lsa <- if (file.exists(archivo_modelo)) {
      # Leer el modelo previo para conservar la dimensión.
      modelo_anterior <- readRDS(archivo_modelo)
      modelo_anterior$svd$k_final
    } else {
      100L
    }
  }
  
  rango_disponible <- min(nrow(matriz_tfidf), ncol(matriz_tfidf)) # Límite de componentes posibles
  # Construir o actualizar el objeto k_final utilizado en esta etapa.
  k_final <- min(as.integer(k_lsa), rango_disponible - 1L)
  
  if (k_final < 2L) {
    stop("La colección no tiene dimensión suficiente para construir LSA.")
  }
  
  svd_tfidf <- svd( # Descomponer la matriz TF-IDF con la dimensión seleccionada
    matriz_tfidf,
    nu = k_final,
    nv = k_final
  )
  
  vectores_terminos_lsa <- svd_tfidf$v[, 1:k_final, drop = FALSE] # Direcciones para proyectar documentos y consultas
  rownames(vectores_terminos_lsa) <- colnames(matriz_tfidf)
  colnames(vectores_terminos_lsa) <- paste0("LSA_", seq_len(k_final))
  
  matriz_lsa <- matriz_tfidf %*% vectores_terminos_lsa # Representar documentos en el espacio reducido
  colnames(matriz_lsa) <- colnames(vectores_terminos_lsa)
  
  # Conservar los valores singulares seleccionados.
  valores_singulares_lsa <- svd_tfidf$d[1:k_final]
  # Calcular la información retenida.
  informacion_k_final <- sum(valores_singulares_lsa^2) /
    sum(svd_tfidf$d^2)
  
  modelo_busqueda <- list( # Reunir objetos necesarios para buscar sin reconstruir el corpus
    informacion = list(
      fecha_creacion = Sys.time(),
      articulos = nrow(corpus_articulos),
      fecha_minima = min(corpus_articulos$publication_date, na.rm = TRUE),
      fecha_maxima = max(corpus_articulos$publication_date, na.rm = TRUE)
    ),
    metadatos = corpus_articulos |>
      dplyr::select(
        paper_id, title, authors_raw, publication_date, topic_label,
        doi, url, abstract, keywords
      ),
    vocabulario = vocabulario_tfidf,
    idf_vocabulario = idf_vocabulario,
    dtm_tfidf = dtm_tfidf,
    matriz_tfidf = matriz_tfidf,
    svd = list(
      k_final = k_final,
      valores_singulares = valores_singulares_lsa,
      vectores_terminos = vectores_terminos_lsa,
      informacion_acumulada = informacion_k_final
    ),
    matriz_lsa = matriz_lsa,
    parametros = list(
      idioma_predominante = idioma_predominante,
      stopwords = stopwords_corpus,
      min_documentos_termino = min_documentos_termino,
      max_proporcion_documentos = max_proporcion_documentos,
      n_resultados_disponibles = c(5, 10, 20),
      patron_guiones = "[‐‑‒–—−]",
      patron_tokenizacion = "[^[:alnum:]-]+",
      longitud_minima_token = 2L
    )
  )
  
  funciones_busqueda <- crear_funciones_busqueda() # Crear entorno de funciones que acompañará al modelo
  
  dir.create(dirname(archivo_modelo), recursive = TRUE, showWarnings = FALSE) # Asegurar carpeta de modelos
  dir.create(dirname(archivo_funciones), recursive = TRUE, showWarnings = FALSE)
  
  temporal_modelo <- tempfile(tmpdir = dirname(archivo_modelo), fileext = ".rds") # Guardar primero en archivo temporal
  # Crear un destino temporal para las funciones.
  temporal_funciones <- tempfile(tmpdir = dirname(archivo_funciones), fileext = ".rds")
  
  # Serializar el objeto para reutilizarlo.
  saveRDS(modelo_busqueda, temporal_modelo, compress = "xz") # Serializar modelo comprimido
  # Serializar el objeto para reutilizarlo.
  saveRDS(funciones_busqueda, temporal_funciones, compress = "xz")
  
  # Recargar un objeto previamente serializado.
  modelo_prueba <- readRDS(temporal_modelo) # Comprobar que el RDS puede volver a cargarse
  # Recargar las funciones para validar integridad.
  funciones_prueba <- readRDS(temporal_funciones)
  
  if (
    is.null(modelo_prueba$matriz_tfidf) ||
    !is.environment(funciones_prueba) ||
    !all(c("buscar_tfidf", "buscar_lsa") %in% ls(funciones_prueba))
  ) {
    unlink(c(temporal_modelo, temporal_funciones)) # Eliminar temporales cuando la validación falla
    stop("No fue posible validar la colección de búsqueda.")
  }
  
  # Copiar o reemplazar el archivo solo después de validar.
  modelo_copiado <- file.copy( # Reemplazar el modelo solo después de validar temporales
    temporal_modelo,
    archivo_modelo,
    overwrite = TRUE
  )
  
  # Confirmar el reemplazo de funciones vigentes.
  funciones_copiadas <- file.copy(
    temporal_funciones,
    archivo_funciones,
    overwrite = TRUE
  )
  
  unlink(c(temporal_modelo, temporal_funciones)) # Eliminar temporales después de copiar los RDS definitivos
  
  if (!modelo_copiado || !funciones_copiadas) {
    stop("No fue posible guardar la colección de búsqueda.")
  }
  
  invisible(list( # Devolver resumen técnico sin imprimirlo en la aplicación
    articulos = nrow(corpus_articulos),
    terminos = ncol(matriz_tfidf),
    componentes = k_final,
    informacion_acumulada = informacion_k_final
  ))
}
