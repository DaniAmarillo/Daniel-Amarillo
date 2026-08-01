#------------------------------------------------------------
# Taller 2 - Minería de Datos (Versión para taller 4)
# Funciones de scraping copiadas del Taller 1
# Revista: Frontiers in Bioinformatics
#------------------------------------------------------------

# Este archivo contiene la lógica necesaria para buscar artículos recientes
# y devolverlos con la misma estructura de columnas de la tabla papers con base
# en lo trabajado previamente en el Taller 1.

#------------------------------------------------------------
# Parámetros base
#------------------------------------------------------------

# Consulta el listado de Frontiers, visita los artículos publicados, extrae
# metadatos, citas y referencias, clasifica temas y devuelve la estructura papers.

# Definir el dominio usado para enlaces relativos.
base_frontiers <- "https://www.frontiersin.org"
# Definir el listado principal de la revista.
url_listado <- "https://www.frontiersin.org/journals/bioinformatics/articles"

# Simular un navegador convencional.
user_agent_frontiers <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)

#------------------------------------------------------------
# Funciones auxiliares
#------------------------------------------------------------

limpiar_texto <- function(x) { # Limpiar espacios, caracteres de control y textos vacíos
  # Construir o actualizar el objeto x utilizado en esta etapa.
  x <- as.character(x)
  # Construir o actualizar el objeto x utilizado en esta etapa.
  x <- str_replace_all(x, "\\u00A0", " ")
  # Construir o actualizar el objeto x utilizado en esta etapa.
  x <- str_replace_all(x, "[[:cntrl:]]+", " ")
  # Construir o actualizar el objeto x utilizado en esta etapa.
  x <- str_squish(x)
  ifelse(is.na(x) | x == "", NA_character_, x)
}

tomar_primero <- function(x) { # Tomar el primer valor no vacío
  # Construir o actualizar el objeto x utilizado en esta etapa.
  x <- limpiar_texto(x)
  # Construir o actualizar el objeto x utilizado en esta etapa.
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_character_ else x[1]
}

convertir_numero <- function(x) { # Convertir textos tipo 1.2k o 1,200 a número
  # Construir o actualizar el objeto x utilizado en esta etapa.
  x <- as.character(x)
  # Construir o actualizar el objeto x utilizado en esta etapa.
  x <- str_to_lower(str_squish(x))
  # Construir o actualizar el objeto x utilizado en esta etapa.
  x <- str_replace_all(x, ",", "")
  # Construir o actualizar el objeto numero utilizado en esta etapa.
  numero <- as.numeric(str_extract(x, "[0-9]+(\\.[0-9]+)?"))
  # Construir o actualizar el objeto numero utilizado en esta etapa.
  numero <- ifelse(str_detect(x, "k"), numero * 1000, numero)
  numero
}

leer_frontiers <- function(url) { # Leer una página de Frontiers simulando navegador
  # Construir o actualizar el objeto respuesta utilizado en esta etapa.
  respuesta <- try(
    GET(
      url,
      add_headers(`User-Agent` = user_agent_frontiers),
      timeout(30)
    ),
    silent = TRUE
  )
  
  if (inherits(respuesta, "try-error")) return(NULL)
  if (status_code(respuesta) >= 400) return(NULL)
  
  read_html(respuesta)
}

extraer_citas_crossref <- function(doi) { # Consultar citas desde Crossref
  if (length(doi) == 0 || is.na(doi)) return(NA_real_)
  
  # Construir o actualizar el objeto url_api utilizado en esta etapa.
  url_api <- paste0(
    "https://api.crossref.org/works/",
    URLencode(doi, reserved = TRUE)
  )
  
  # Construir o actualizar el objeto respuesta utilizado en esta etapa.
  respuesta <- try(GET(url_api, timeout(20)), silent = TRUE)
  
  if (inherits(respuesta, "try-error")) return(NA_real_)
  if (status_code(respuesta) >= 400) return(NA_real_)
  
  # Construir o actualizar el objeto contenido utilizado en esta etapa.
  contenido <- content(respuesta, as = "parsed", type = "application/json")
  # Construir o actualizar el objeto citas utilizado en esta etapa.
  citas <- contenido$message$`is-referenced-by-count`
  
  if (is.null(citas)) NA_real_ else as.numeric(citas)
}

extraer_bloque_h2 <- function(html, encabezado) { # Extraer texto asociado a un encabezado h2
  # Construir o actualizar el objeto nodos utilizado en esta etapa.
  nodos <- html |> html_elements("h2, h3, p")
  
  # Construir o actualizar el objeto tabla_nodos utilizado en esta etapa.
  tabla_nodos <- tibble(
    orden = seq_along(nodos),
    etiqueta = html_name(nodos),
    texto = html_text2(nodos) |> limpiar_texto()
  ) |>
    filter(!is.na(texto))
  
  # Construir o actualizar el objeto pos_inicio utilizado en esta etapa.
  pos_inicio <- tabla_nodos |>
    filter(etiqueta == "h2", str_to_lower(texto) == str_to_lower(encabezado)) |>
    slice(1) |>
    pull(orden)
  
  if (length(pos_inicio) == 0) return(NA_character_)
  
  # Construir o actualizar el objeto pos_fin utilizado en esta etapa.
  pos_fin <- tabla_nodos |>
    filter(orden > pos_inicio, etiqueta == "h2") |>
    slice(1) |>
    pull(orden)
  
  if (length(pos_fin) == 0) {
    # Construir o actualizar el objeto pos_fin utilizado en esta etapa.
    pos_fin <- max(tabla_nodos$orden) + 1
  }
  
  # Construir o actualizar el objeto bloque utilizado en esta etapa.
  bloque <- tabla_nodos |>
    filter(orden > pos_inicio, orden < pos_fin) |>
    filter(etiqueta %in% c("h3", "p")) |>
    pull(texto)
  
  tomar_primero(paste(bloque, collapse = " "))
}

extraer_abstract <- function(html) { # Extraer resumen desde el artículo
  # Construir o actualizar el objeto abstract_html utilizado en esta etapa.
  abstract_html <- extraer_bloque_h2(html, "Abstract")
  
  if (!is.na(abstract_html)) {
    return(abstract_html)
  }
  
  html |>
    html_elements("meta[name='description']") |>
    html_attr("content") |>
    tomar_primero()
}

extraer_keywords <- function(html, texto_pagina) { # Extraer palabras clave
  # Construir o actualizar el objeto keywords_meta utilizado en esta etapa.
  keywords_meta <- html |>
    html_elements("meta[name='citation_keywords']") |>
    html_attr("content") |>
    limpiar_texto()
  
  # Construir o actualizar el objeto keywords_meta utilizado en esta etapa.
  keywords_meta <- keywords_meta[!is.na(keywords_meta)]
  
  if (length(keywords_meta) > 0) {
    return(tomar_primero(paste(unique(keywords_meta), collapse = "; ")))
  }
  
  # Construir o actualizar el objeto keywords_summary utilizado en esta etapa.
  keywords_summary <- str_match(
    texto_pagina,
    "Summary\\s+Keywords\\s+(.*?)\\s+Citation"
  )[, 2]
  
  tomar_primero(keywords_summary)
}

extraer_listado_pagina <- function(url_pagina, pagina) { # Extraer enlaces desde una página de listado
  # Construir o actualizar el objeto html utilizado en esta etapa.
  html <- leer_frontiers(url_pagina)
  if (is.null(html)) return(tibble())
  
  # Construir o actualizar el objeto links_nodos utilizado en esta etapa.
  links_nodos <- html |>
    html_elements("a[href*='/journals/bioinformatics/articles/10.3389/fbinf']")
  
  if (length(links_nodos) == 0) return(tibble())
  
  tibble(
    pagina = pagina,
    url = links_nodos |> html_attr("href"),
    texto_tarjeta = links_nodos |> html_text2()
  ) |>
    mutate(
      url = ifelse(str_detect(url, "^http"), url, paste0(base_frontiers, url)),
      url = str_replace(url, "/abstract$", "/full"),
      doi = str_extract(url, "10\\.3389/fbinf\\.[0-9]{4}\\.[0-9]+"),
      estado_tarjeta = str_extract(texto_tarjeta, "Published on|Accepted on"),
      fecha_tarjeta_texto = str_match(
        texto_tarjeta,
        "(Published on|Accepted on)\\s+([0-9]{1,2} [A-Za-z]+ [0-9]{4})"
      )[, 3],
      fecha_tarjeta = suppressWarnings(dmy(fecha_tarjeta_texto, locale = "C")),
      views_text = str_extract(
        texto_tarjeta,
        regex("[0-9][0-9,]*(\\.[0-9]+)?\\s*[kK]?\\s+views", ignore_case = TRUE)
      ),
      downloads = convertir_numero(
        str_remove(views_text, regex("\\s+views", ignore_case = TRUE))
      )
    ) |>
    filter(!is.na(url), !is.na(doi)) |>
    distinct(url, .keep_all = TRUE)
}

extraer_referencias_articulo <- function(html, url_articulo) { # Contar referencias DOI del artículo
  # Construir o actualizar el objeto doi_articulo utilizado en esta etapa.
  doi_articulo <- str_extract(
    url_articulo,
    "10\\.3389/fbinf\\.[0-9]{4}\\.[0-9]+"
  )
  
  html |>
    html_elements("a[href*='doi.org']") |>
    html_attr("href") |>
    tibble(reference_url = _) |>
    mutate(
      reference_doi = str_extract(reference_url, "10\\.[0-9]{4,9}/[^\\s]+"),
      reference_doi = str_remove(reference_doi, "[\\.,;\\)]$")
    ) |>
    filter(!is.na(reference_doi)) |>
    filter(reference_doi != doi_articulo) |>
    distinct(reference_doi, .keep_all = TRUE)
}

extraer_articulo_reciente <- function(url_articulo, listado_reciente) { # Extraer metadatos de un artículo
  # Construir o actualizar el objeto html utilizado en esta etapa.
  html <- leer_frontiers(url_articulo)
  
  if (is.null(html)) {
    return(tibble(extraction_status = "error_html", url = url_articulo))
  }
  
  # Construir o actualizar el objeto texto_pagina utilizado en esta etapa.
  texto_pagina <- html |> html_text2() |> limpiar_texto()
  
  # Construir o actualizar el objeto doi_url utilizado en esta etapa.
  doi_url <- str_extract(
    url_articulo,
    "10\\.3389/fbinf\\.[0-9]{4}\\.[0-9]+"
  )
  
  # Construir o actualizar el objeto title utilizado en esta etapa.
  title <- html |>
    html_elements("meta[name='citation_title']") |>
    html_attr("content") |>
    tomar_primero()
  
  if (is.na(title)) {
    # Construir o actualizar el objeto title utilizado en esta etapa.
    title <- html |> html_element("h1") |> html_text(trim = TRUE) |> tomar_primero()
  }
  
  # Construir o actualizar el objeto article_type utilizado en esta etapa.
  article_type <- html |>
    html_elements("h2") |>
    html_text(trim = TRUE) |>
    tomar_primero()
  
  # Construir o actualizar el objeto doi utilizado en esta etapa.
  doi <- html |>
    html_elements("meta[name='citation_doi']") |>
    html_attr("content") |>
    tomar_primero()
  
  if (is.na(doi)) doi <- doi_url
  
  # Construir o actualizar el objeto fecha_texto utilizado en esta etapa.
  fecha_texto <- html |>
    html_elements("meta[name='citation_publication_date']") |>
    html_attr("content") |>
    tomar_primero()
  
  # Construir o actualizar el objeto publication_date utilizado en esta etapa.
  publication_date <- suppressWarnings(ymd(fecha_texto))
  
  if (is.na(publication_date)) {
    # Construir o actualizar el objeto fecha_texto utilizado en esta etapa.
    fecha_texto <- str_match(
      texto_pagina,
      "Published\\s+([0-9]{2} [A-Za-z]+ [0-9]{4})"
    )[, 2]
    # Construir o actualizar el objeto publication_date utilizado en esta etapa.
    publication_date <- suppressWarnings(dmy(fecha_texto, locale = "C"))
  }
  
  # Construir o actualizar el objeto volume_text utilizado en esta etapa.
  volume_text <- str_match(
    texto_pagina,
    "Volume\\s+([0-9]+\\s+-\\s+[0-9]{4})"
  )[, 2] |> tomar_primero()
  
  # Construir o actualizar el objeto volume_number utilizado en esta etapa.
  volume_number <- as.numeric(str_extract(volume_text, "^[0-9]+"))
  # Construir o actualizar el objeto volume_year utilizado en esta etapa.
  volume_year <- as.numeric(str_extract(volume_text, "[0-9]{4}$"))
  
  # Construir o actualizar el objeto no_publicado utilizado en esta etapa.
  no_publicado <- str_detect(
    texto_pagina,
    "final, formatted version of the article will be published soon|Notify me on publication|Accepted on"
  )
  
  # Construir o actualizar el objeto publication_status utilizado en esta etapa.
  publication_status <- ifelse(no_publicado | is.na(publication_date), "not_published", "published")
  
  # Construir o actualizar el objeto autores utilizado en esta etapa.
  autores <- html |>
    html_elements("meta[name='citation_author']") |>
    html_attr("content") |>
    limpiar_texto()
  
  # Construir o actualizar el objeto autores utilizado en esta etapa.
  autores <- autores[!is.na(autores)]
  # Construir o actualizar el objeto authors_raw utilizado en esta etapa.
  authors_raw <- paste(autores, collapse = "; ") |> limpiar_texto()
  # Construir o actualizar el objeto n_authors utilizado en esta etapa.
  n_authors <- length(autores)
  if (n_authors == 0) n_authors <- NA_integer_
  
  # Construir o actualizar el objeto abstract utilizado en esta etapa.
  abstract <- extraer_abstract(html)
  # Construir o actualizar el objeto keywords utilizado en esta etapa.
  keywords <- extraer_keywords(html, texto_pagina)
  
  # Construir o actualizar el objeto section utilizado en esta etapa.
  section <- str_match(
    texto_pagina,
    "Sec\\. ([A-Za-z0-9 ,\\-&]+?) Volume"
  )[, 2] |> limpiar_texto()
  
  # Construir o actualizar el objeto citations utilizado en esta etapa.
  citations <- ifelse(
    publication_status == "published",
    extraer_citas_crossref(doi),
    NA_real_
  )
  
  # Construir o actualizar el objeto doi_articulo_actual utilizado en esta etapa.
  doi_articulo_actual <- doi
  
  # Construir o actualizar el objeto downloads utilizado en esta etapa.
  downloads <- listado_reciente |>
    filter(.data$doi == doi_articulo_actual) |>
    pull(downloads) |>
    tomar_primero()
  
  # Construir o actualizar el objeto downloads utilizado en esta etapa.
  downloads <- as.numeric(downloads)
  # Construir o actualizar el objeto referencias utilizado en esta etapa.
  referencias <- extraer_referencias_articulo(html, url_articulo)
  
  tibble(
    extraction_status = "ok",
    paper_id = NA_integer_,
    journal_name = "Frontiers in Bioinformatics",
    article_type = article_type,
    publication_status = publication_status,
    volume_text = volume_text,
    volume_number = volume_number,
    volume_year = volume_year,
    section = section,
    title = title,
    publication_date = publication_date,
    year = year(publication_date),
    doi = doi,
    url = url_articulo,
    abstract = abstract,
    authors_raw = authors_raw,
    n_authors = n_authors,
    citations = citations,
    citation_source = "Crossref",
    downloads = downloads,
    metric_used_as_downloads = "views_from_recent_article_listing",
    n_references = nrow(referencias),
    keywords = keywords
  )
}

#------------------------------------------------------------
# Clasificación temática simple
#------------------------------------------------------------

diccionario_temas <- tibble::tribble( # Diccionario de palabras para clasificación aproximada de cada artículo scrapeado
  ~topic_label,        ~prioridad, ~patron,
  "IA Generativa",           1,    "generative ai|generative model(s)?|diffusion model(s)?|large language model(s)?|\\bllm(s)?\\b|chatgpt|\\bgpt[- ]?[0-9]*\\b|foundation model(s)?",
  "Machine Learning",        2,    "machine learning|deep learning|neural network(s)?|graph neural network(s)?|random forest|support vector machine|reinforcement learning|supervised learning|unsupervised learning|representation learning|conformal prediction|mask r-cnn|\\bqsar\\b|automl",
  "Estadistica",             3,    "bayesian|survival analysis|regression|principal component analysis|\\bpca\\b|clustering|statistical analysis|statistical model(s)?"
)

clasificar_tema_simple <- function(title, keywords, abstract) { # Clasificar un artículo con reglas del Taller 1
  # Construir o actualizar el objeto texto_title utilizado en esta etapa.
  texto_title <- str_to_lower(coalesce(title, ""))
  # Construir o actualizar el objeto texto_keywords utilizado en esta etapa.
  texto_keywords <- str_to_lower(coalesce(keywords, ""))
  # Construir o actualizar el objeto texto_abstract utilizado en esta etapa.
  texto_abstract <- str_to_lower(coalesce(abstract, ""))
  
  # Construir o actualizar el objeto candidatos utilizado en esta etapa.
  candidatos <- diccionario_temas |>
    mutate(
      evidencia_title = str_extract(texto_title, regex(patron, ignore_case = TRUE)),
      evidencia_keywords = str_extract(texto_keywords, regex(patron, ignore_case = TRUE)),
      evidencia_abstract = str_extract(texto_abstract, regex(patron, ignore_case = TRUE)),
      evidencia_fuerte = !is.na(evidencia_title) | !is.na(evidencia_keywords),
      evidencia_debil = is.na(evidencia_title) & is.na(evidencia_keywords) & !is.na(evidencia_abstract),
      topic_source = case_when(
        !is.na(evidencia_title) ~ "title",
        !is.na(evidencia_keywords) ~ "keywords",
        !is.na(evidencia_abstract) ~ "abstract",
        TRUE ~ NA_character_
      ),
      topic_evidence = case_when(
        !is.na(evidencia_title) ~ evidencia_title,
        !is.na(evidencia_keywords) ~ evidencia_keywords,
        !is.na(evidencia_abstract) ~ evidencia_abstract,
        TRUE ~ NA_character_
      ),
      topic_score = case_when(
        evidencia_fuerte ~ 3,
        evidencia_debil ~ 1,
        TRUE ~ 0
      )
    ) |>
    filter(topic_score > 0) |>
    arrange(desc(topic_score), prioridad)
  
  if (nrow(candidatos) == 0) {
    return(tibble(
      topic_label = "Otros",
      topic_source = NA_character_,
      topic_evidence = NA_character_,
      topic_confidence = "baja"
    ))
  }
  
  # Construir o actualizar el objeto mejor utilizado en esta etapa.
  mejor <- candidatos |> slice(1)
  
  tibble(
    topic_label = ifelse(mejor$topic_score == 3, mejor$topic_label, "Otros"),
    topic_source = mejor$topic_source,
    topic_evidence = mejor$topic_evidence,
    topic_confidence = ifelse(mejor$topic_score == 3, "alta", "baja")
  )
}

#------------------------------------------------------------
# Función principal
#------------------------------------------------------------

completar_estructura_papers <- function(datos, columnas_papers) { # Ajustar columnas a la estructura de papers
  if (is.null(columnas_papers)) return(datos)
  
  if (nrow(datos) == 0) {
    # Construir o actualizar el objeto salida utilizado en esta etapa.
    salida <- as_tibble(setNames(rep(list(NA), length(columnas_papers)), columnas_papers))[0, ]
    return(salida)
  }
  
  # Construir o actualizar el objeto faltantes utilizado en esta etapa.
  faltantes <- setdiff(columnas_papers, names(datos))
  
  # Recorrer de forma reproducible los elementos definidos.
  for (columna in faltantes) {
    datos[[columna]] <- NA
  }
  
  datos |> select(any_of(columnas_papers))
}

# Orquestar listado, filtrado y extracción.
scrapear_articulos_recientes <- function(
    fecha_inicio = Sys.Date() - 30,
    fecha_fin = Sys.Date(),
    max_paginas = 3,
    limite_articulos = 10,
    columnas_papers = NULL) { # Buscar artículos recientes y devolver estructura tipo papers
  
  # Construir o actualizar el objeto fecha_inicio utilizado en esta etapa.
  fecha_inicio <- as.Date(fecha_inicio)
  # Construir o actualizar el objeto fecha_fin utilizado en esta etapa.
  fecha_fin <- as.Date(fecha_fin)
  
  # Construir o actualizar el objeto rango_frontiers utilizado en esta etapa.
  rango_frontiers <- paste0(
    format(fecha_inicio, "%d%%2F%m%%2F%Y"),
    "-",
    format(fecha_fin, "%d%%2F%m%%2F%Y")
  )
  
  # Construir o actualizar el objeto url_listado_reciente utilizado en esta etapa.
  url_listado_reciente <- paste0(url_listado, "?publication-date=", rango_frontiers)
  
  # Construir o actualizar el objeto paginas utilizado en esta etapa.
  paginas <- tibble(
    pagina = 1:max_paginas,
    url_pagina = ifelse(
      pagina == 1,
      url_listado_reciente,
      paste0(url_listado_reciente, "&page=", pagina)
    )
  )
  
  # Construir o actualizar el objeto listado_reciente utilizado en esta etapa.
  listado_reciente <- paginas |>
    mutate(datos = map2(url_pagina, pagina, extraer_listado_pagina)) |>
    pull(datos) |>
    bind_rows()
  
  if (nrow(listado_reciente) == 0) {
    return(completar_estructura_papers(tibble(), columnas_papers))
  }
  
  # Construir o actualizar el objeto listado_reciente utilizado en esta etapa.
  listado_reciente <- listado_reciente |>
    filter(estado_tarjeta == "Published on") |>
    filter(fecha_tarjeta >= fecha_inicio, fecha_tarjeta <= fecha_fin) |>
    distinct(doi, .keep_all = TRUE) |>
    arrange(desc(fecha_tarjeta))
  
  if (!is.null(limite_articulos)) {
    # Construir o actualizar el objeto listado_reciente utilizado en esta etapa.
    listado_reciente <- listado_reciente |> slice_head(n = limite_articulos)
  }
  
  if (nrow(listado_reciente) == 0) {
    return(completar_estructura_papers(tibble(), columnas_papers))
  }
  
  # Construir o actualizar el objeto articulos_raw utilizado en esta etapa.
  articulos_raw <- listado_reciente |>
    pull(url) |>
    map_dfr(~ extraer_articulo_reciente(.x, listado_reciente)) |>
    clean_names()
  
  if (!"extraction_status" %in% names(articulos_raw)) {
    # Construir o actualizar el objeto articulos_raw utilizado en esta etapa.
    articulos_raw <- articulos_raw |> mutate(extraction_status = "ok")
  }
  
  if (!"publication_status" %in% names(articulos_raw)) {
    return(completar_estructura_papers(tibble(), columnas_papers))
  }
  
  # Construir o actualizar el objeto articulos utilizado en esta etapa.
  articulos <- articulos_raw |>
    filter(extraction_status == "ok") |>
    filter(publication_status == "published") |>
    arrange(desc(publication_date), title)
  
  if (nrow(articulos) == 0) {
    return(completar_estructura_papers(tibble(), columnas_papers))
  }
  
  # Construir o actualizar el objeto temas utilizado en esta etapa.
  temas <- pmap_dfr(
    list(articulos$title, articulos$keywords, articulos$abstract),
    clasificar_tema_simple
  )
  
  # Construir o actualizar el objeto articulos utilizado en esta etapa.
  articulos <- articulos |>
    select(-any_of(c(
      "topic_label",
      "topic_source",
      "topic_evidence",
      "topic_confidence"
    ))) |>
    bind_cols(temas) |>
    mutate(
      manual_topic_label = NA_character_,
      manual_topic_source = NA_character_,
      manual_topic_evidence = NA_character_,
      manual_topic_status = NA_character_
    )
  
  completar_estructura_papers(articulos, columnas_papers)
}


#------------------------------------------------------------
# Función general de clasificación temática
#------------------------------------------------------------

clasificar_tema <- function(datos) { # Clasificar artículos nuevos con el criterio del Taller 1
  
  if (nrow(datos) == 0) {
    return(datos)
  }
  
  # Construir o actualizar el objeto temas utilizado en esta etapa.
  temas <- pmap_dfr(
    list(
      datos$title,
      datos$keywords,
      datos$abstract
    ),
    clasificar_tema_simple
  )
  
  datos |>
    select(
      -any_of(c(
        "topic_label",
        "topic_source",
        "topic_evidence",
        "topic_confidence"
      ))
    ) |>
    bind_cols(temas) |>
    mutate(
      topic_label = case_when(
        is.na(topic_label) | topic_label == "" ~ "Otros",
        TRUE ~ topic_label
      ),
      topic_source = ifelse(is.na(topic_source), NA_character_, topic_source),
      topic_evidence = ifelse(is.na(topic_evidence), NA_character_, topic_evidence),
      topic_confidence = case_when(
        is.na(topic_confidence) | topic_confidence == "" ~ "baja",
        TRUE ~ topic_confidence
      )
    )
}

