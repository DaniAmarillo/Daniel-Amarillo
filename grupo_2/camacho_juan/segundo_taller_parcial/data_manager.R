# ==================================================
# Taller 2 - Minería de Datos
# Conexión, consultas, filtros y actualización
# ==================================================

library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(DT)
library(highcharter)
library(rvest)
library(httr)
library(visNetwork)

# ==================================================
# CONEXIÓN A SQLITE
# ==================================================

DB_PATH <- "frontiers_big_data_2025.sqlite"

get_con <- function() {
  dbConnect(RSQLite::SQLite(), DB_PATH)
}

# ==================================================
# CONSULTA PRINCIPAL DE ARTÍCULOS
# ==================================================

get_papers_filtrados <- function(
    fecha_inicio = "2025-01-01",
    fecha_fin = Sys.Date(),
    topic = "Todos",
    autor = "",
    doi_busq = "",
    keyword = ""
) {
  
  con <- get_con()
  
  df <- dbGetQuery(con, "
    SELECT
      paper_id,
      journal_name,
      title,
      publication_date,
      year,
      doi,
      url,
      abstract,
      authors_raw,
      n_authors,
      citations,
      downloads,
      n_references,
      topic_label
    FROM papers
  ")
  
  dbDisconnect(con)
  
  df <- df %>%
    mutate(
      publication_date_clean = suppressWarnings(dmy(publication_date)),
      year = as.numeric(year),
      n_authors = as.numeric(n_authors),
      citations = as.numeric(citations),
      downloads = as.numeric(downloads),
      n_references = as.numeric(n_references)
    ) %>%
    filter(
      publication_date_clean >= as.Date(fecha_inicio),
      publication_date_clean <= as.Date(fecha_fin)
    )
  
  if (!is.null(topic) && topic != "Todos") {
    df <- df %>% filter(topic_label == topic)
  }
  
  if (!is.null(autor) && autor != "") {
    df <- df %>% filter(str_detect(str_to_lower(authors_raw), str_to_lower(autor)))
  }
  
  if (!is.null(doi_busq) && doi_busq != "") {
    df <- df %>% filter(str_detect(str_to_lower(doi), str_to_lower(doi_busq)))
  }
  
  if (!is.null(keyword) && keyword != "") {
    df <- df %>%
      filter(
        str_detect(str_to_lower(title), str_to_lower(keyword)) |
          str_detect(str_to_lower(abstract), str_to_lower(keyword))
      )
  }
  
  df
}

# ==================================================
# OPCIONES DEL FILTRO DE CATEGORÍAS
# ==================================================

get_topics <- function() {
  
  con <- get_con()
  
  topics <- dbGetQuery(con, "
    SELECT DISTINCT topic_label
    FROM papers
    WHERE topic_label IS NOT NULL
    ORDER BY topic_label
  ")
  
  dbDisconnect(con)
  
  c("Todos", topics$topic_label)
}

# ==================================================
# ÚLTIMOS 5 ARTÍCULOS GUARDADOS
# ==================================================

get_ultimos_cinco_papers <- function() {
  
  con <- get_con()
  
  df <- dbGetQuery(con, "
    SELECT
      paper_id,
      journal_name,
      title,
      publication_date,
      year,
      doi,
      url,
      abstract,
      authors_raw,
      n_authors,
      citations,
      downloads,
      n_references,
      topic_label
    FROM papers
  ")
  
  dbDisconnect(con)
  
  df %>%
    mutate(publication_date_clean = suppressWarnings(dmy(publication_date))) %>%
    arrange(desc(publication_date_clean)) %>%
    slice_head(n = 5) %>%
    select(-publication_date_clean)
}

# ==================================================
# LIMPIEZA DE AUTORES
# ==================================================

limpiar_nombre_autor <- function(nombre) {
  
  nombre <- nombre %>%
    str_remove_all("\\[[^\\]]*\\]") %>%
    str_remove_all("\\([^\\)]*\\)") %>%
    str_remove_all("[\\[\\]\\{\\}\\*]") %>%
    str_remove_all("\\s+[0-9]+(?=,|$)") %>%
    str_remove_all("[0-9]+$") %>%
    str_remove_all("\\b[0-9]+\\b") %>%
    str_replace_all(",$", "") %>%
    str_replace_all("\\s+", " ") %>%
    str_squish()
  
  str_to_title(str_to_lower(nombre))
}

limpiar_autores_frontiers <- function(authors) {
  
  authors <- authors %>%
    str_split(",|;") %>%
    unlist() %>%
    limpiar_nombre_autor()
  
  authors <- authors[authors != ""]
  authors <- authors[!str_detect(authors, "^[A-Z]{1,4}$")]
  unique(authors)
}

# ==================================================
# CLASIFICACIÓN
# ==================================================

clasificar_tema_taller1 <- function(title, abstract = "") {
  
  texto <- str_to_lower(paste(title, abstract))
  
  case_when(
    str_detect(texto, "generative|chatgpt|gpt|large language model|llm|foundation model") ~ "IA Generativa",
    str_detect(texto, "machine learning|deep learning|neural network|classification|prediction|model|algorithm|artificial intelligence|ai|random forest|gradient|supervised|unsupervised") ~ "Machine Learning",
    str_detect(texto, "statistical|statistics|regression|bayesian|probability|time series|correlation|variance|forecast") ~ "Estadística",
    TRUE ~ "Otros"
  )
}

# ==================================================
# SCRAPING FRONTIERS BIG DATA
# ==================================================

buscar_articulos_nuevos <- function(anio_inicio = 2026, max_paginas = 5) {
  
  anio_actual <- lubridate::year(Sys.Date())
  anios_busqueda <- anio_inicio:anio_actual
  
  url_base <- "https://www.frontiersin.org/journals/big-data/articles"
  links_articulos <- c()
  registros <- list()
  
  # 1. Buscar links reales de artículos
  for (anio_busqueda in anios_busqueda) {
    
    for (pagina_num in 1:max_paginas) {
      
      url_pagina <- paste0(
        url_base,
        "?publication-date=01%2F01%2F", anio_busqueda,
        "-31%2F12%2F", anio_busqueda,
        "&page=", pagina_num
      )
      
      pagina <- tryCatch(read_html(url_pagina), error = function(e) NULL)
      if (is.null(pagina)) next
      
      links <- pagina %>%
        html_elements("a") %>%
        html_attr("href") %>%
        na.omit() %>%
        unique()
      
      links <- links[str_detect(links, "/journals/big-data/articles/")]
      
      links <- ifelse(
        str_starts(links, "http"),
        links,
        paste0("https://www.frontiersin.org", links)
      )
      
      links <- str_replace(links, "\\?.*$", "")
      links <- unique(links)
      
      links_articulos <- c(links_articulos, links)
      
      Sys.sleep(0.4)
    }
  }
  
  links_articulos <- unique(links_articulos)
  
  if (length(links_articulos) == 0) {
    ultimos <- get_ultimos_cinco_papers()
    return(list(nuevos = 0, df_nuevos = ultimos, verificados = ultimos$doi))
  }
  
  # 2. Entrar a cada artículo
  for (link in links_articulos) {
    
    articulo <- tryCatch(read_html(link), error = function(e) NULL)
    if (is.null(articulo)) next
    
    texto <- articulo %>%
      html_text2() %>%
      str_squish()
    
    doi <- str_extract(link, "10\\.3389/fdata\\.[0-9]{4}\\.[0-9]+")
    
    if (is.na(doi)) {
      doi <- str_extract(texto, "10\\.3389/fdata\\.[0-9]{4}\\.[0-9]+")
    }
    
    if (is.na(doi)) next
    
    # IMPORTANTE:
    # El año válido será el de publicación, no el año del DOI.
    fecha <- str_extract(texto, "[0-9]{2} [A-Za-z]+ [0-9]{4}")
    fecha_limpia <- suppressWarnings(lubridate::dmy(fecha))
    
    if (is.na(fecha_limpia)) next
    
    anio_publicacion <- lubridate::year(fecha_limpia)
    
    if (anio_publicacion < anio_inicio) next
    
    title <- articulo %>%
      html_element("h1") %>%
      html_text2() %>%
      str_squish()
    
    if (length(title) == 0 || is.na(title) || title == "") {
      title <- texto %>%
        str_extract(
          "(?<=Original Research ).*?(?= in Machine Learning and Artificial Intelligence| in Medicine and Public Health| in Cybersecurity and Privacy| in Data Analytics for Social Impact| in Data Science| in Data Mining and Management| in Data-driven Climate Sciences| in Big Data Networks| in Recommender Systems| in Big Data and AI in High Energy Physics| Published on| Accepted on)"
        ) %>%
        str_squish()
    }
    
    if (
      is.na(title) ||
      title == "" ||
      str_detect(title, "Type at least|No matches|All types|All sections|Clear all Filters")
    ) {
      next
    }
    
    authors <- articulo %>%
      html_elements("a[href*='/people/']") %>%
      html_text2() %>%
      str_squish()
    
    authors <- limpiar_autores_frontiers(authors)
    
    if (length(authors) == 0) {
      authors_raw <- "Autores no disponibles"
      n_authors <- 0
    } else {
      authors_raw <- paste(authors, collapse = ", ")
      n_authors <- length(authors)
    }
    
    abstract <- texto %>%
      str_extract("(?i)(?<=Abstract ).*?(?= Introduction| Keywords| Citation|$)") %>%
      str_squish()
    
    if (length(abstract) == 0 || is.na(abstract)) {
      abstract <- ""
    }
    
    downloads <- str_extract(texto, "[0-9,]+ views")
    downloads <- as.numeric(str_remove_all(str_remove(downloads, " views"), ","))
    
    if (is.na(downloads)) {
      downloads <- 0
    }
    
    n_references <- str_count(texto, "\\([0-9]{4}\\)")
    
    registros[[length(registros) + 1]] <- data.frame(
      journal_name = "Frontiers in Big Data",
      title = title,
      publication_date = format(fecha_limpia, "%d %B %Y"),
      year = anio_publicacion,
      doi = doi,
      url = link,
      abstract = abstract,
      authors_raw = authors_raw,
      n_authors = n_authors,
      citations = 0,
      downloads = downloads,
      n_references = n_references,
      topic_label = clasificar_tema_taller1(title, abstract),
      stringsAsFactors = FALSE
    )
    
    Sys.sleep(0.4)
  }
  
  if (length(registros) == 0) {
    ultimos <- get_ultimos_cinco_papers()
    return(list(nuevos = 0, df_nuevos = ultimos, verificados = ultimos$doi))
  }
  
  df_scrapeado <- bind_rows(registros) %>%
    distinct(doi, .keep_all = TRUE)
  
  con <- get_con()
  dois_existentes <- dbGetQuery(con, "SELECT doi FROM papers")$doi
  
  df_nuevos <- df_scrapeado %>%
    filter(!doi %in% dois_existentes)
  
  if (nrow(df_nuevos) > 0) {
    dbWriteTable(con, "papers", df_nuevos, append = TRUE, row.names = FALSE)
  }
  
  dbDisconnect(con)
  
  if (nrow(df_nuevos) == 0) {
    return(list(
      nuevos = 0,
      df_nuevos = df_scrapeado %>%
        mutate(fecha_aux = suppressWarnings(lubridate::dmy(publication_date))) %>%
        arrange(desc(fecha_aux)) %>%
        slice_head(n = 5) %>%
        select(-fecha_aux),
      verificados = df_scrapeado$doi
    ))
  }
  
  list(
    nuevos = nrow(df_nuevos),
    df_nuevos = df_nuevos,
    verificados = df_nuevos$doi
  )
}

# ==================================================
# RED DE AUTORES
# ==================================================

get_red_autores <- function(top_n = 30) {
  
  con <- get_con()
  
  df <- dbGetQuery(con, "
    SELECT
      p.paper_id,
      a.author_name
    FROM paper_authors AS pa
    INNER JOIN authors AS a
      ON pa.author_id = a.author_id
    INNER JOIN papers AS p
      ON pa.paper_id = p.paper_id
  ")
  
  dbDisconnect(con)
  
  autores_top <- df %>%
    count(author_name, sort = TRUE) %>%
    slice_head(n = top_n)
  
  df_filtrado <- df %>%
    filter(author_name %in% autores_top$author_name)
  
  edges <- df_filtrado %>%
    inner_join(df_filtrado, by = "paper_id") %>%
    filter(author_name.x < author_name.y) %>%
    count(author_name.x, author_name.y, name = "weight")
  
  nodes <- autores_top %>%
    transmute(
      id = author_name,
      label = author_name,
      value = n,
      title = paste0(author_name, "<br>Artículos: ", n)
    )
  
  edges <- edges %>%
    transmute(
      from = author_name.x,
      to = author_name.y,
      value = weight,
      title = paste0("Colaboraciones: ", weight)
    )
  
  list(nodes = nodes, edges = edges)
}