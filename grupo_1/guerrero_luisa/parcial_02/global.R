# =========================================================
# GLOBAL
# BDCC Research Dashboard
# Taller 2 — Minería de Datos
# Shiny + SQLite + Highcharter + Web Scraping (CrossRef)
# =========================================================

library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(DT)
library(highcharter)
library(lubridate)
library(stringr)
library(purrr)
library(tidyr)
library(httr)
library(jsonlite)

# ---------------------------------------------------------
# Conexión y carga de datos
# ---------------------------------------------------------

db_path <- "bdcc_2025.sqlite"

if (!file.exists(db_path)) {
  stop("No se encontró la base de datos bdcc_2025.sqlite en la carpeta del proyecto.")
}

cargar_papers <- function() {
  con <- dbConnect(SQLite(), db_path)
  datos <- dbReadTable(con, "papers")
  dbDisconnect(con)
  
  datos %>%
    mutate(
      publication_date = as.Date(publication_date),
      year             = as.numeric(year),
      n_authors        = as.numeric(n_authors),
      citations        = as.numeric(citations),
      downloads        = as.numeric(downloads),
      n_references     = as.numeric(n_references),
      topic_label      = ifelse(is.na(topic_label) | topic_label == "", "Sin categoría", topic_label),
      authors_raw      = ifelse(is.na(authors_raw), "", authors_raw),
      doi              = ifelse(is.na(doi), "", doi),
      abstract         = ifelse(is.na(abstract), "", abstract),
      title            = ifelse(is.na(title), "", title)
    )
}

papers_base <- cargar_papers()

# ---------------------------------------------------------
# Autores únicos para el selector
# ---------------------------------------------------------

autores_disponibles <- papers_base$authors_raw %>%
  str_split(";") %>%
  unlist() %>%
  str_trim() %>%
  discard(~ .x == "" | is.na(.x)) %>%
  unique() %>%
  sort()

# ---------------------------------------------------------
# Paleta base
# ---------------------------------------------------------

paleta_bdcc <- c(
  "#0a0a0a",
  "#e10600",
  "#6f6f6f",
  "#b8b8b8",
  "#2b2b2b",
  "#ffffff"
)

# ---------------------------------------------------------
# Helpers de scraping con CrossRef
# ---------------------------------------------------------

ISSN_BDCC <- "2504-2289"

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

clasificar_tema <- function(titulo, resumen) {
  texto <- paste(titulo, resumen) %>% 
    str_to_lower()
  
  case_when(
    str_detect(
      texto,
      "generative|genai|large language model|llm|chatgpt|prompt|diffusion|gpt"
    ) ~ "IA Generativa",
    
    str_detect(
      texto,
      "machine learning|deep learning|neural network|random forest|svm|classification|prediction|classifier|convolutional|transformer"
    ) ~ "Machine Learning",
    
    str_detect(
      texto,
      "statistical|statistics|regression|bayesian|probability|variance|correlation|hypothesis"
    ) ~ "Estadística",
    
    TRUE ~ "Otros"
  )
}

limpiar_texto <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  
  str_squish(x[[1]])
}

extraer_autores <- function(item) {
  if (is.null(item$author)) {
    return(NA_character_)
  }
  
  autores <- map_chr(item$author, function(a) {
    given  <- ifelse(is.null(a$given),  "", a$given)
    family <- ifelse(is.null(a$family), "", a$family)
    
    str_squish(paste(given, family))
  })
  
  paste(autores, collapse = "; ")
}

extraer_fecha <- function(item) {
  tryCatch({
    partes <- item$published$`date-parts`[[1]]
    paste(partes, collapse = "-")
  }, error = function(e) {
    NA_character_
  })
}

scraping_crossref <- function(anio) {
  
  url <- paste0(
    "https://api.crossref.org/works?",
    "filter=issn:", ISSN_BDCC,
    ",from-pub-date:", anio, "-01-01",
    ",until-pub-date:", anio, "-12-31",
    ",type:journal-article",
    "&rows=200&mailto=taller2@unal.edu.co"
  )
  
  resp <- tryCatch(
    httr::GET(
      url,
      httr::timeout(45),
      httr::add_headers(
        `User-Agent` = "UNAL-Taller2-MineriadeDatos"
      )
    ),
    error = function(e) NULL
  )
  
  if (is.null(resp) || httr::status_code(resp) != 200) {
    return(
      list(
        ok = FALSE,
        msg = paste(
          "Error HTTP:",
          if (is.null(resp)) "sin respuesta" else httr::status_code(resp)
        ),
        data = NULL
      )
    )
  }
  
  raw <- httr::content(resp, "text", encoding = "UTF-8")
  
  data <- tryCatch(
    jsonlite::fromJSON(raw, simplifyVector = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(data) || is.null(data$message$items)) {
    return(
      list(
        ok = FALSE,
        msg = "No se pudo parsear la respuesta de CrossRef.",
        data = NULL
      )
    )
  }
  
  items <- data$message$items
  
  if (length(items) == 0) {
    return(
      list(
        ok = TRUE,
        msg = "CrossRef no devolvió artículos para ese año.",
        data = data.frame()
      )
    )
  }
  
  rows <- map_dfr(items, function(it) {
    
    titulo  <- limpiar_texto(it$title)
    resumen <- limpiar_texto(it$abstract)
    autores <- extraer_autores(it)
    fecha_r <- extraer_fecha(it)
    doi_val <- limpiar_texto(it$DOI)
    
    citas <- as.numeric(it$`is-referenced-by-count` %||% NA)
    n_refs <- as.numeric(length(it$reference %||% list()))
    n_aut <- length(it$author %||% list())
    
    fecha_ok <- tryCatch({
      partes <- strsplit(fecha_r, "-")[[1]]
      
      year_v  <- as.integer(partes[1])
      month_v <- if (length(partes) >= 2) as.integer(partes[2]) else 1L
      day_v   <- if (length(partes) >= 3) as.integer(partes[3]) else 1L
      
      as.character(
        as.Date(sprintf("%04d-%02d-%02d", year_v, month_v, day_v))
      )
    }, error = function(e) {
      NA_character_
    })
    
    topic <- clasificar_tema(titulo %||% "", resumen %||% "")
    
    data.frame(
      doi              = doi_val,
      title            = titulo,
      authors_raw      = autores,
      publication_date = fecha_ok,
      year             = as.numeric(anio),
      n_authors        = as.numeric(n_aut),
      citations        = citas,
      downloads        = NA_real_,
      n_references     = as.numeric(n_refs),
      abstract         = resumen %||% "",
      topic_label      = topic,
      stringsAsFactors = FALSE
    )
  })
  
  list(
    ok = TRUE,
    msg = paste("CrossRef devolvió", nrow(rows), "artículos para", anio),
    data = rows
  )
}