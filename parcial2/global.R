library(shiny)
library(shinydashboard)
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(highcharter)
library(DT)
library(httr)
library(rvest)
library(fresh)
library(shinyWidgets)
library(shinycssloaders)
library(jsonlite)

tema_claro <- create_theme(
  adminlte_color(
    light_blue = "#2563EB", navy = "#1E40AF", blue = "#3B82F6",
    green      = "#16A34A", red  = "#DC2626", yellow = "#D97706",
    fuchsia    = "#7C3AED", black = "#111827"
  ),
  adminlte_sidebar(
    dark_bg           = "#F1F5F9", dark_hover_bg    = "#DBEAFE",
    dark_color        = "#1E293B", dark_hover_color = "#1E40AF",
    dark_submenu_bg   = "#E2E8F0", dark_submenu_color = "#334155"
  ),
  adminlte_global(
    content_bg = "#F8FAFC", box_bg = "#FFFFFF", info_box_bg = "#FFFFFF"
  )
)

# ── 3. Base de datos ----------------------------------------------------------
conectar_db <- function(ruta = "annual_reviews_2025.db") {
  dbConnect(SQLite(), ruta)
}

leer_papers <- function(con) {
  tryCatch({
    if (!dbExistsTable(con, "papers")) return(tibble())
    dbReadTable(con, "papers")
  }, error = function(e) tibble())
}

limpiar_texto <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x %>% str_replace_all("\\s+", " ") %>% str_trim()
}

scrapear_pagina <- function(url, anio_pagina = "2025") {
  tryCatch({
    resp     <- GET(url,
                    add_headers(`User-Agent` = "Mozilla/5.0",
                                `Accept-Language` = "en"),
                    timeout(30))
    html_doc <- read_html(resp)
    nodos    <- html_doc %>% html_nodes(".articleInToc")
    if (length(nodos) == 0) return(tibble())

    map_dfr(nodos, function(nodo) {
      doi_val <- nodo %>%
        html_node(".articleSourceTag a") %>%
        html_attr("href") %>%
        str_extract("10\\.\\d{4,}/\\S+")

      url_art <- nodo %>%
        html_node(".articleTitle a, .js-articleTitle a") %>%
        html_attr("href")

      url_completa <- if (!is.na(url_art) && !str_starts(url_art, "http"))
        paste0("https://www.annualreviews.org", url_art)
      else
        url_art

      autores_txt <- nodo %>%
        html_nodes(".meta-value.authors .author-list__item a") %>%
        html_text() %>%
        limpiar_texto() %>%
        paste(collapse = "; ")

      tibble(
        titulo            = nodo %>%
          html_node(".articleTitle a, .js-articleTitle a") %>%
          html_text() %>% limpiar_texto(),
        fecha_publicacion = anio_pagina,
        doi               = doi_val,
        url               = url_completa,
        autores           = autores_txt,
        resumen           = nodo %>%
          html_node(".js-desc p") %>%
          html_text() %>% limpiar_texto()
      )
    })
  }, error = function(e) tibble())
}

clasificar <- function(titulo, resumen) {
  tx <- tolower(paste(titulo %||% "", resumen %||% ""))
  if      (str_detect(tx, "machine learning|neural network|deep learning"))
    "Machine Learning"
  else if (str_detect(tx, "generative ai|ia generativa|llm|gpt|large language"))
    "IA Generativa"
  else if (str_detect(tx, "statistics|statistical|inference|probability|econometrics|causal"))
    "Estadística"
  else
    "Otros"
}

obtener_citas_crossref <- function(doi) {
  if (is.na(doi) || doi == "") return(NA_integer_)
  tryCatch({
    url  <- paste0("https://api.crossref.org/works/",
                   utils::URLencode(doi, reserved = TRUE))
    resp <- GET(url,
                add_headers(`User-Agent` = "ShinyDashboard/1.0"),
                timeout(10))
    if (http_error(resp)) return(NA_integer_)
    datos <- content(resp, as = "parsed", type = "application/json")
    as.integer(datos$message$`is-referenced-by-count`)
  }, error = function(e) NA_integer_)
}

URLS_SCRAPING <- list(
  "2025" = c(
    "https://www.annualreviews.org/content/journals/economics/17/1",
    "https://www.annualreviews.org/content/journals/economics/17/1?page=2"
  ),
  "2026" = c(
    "https://www.annualreviews.org/content/journals/economics/18/1",
    "https://www.annualreviews.org/content/journals/economics/18/1?page=2"
  )
)
