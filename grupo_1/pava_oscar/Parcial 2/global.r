# global.R - Entorno y Configuración del Scraper

library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(DT)
library(highcharter)
library(rvest)
library(httr)
library(lubridate)
library(chromote)

# IMPORTANTE: Tu archivo .sqlite debe llamarse exactamente así y estar en esta misma carpeta
DB_PATH <- "frontiers_plant_science_2025.sqlite"
PAGINAS_NUEVOS <- c(1, 2, 3, 4, 5)  
UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"

# Conexión directa a la base de datos real
get_con <- function() dbConnect(SQLite(), DB_PATH)

# Función de extracción de metadatos (Mantiene tu lógica exacta de minería de texto)
extraer_articulo <- function(b, url) {
  tryCatch({
    b$Page$navigate(url)
    Sys.sleep(4)
    html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    pag  <- read_html(html)
    
    titulo  <- pag |> html_element("h1") |> html_text2() |> str_trim()
    if(is.na(titulo) || nchar(titulo) == 0) return(NULL)
    
    doi_url <- paste0("https://doi.org/", str_extract(url, "10\\.3389/fpls\\.\\d{4}\\.[0-9]+"))
    
    fecha_raw <- pag |> html_elements("meta[name='citation_online_date']") |> html_attr("content")
    fecha_pub <- if (length(fecha_raw) > 0 && nchar(fecha_raw[1]) > 0)
      as.character(as.Date(fecha_raw[1], format = "%Y/%m/%d")) else NA_character_
    yr <- as.integer(substr(fecha_pub, 1, 4))
    
    abstract <- pag |> html_elements("meta[name='citation_abstract']") |>
      html_attr("content") |> str_remove("^Abstract\\s*") |> str_trim()
    if (length(abstract) == 0) abstract <- NA_character_
    
    autores <- pag |> html_elements("meta[name='citation_author']") |>
      html_attr("content") |> paste(collapse = "; ")
    n_autores <- str_count(autores, ";") + 1
    if (nchar(autores) == 0) { autores <- NA_character_; n_autores <- NA }
    
    todos_textos <- pag |> html_elements("span, p, div") |> html_text2()
    descargas <- str_extract(
      todos_textos[grepl("Views", todos_textos) & nchar(todos_textos) < 30] |> first(), "[0-9,]+"
    ) |> str_remove_all(",") |> as.integer()
    
    citas <- str_extract(
      todos_textos[grepl("Citations", todos_textos) & nchar(todos_textos) < 30] |> first(), "[0-9,]+"
    ) |> str_remove_all(",") |> as.integer()
    
    n_refs <- pag |> html_elements(".References li, [class*='reference'] li") |> length()
    if (n_refs == 0) n_refs <- NA_integer_
    
    texto <- tolower(paste(titulo, abstract, sep = " "))
    topic <- dplyr::case_when(
      any(str_detect(texto, c("generative", "llm", "gpt", "diffusion model", "chatgpt"))) ~ "IA Generativa",
      any(str_detect(texto, c("machine learning", "deep learning", "neural network",
                              "random forest", "convolutional", "cnn", "lstm")))          ~ "Machine Learning",
      any(str_detect(texto, c("regression", "statistical", "bayesian", "anova",
                              "principal component", "pca", "linear model")))             ~ "Estadística",
      TRUE ~ "Otros"
    )
    
    list(journal_name = "Frontiers in Plant Science", title = titulo,
         publication_date = fecha_pub, year = yr, doi = doi_url, url = url,
         abstract = abstract, authors_raw = autores, n_authors = n_autores,
         citations = citas, downloads = descargas, n_references = n_refs,
         topic_label = topic)
  }, error = function(e) { return(NULL) })
}