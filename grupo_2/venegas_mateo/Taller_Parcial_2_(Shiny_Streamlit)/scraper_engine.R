library(rvest)
library(tidyverse)
library(httr)
library(jsonlite)
library(RSQLite)
library(DBI)

#' Extraer metadatos de un artículo individual
#' @param article_url URL del artículo en JAIR
#' @return Un tibble con los metadatos o NULL si falla
scrape_article_details <- function(article_url) {
  message("Scraping article: ", article_url)
  
  page <- tryCatch(read_html(article_url), error = function(e) NULL)
  if (is.null(page)) return(NULL)
  
  # Metadatos vía meta tags OJS
  t <- page %>% html_node("meta[name='citation_title']") %>% html_attr("content")
  d <- page %>% html_node("meta[name='citation_date']") %>% html_attr("content")
  doi_val <- page %>% html_node("meta[name='citation_doi']") %>% html_attr("content")
  auths <- page %>% html_nodes("meta[name='citation_author']") %>% html_attr("content")
  abstr <- page %>% html_node(".article-abstract") %>% html_text(trim = TRUE)
  abstr <- gsub("^Abstract\\s*", "", ifelse(is.null(abstr), NA_character_, abstr))
  
  # Consultar OpenAlex para citas y referencias
  citas_val <- NA_real_
  refs_val <- NA_real_
  
  if (!is.na(doi_val)) {
    oa <- tryCatch(
      GET(paste0("https://api.openalex.org/works/https://doi.org/", doi_val),
          add_headers(`User-Agent` = "JAIR-Scraper-Shiny/1.0")),
      error = function(e) NULL
    )
    if (!is.null(oa) && status_code(oa) == 200) {
      oa_data <- content(oa, as = "parsed", type = "application/json")
      citas_val <- as.numeric(oa_data$cited_by_count)
      refs_val <- as.numeric(length(oa_data$referenced_works))
    }
    Sys.sleep(0.5) # Respetar la API
  }
  
  return(tibble(
    journal_name     = "Journal of Artificial Intelligence Research",
    title            = ifelse(is.na(t), "Sin título", t),
    publication_date = d,
    year             = as.numeric(substr(d, 1, 4)),
    doi              = doi_val,
    url              = article_url,
    abstract         = abstr,
    authors_raw      = paste(auths, collapse = ", "),
    n_authors        = ifelse(length(auths) == 0, NA_real_, as.numeric(length(auths))),
    citations        = citas_val,
    n_references     = refs_val
  ))
}

#' Función principal para actualizar la base de datos por volúmenes
#' @param target_volumes Vector de strings con los volúmenes (ej. c("82", "83"))
#' @param db_path Ruta al archivo .sqlite
#' @return Boolean indicando éxito
update_jair_database <- function(target_volumes, db_path = "Taller_Parcial_1_(WS_SQL)/revista_q1_2025.sqlite") {
  
  if (!file.exists(db_path)) stop("La base de datos no se encuentra en la ruta: ", db_path)
  
  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  # Mapeo de volúmenes (según Taller_Parcial_1.Rmd)
  vol_url_map <- c(
    "82" = "https://www.jair.org/index.php/jair/issue/view/1171",
    "83" = "https://www.jair.org/index.php/jair/issue/view/1172",
    "84" = "https://www.jair.org/index.php/jair/issue/view/1173"
  )
  
  if (!all(target_volumes %in% names(vol_url_map))) {
    stop("Uno o más volúmenes no están mapeados en el motor.")
  }
  
  message("Iniciando actualización para volúmenes: ", paste(target_volumes, collapse = ", "))
  
  enlaces_articulos <- c()
  for (vol in target_volumes) {
    vol_url <- vol_url_map[vol]
    message("Explorando volumen ", vol, ": ", vol_url)
    pag <- tryCatch(read_html(vol_url), error = function(e) NULL)
    Sys.sleep(1)
    
    if (!is.null(pag)) {
      links <- pag %>% html_nodes("a") %>% html_attr("href")
      articulos <- unique(links[grepl("^https://www.jair.org/index.php/jair/article/view/\\d+$", links)])
      enlaces_articulos <- c(enlaces_articulos, articulos)
    }
  }
  enlaces_articulos <- unique(enlaces_articulos)
  
  if (length(enlaces_articulos) == 0) {
    message("No se encontraron artículos para los volúmenes seleccionados.")
    return(FALSE)
  }
  
  papers_df <- tibble()
  for (link in enlaces_articulos) {
    new_paper <- scrape_article_details(link)
    if (!is.null(new_paper)) {
      papers_df <- bind_rows(papers_df, new_paper)
    }
  }
  
  if (nrow(papers_df) == 0) return(FALSE)
  
  # Clasificación temática
  papers_df <- papers_df %>%
    mutate(
      topic_label = case_when(
        grepl("generative|LLM|diffusion|large language|GPT|foundation model", title, ignore.case = TRUE) ~ "IA Generativa",
        grepl("learning|neural|reinforcement|classification|network", title, ignore.case = TRUE) ~ "Machine Learning",
        grepl("bayesian|statistical|inference|probability|variance|theorem", title, ignore.case = TRUE) ~ "Estadistica",
        TRUE ~ "Otros"
      ),
      citation = paste0(authors_raw, " (", year, "). ", title, ". ", 
                        "Journal of Artificial Intelligence Research. ", 
                        ifelse(!is.na(doi), paste0("https://doi.org/", doi), link))
    ) %>%
    mutate(paper_id = row_number())
  
  # Inserción en DB (Respetando normalización)
  # 1. Insertar PAPERS
  papers_to_insert <- papers_df %>%
    select(paper_id, journal_name, title, publication_date, year, doi, url, abstract, topic_label, citations, citation)
  
  dbWriteTable(con, "papers", papers_to_insert, append = TRUE)
  
  # 2. Insertar AUTHORS & PAPER_AUTHORS
  for (i in seq_along(papers_df$authors_raw)) {
    autores_articulo <- strsplit(papers_df$authors_raw[i], ", ")[[1]]
    for (orden in seq_along(autores_articulo)) {
      autor_limpio <- trimws(autores_articulo[orden])
      if (autor_limpio == "" || is.na(autor_limpio)) next
      
      tryCatch({
        dbExecute(con, "INSERT INTO authors (author_name) VALUES (?)", params = list(autor_limpio))
      }, error = function(e) NULL)
      
      autor_query <- dbGetQuery(con, "SELECT author_id FROM authors WHERE author_name = ?", params = list(autor_limpio))
      if (nrow(autor_query) > 0) {
        autor_id <- autor_query$author_id[1]
        tryCatch({
          dbExecute(con, "INSERT INTO paper_authors (paper_id, author_id, author_order) VALUES (?, ?, ?)",
                    params = list(papers_df$paper_id[i], autor_id, orden))
        }, error = function(e) NULL)
      }
    }
  }
  
  message("✓ Actualización completada. Procesados: ", nrow(papers_df))
  return(TRUE)
}
