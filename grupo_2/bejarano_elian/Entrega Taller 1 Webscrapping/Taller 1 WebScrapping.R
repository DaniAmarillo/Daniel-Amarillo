############################################################
# WEB SCRAPING MDPI - ENTROPY 2025
############################################################

# Librerías
library(httr2)
library(rvest)
library(dplyr)
library(stringr)
library(jsonlite)
library(DBI)
library(RSQLite)
library(ggplot2)

# Para leer fechas con meses en inglés: March, February, etc.
Sys.setlocale("LC_TIME", "C")

############################################################
# 1. URL DEL VOLUMEN
############################################################

url_volumen <- "https://www.mdpi.com/1099-4300/27"

############################################################
# 2. FUNCIÓN PARA LEER PÁGINAS MDPI
############################################################

leer_mdpi <- function(url) {
  
  request(url) |>
    req_headers(
      "User-Agent" =
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
      "Accept" =
        "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
      "Accept-Language" =
        "en-US,en;q=0.9,es;q=0.8",
      "Referer" = "https://www.mdpi.com/",
      "Connection" = "keep-alive"
    ) |>
    req_perform() |>
    resp_body_html()
}

############################################################
# 3. FUNCIÓN PARA LEER MÉTRICAS /stats
############################################################

leer_stats_mdpi <- function(url_articulo) {
  
  stats_url <- paste0(url_articulo, "/stats")
  
  tryCatch({
    
    req <- request(stats_url) |>
      req_headers(
        "User-Agent" =
          "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
        "Accept" = "application/json,text/plain,*/*",
        "Accept-Language" = "en-US,en;q=0.9,es;q=0.8",
        "Referer" = url_articulo,
        "Connection" = "keep-alive",
        "X-Requested-With" = "XMLHttpRequest"
      )
    
    res <- req |> req_perform()
    
    texto_json <- res |> resp_body_string()
    
    jsonlite::fromJSON(texto_json)
    
  }, error = function(e) {
    
    message("Error stats: ", url_articulo)
    
    NULL
  })
}

############################################################
# 4. CLASIFICACIÓN TEMÁTICA
############################################################

clasificar_tema <- function(title, abstract) {
  
  texto <- str_to_lower(paste(title, abstract))
  
  case_when(
    str_detect(texto, "generative ai|large language model|llm|gpt|transformer|diffusion model") ~ "IA Generativa",
    
    str_detect(texto, "machine learning|deep learning|neural network|classification|prediction|random forest|support vector machine|svm|clustering|reinforcement learning") ~ "Machine Learning",
    
    str_detect(texto, "statistical|statistics|bayesian|stochastic|regression|probability|entropy|distribution|estimation|inference") ~ "Estadística",
    
    TRUE ~ "Otros"
  )
}

############################################################
# 5. LEER PÁGINA DEL VOLUMEN
############################################################

pagina <- leer_mdpi(url_volumen)

############################################################
# 6. EXTRAER LINKS DE LOS ISSUES
############################################################

links <- pagina |>
  html_elements("a") |>
  html_attr("href") |>
  na.omit()

links_issues <- links |>
  str_subset("^/1099-4300/27/[0-9]+$") |>
  unique()

links_issues <- paste0("https://www.mdpi.com", links_issues)

############################################################
# 7. FUNCIÓN PARA EXTRAER LINKS DE ARTÍCULOS
############################################################

############################################################
# 7. FUNCIÓN PARA EXTRAER LINKS DE ARTÍCULOS
############################################################

obtener_links_articulos_issue <- function(url_issue) {
  
  issue <- leer_mdpi(url_issue)
  
  numero_issue <- str_extract(url_issue, "[0-9]+$")
  
  links_articulos <- issue |>
    html_elements("a") |>
    html_attr("href") |>
    na.omit() |>
    str_extract(
      paste0("/1099-4300/27/", numero_issue, "/[0-9]+")
    ) |>
    na.omit() |>
    unique()
  
  paste0("https://www.mdpi.com", links_articulos)
}

############################################################
# 8. EXTRAER TODOS LOS LINKS DE ARTÍCULOS
############################################################

############################################################
# 8. GENERAR TODOS LOS LINKS DE ARTÍCULOS
############################################################

conteo_issues <- data.frame(
  issue = 1:12,
  n_articulos = c(
    96, 119, 109, 127,
    95, 108, 117, 115,
    106, 93, 94, 85
  )
)

conteo_issues <- conteo_issues |>
  mutate(
    articulo_inicio = lag(cumsum(n_articulos), default = 0) + 1,
    articulo_fin = cumsum(n_articulos)
  )

conteo_issues

links_articulos <- unlist(
  lapply(seq_len(nrow(conteo_issues)), function(i) {
    
    issue <- conteo_issues$issue[i]
    
    articulos <- conteo_issues$articulo_inicio[i]:
      conteo_issues$articulo_fin[i]
    
    paste0(
      "https://www.mdpi.com/1099-4300/27/",
      issue,
      "/",
      articulos
    )
  })
)

leer_mdpi("https://www.mdpi.com/1099-4300/27/1/1") |>
  html_element("h1") |>
  html_text2()

leer_mdpi("https://www.mdpi.com/1099-4300/27/12/1264") |>
  html_element("h1") |>
  html_text2()

############################################################
# 9. FUNCIÓN PRINCIPAL DE SCRAPING
############################################################

scrapear_articulo_mdpi <- function(url_articulo) {
  
  articulo <- leer_mdpi(url_articulo)
  
  stats <- leer_stats_mdpi(url_articulo)
  
  title <- articulo |>
    html_element("h1") |>
    html_text2()
  
  # Fecha de publicación
  
  pubhistory <- articulo |>
    html_element("div.pubhistory") |>
    html_text2()
  
  fecha_raw <- str_match(
    pubhistory,
    "Published:\\s*([0-9]{1,2}\\s+[A-Za-z]+\\s+[0-9]{4})"
  )[, 2]
  
  publication_date <- as.Date(
    fecha_raw,
    format = "%d %B %Y"
  )
  
  publication_date <- format(
    publication_date,
    "%Y/%m/%d"
  )
  
  # DOI
  
  doi <- articulo |>
    html_elements("meta[name='citation_doi']") |>
    html_attr("content") |>
    first()
  
  if (!is.na(doi)) {
    doi <- paste0("https://doi.org/", doi)
  }
  
  # Número del artículo
  
  article_number <- articulo |>
    html_elements("meta[name='citation_id']") |>
    html_attr("content") |>
    first()
  
  if (is.na(article_number)) {
    article_number <- str_extract(url_articulo, "[0-9]+$")
  }
  
  # Abstract
  
  abstract <- articulo |>
    html_element(".art-abstract") |>
    html_text2() |>
    str_remove("^Abstract\\s*")
  
  # Autores
  
  authors <- articulo |>
    html_elements("meta[name='citation_author']") |>
    html_attr("content") |>
    unique()
  
  authors_raw <- paste(authors, collapse = "; ")
  
  n_authors <- length(authors)
  
  # Métricas
  
  downloads <- if (!is.null(stats$metrics$downloads)) {
    as.integer(stats$metrics$downloads)
  } else {
    NA_integer_
  }
  
  views <- if (!is.null(stats$metrics$views)) {
    as.integer(stats$metrics$views)
  } else {
    NA_integer_
  }
  
  citations <- if (!is.null(stats$metrics$citations)) {
    as.integer(stats$metrics$citations)
  } else {
    NA_integer_
  }
  
  # Referencias
  
  references <- articulo |>
    html_elements("#html-references_list li") |>
    html_text2()
  
  n_references <- length(references)
  
  if (n_references == 0) {
    n_references <- NA_integer_
  }
  
  topic_label <- clasificar_tema(title, abstract)
  
  data.frame(
    journal_name = "Entropy",
    article_number = article_number,
    title = title,
    publication_date = publication_date,
    year = 2025,
    doi = doi,
    url = url_articulo,
    abstract = abstract,
    authors_raw = authors_raw,
    n_authors = n_authors,
    citations = citations,
    downloads = downloads,
    views = views,
    n_references = n_references,
    topic_label = topic_label,
    stringsAsFactors = FALSE
  )
}

############################################################
# 10. PRUEBA CON UN ARTÍCULO
############################################################

paper_prueba <- scrapear_articulo_mdpi(
  "https://www.mdpi.com/1099-4300/27/3/324"
)

paper_prueba |>
  select(
    publication_date,
    article_number,
    citations,
    downloads,
    views,
    n_references
  )

############################################################
# 11. SCRAPING COMPLETO
############################################################

papers <- lapply(
  seq_along(links_articulos),
  function(i) {
    
    message("Scrapeando artículo ", i, " de ", length(links_articulos))
    
    Sys.sleep(runif(1, 2, 4))
    
    tryCatch(
      scrapear_articulo_mdpi(links_articulos[i]),
      error = function(e) {
        
        data.frame(
          journal_name = "Entropy",
          article_number = NA_character_,
          title = NA_character_,
          publication_date = NA_character_,
          year = 2025,
          doi = NA_character_,
          url = links_articulos[i],
          abstract = NA_character_,
          authors_raw = NA_character_,
          n_authors = NA_integer_,
          citations = NA_integer_,
          downloads = NA_integer_,
          views = NA_integer_,
          n_references = NA_integer_,
          topic_label = NA_character_,
          stringsAsFactors = FALSE
        )
      }
    )
  }
) |>
  bind_rows()

############################################################
# 12. AGREGAR ID
############################################################

papers <- papers |>
  mutate(
    paper_id = row_number()
  ) |>
  select(
    paper_id,
    everything()
  )

############################################################
# 13. RESULTADOS
############################################################

dim(papers)

head(
  papers |>
    select(
      paper_id,
      publication_date,
      article_number,
      citations,
      downloads,
      views,
      n_references,
      topic_label
    )
)

############################################################
# 14. RESÚMENES
############################################################

summary(papers$citations)

summary(papers$downloads)

summary(papers$views)

summary(papers$n_references)

############################################################
# 15. CREACIÓN DE BASE DE DATOS
############################################################

con <- dbConnect(
  SQLite(),
  "entropy_2025.sqlite"
)

dbWriteTable(
  con,
  "papers",
  papers,
  overwrite = TRUE
)

dbListTables(con)

dbGetQuery(
  con,
  "PRAGMA table_info(papers)"
)

############################################################
# 16. RESPUESTAS A LAS PREGUNTAS SQL
############################################################

dbGetQuery(con, "
SELECT AVG(n_authors) AS promedio_autores
FROM papers;
")

dbGetQuery(con, "
SELECT COUNT(*) AS n_ml
FROM papers
WHERE topic_label = 'Machine Learning';
")

dbGetQuery(con, "
SELECT COUNT(*) AS n_ia_generativa
FROM papers
WHERE topic_label = 'IA Generativa';
")

dbGetQuery(con, "
SELECT COUNT(*) AS n_estadistica
FROM papers
WHERE topic_label = 'Estadística';
")

dbGetQuery(con, "
SELECT COUNT(*) AS n_otros
FROM papers
WHERE topic_label = 'Otros';
")

dbGetQuery(con, "
SELECT SUM(downloads) AS total_downloads
FROM papers;
")

dbGetQuery(con, "
SELECT AVG(n_references) AS promedio_referencias
FROM papers;
")

dbGetQuery(con, "
SELECT AVG(citations) AS promedio_citas
FROM papers;
")

dbGetQuery(con, "
SELECT title, citations
FROM papers
ORDER BY citations DESC
LIMIT 1;
")

dbGetQuery(con, "
SELECT title, topic_label, citations
FROM papers
WHERE topic_label IN (
  'Machine Learning',
  'IA Generativa',
  'Estadística'
)
ORDER BY citations DESC
LIMIT 1;
")

dbGetQuery(con, "
SELECT title, topic_label, downloads
FROM papers
WHERE topic_label IN (
  'Machine Learning',
  'IA Generativa',
  'Estadística'
)
ORDER BY downloads DESC
LIMIT 1;
")

############################################################
# 17. LIMPIEZA DE DATOS
############################################################

papers_clean <- papers |>
  filter(
    !is.na(citations),
    !is.na(downloads),
    !is.na(views),
    !is.na(n_references)
  )

dim(papers_clean)

ggplot(papers_clean, aes(x = views, y = downloads)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Relación entre visualizaciones y descargas",
    x = "Article Views",
    y = "Downloads"
  ) +
  theme_minimal()

ggplot(papers_clean, aes(x = views, y = citations)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Relación entre visualizaciones y citas",
    x = "Article Views",
    y = "Citations"
  ) +
  theme_minimal()

############################################################
# 19. EXPORTAR DATASET A CSV
############################################################

write.csv(
  papers,
  "entropy_2025.csv",
  row.names = FALSE
)

