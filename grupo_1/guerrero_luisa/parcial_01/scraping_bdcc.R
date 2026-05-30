#install.packages(c(
# "httr", "rvest", "dplyr", "stringr", "purrr", "tidyr",
#  "RSQLite", "DBI", "rcrossref", "jsonlite", "readr"))

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(readr)
library(DBI)
library(RSQLite)

DIR_SALIDA <- "output"

if (!dir.exists(DIR_SALIDA)) {
  dir.create(DIR_SALIDA, recursive = TRUE)
}

ISSN_BDCC <- "2504-2289"

limpiar_texto <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- x[1]
  if (is.na(x)) return(NA_character_)
  str_squish(x)
}

extraer_fecha <- function(item) {
  partes <- item$published$`date-parts`[[1]]
  paste(partes, collapse = "-")
}

extraer_autores <- function(item) {
  if (is.null(item$author)) return(NA_character_)
  
  autores <- map_chr(item$author, function(a) {
    given <- ifelse(is.null(a$given), "", a$given)
    family <- ifelse(is.null(a$family), "", a$family)
    str_squish(paste(given, family))
  })
  
  paste(autores, collapse = "; ")
}

extraer_referencias <- function(item) {
  if (is.null(item$reference)) return(character(0))
  
  refs <- map_chr(item$reference, function(r) {
    partes <- c(
      r$author,
      r$year,
      r$`article-title`,
      r$`journal-title`,
      r$DOI
    )
    
    partes <- partes[!is.null(partes)]
    partes <- partes[!is.na(partes)]
    
    str_squish(paste(partes, collapse = " "))
  })
  
  refs[refs != ""]
}

clasificar_tema <- function(titulo, resumen) {
  texto <- paste(titulo, resumen) %>% 
    str_to_lower()
  
  case_when(
    str_detect(texto, "generative|genai|large language model|llm|chatgpt|prompt|diffusion|gpt") ~ "IA Generativa",
    str_detect(texto, "machine learning|deep learning|neural network|random forest|svm|classification|prediction|classifier|convolutional|transformer") ~ "Machine Learning",
    str_detect(texto, "statistical|statistics|regression|bayesian|probability|variance|correlation|hypothesis") ~ "Estadística",
    TRUE ~ "Otros"
  )
}

cat("DESCARGANDO METADATOS DESDE CROSSREF\n")

url_crossref <- paste0(
  "https://api.crossref.org/works?",
  "filter=issn:", ISSN_BDCC,
  ",from-pub-date:2025-01-01",
  ",until-pub-date:2025-12-31",
  ",type:journal-article",
  "&rows=1000"
)

resp <- httr::GET(
  url_crossref,
  httr::add_headers(
    `User-Agent` = "Universidad Nacional - taller mineria datos"
  ),
  httr::timeout(60)
)

cat("Código HTTP CrossRef:", status_code(resp), "\n")

if (status_code(resp) != 200) {
  stop("No se pudo consultar CrossRef.")
}

contenido <- content(resp, as = "text", encoding = "UTF-8")
data <- fromJSON(contenido, simplifyVector = FALSE)

items <- data$message$items

cat("Artículos encontrados en CrossRef:", length(items), "\n")

if (length(items) == 0) {
  stop("CrossRef no devolvió artículos. No se puede continuar.")
}

papers_lista <- list()
refs_lista <- list()

for (i in seq_along(items)) {
  
  item <- items[[i]]
  
  titulo <- limpiar_texto(item$title)
  doi <- limpiar_texto(item$DOI)
  url <- ifelse(!is.na(doi), paste0("https://doi.org/", doi), NA_character_)
  autores_raw <- extraer_autores(item)
  fecha <- tryCatch(extraer_fecha(item), error = function(e) NA_character_)
  resumen <- limpiar_texto(item$abstract)
  citas <- ifelse(
    is.null(item$`is-referenced-by-count`),
    NA_integer_,
    item$`is-referenced-by-count`
  )
  referencias <- extraer_referencias(item)
  
  if (!is.na(titulo) && titulo != "") {
    
    papers_lista[[length(papers_lista) + 1]] <- tibble(
      journal_name = "Big Data and Cognitive Computing",
      title = titulo,
      publication_date = fecha,
      year = 2025,
      doi = doi,
      url = url,
      abstract = resumen,
      authors_raw = autores_raw,
      n_authors = ifelse(
        is.na(autores_raw),
        NA_integer_,
        length(str_split(autores_raw, ";\\s*")[[1]])
      ),
      citations = citas,
      downloads = NA_integer_,
      n_references = length(referencias),
      topic_label = NA_character_
    )
    
    if (length(referencias) > 0) {
      refs_lista[[length(refs_lista) + 1]] <- tibble(
        doi = doi,
        title = titulo,
        reference_order = seq_along(referencias),
        reference_text = referencias
      )
    }
    
    cat(sprintf(
      "[%d/%d] OK: %s\n",
      i,
      length(items),
      str_trunc(titulo, 70)
    ))
  }
}

papers_df <- bind_rows(papers_lista)
df_referencias <- bind_rows(refs_lista)

papers_df <- papers_df %>%
  mutate(topic_label = map2_chr(title, abstract, clasificar_tema)) %>%
  distinct(doi, .keep_all = TRUE)

write_csv(
  papers_df,
  file.path(DIR_SALIDA, "bdcc_articulos_2025.csv")
)

write_csv(
  df_referencias,
  file.path(DIR_SALIDA, "bdcc_referencias_2025.csv")
)

write_json(
  papers_df,
  file.path(DIR_SALIDA, "bdcc_articulos_2025.json"),
  pretty = TRUE
)

cat("DESCARGA FINALIZADA\n")
cat("Artículos descargados:", nrow(papers_df), "\n")
cat("Referencias descargadas:", nrow(df_referencias), "\n")
cat("Archivos guardados en:", normalizePath(DIR_SALIDA), "\n")
print(list.files(DIR_SALIDA))

papers_df <- read_csv(
  file.path(DIR_SALIDA, "bdcc_articulos_2025.csv"),
  show_col_types = FALSE
)

df_referencias <- read_csv(
  file.path(DIR_SALIDA, "bdcc_referencias_2025.csv"),
  show_col_types = FALSE
)

cat("VERIFICACIÓN FINAL\n")
cat("Artículos cargados:", nrow(papers_df), "\n")
cat("Referencias cargadas:", nrow(df_referencias), "\n")


# CREACIÓN DE BASE DE DATOS SQLITE


DB_PATH <- file.path(DIR_SALIDA, "bdcc_2025.sqlite")
SCHEMA_PATH <- file.path(DIR_SALIDA, "bdcc_2025_schema.sql")
CONSULTAS_PATH <- file.path(DIR_SALIDA, "bdcc_2025_consultas.sql")

if (file.exists(DB_PATH)) {
  file.remove(DB_PATH)
}

con <- dbConnect(SQLite(), DB_PATH)

schema_sql <- "
DROP TABLE IF EXISTS papers;
DROP TABLE IF EXISTS references_table;

CREATE TABLE papers (
    paper_id INTEGER PRIMARY KEY AUTOINCREMENT,
    journal_name TEXT,
    title TEXT NOT NULL,
    publication_date TEXT,
    year INTEGER,
    doi TEXT,
    url TEXT,
    abstract TEXT,
    authors_raw TEXT,
    n_authors INTEGER,
    citations INTEGER,
    downloads INTEGER,
    n_references INTEGER,
    topic_label TEXT
);

CREATE TABLE references_table (
    reference_id INTEGER PRIMARY KEY AUTOINCREMENT,
    doi TEXT,
    title TEXT,
    reference_order INTEGER,
    reference_text TEXT,
    reference_text_normalized TEXT
);
"

dbExecute(con, "DROP TABLE IF EXISTS papers;")
dbExecute(con, "DROP TABLE IF EXISTS references_table;")

dbExecute(con, "
CREATE TABLE papers (
    paper_id INTEGER PRIMARY KEY AUTOINCREMENT,
    journal_name TEXT,
    title TEXT NOT NULL,
    publication_date TEXT,
    year INTEGER,
    doi TEXT,
    url TEXT,
    abstract TEXT,
    authors_raw TEXT,
    n_authors INTEGER,
    citations INTEGER,
    downloads INTEGER,
    n_references INTEGER,
    topic_label TEXT
);
")

dbExecute(con, "
CREATE TABLE references_table (
    reference_id INTEGER PRIMARY KEY AUTOINCREMENT,
    doi TEXT,
    title TEXT,
    reference_order INTEGER,
    reference_text TEXT,
    reference_text_normalized TEXT
);
")

papers_sql <- papers_df %>%
  mutate(
    title = str_squish(title),
    abstract = str_squish(abstract),
    authors_raw = str_squish(authors_raw)
  ) %>%
  filter(!is.na(title), title != "") %>%
  select(
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
  )

references_sql <- df_referencias %>%
  mutate(
    reference_text = str_squish(reference_text),
    reference_text_normalized = str_to_lower(reference_text)
  ) %>%
  filter(!is.na(reference_text), reference_text != "") %>%
  select(
    doi,
    title,
    reference_order,
    reference_text,
    reference_text_normalized
  )

dbWriteTable(con, "papers", papers_sql, append = TRUE, row.names = FALSE)
dbWriteTable(con, "references_table", references_sql, append = TRUE, row.names = FALSE)

writeLines(schema_sql, SCHEMA_PATH)

cat("\nBASE DE DATOS SQLITE CREADA\n")
cat("Ruta:", normalizePath(DB_PATH), "\n")

cat("\nRegistros por tabla:\n")
print(dbGetQuery(con, "SELECT COUNT(*) AS total_papers FROM papers;"))
print(dbGetQuery(con, "SELECT COUNT(*) AS total_referencias FROM references_table;"))


# CONSULTAS SQL DEL TALLER

consultas_sql <- "
-- 1. ¿Cuál es el número promedio de autores por paper?
SELECT 
    ROUND(AVG(n_authors), 2) AS promedio_autores_por_paper
FROM papers;

-- 2. ¿Cuántos artículos están relacionados con Machine Learning?
SELECT 
    COUNT(*) AS articulos_machine_learning
FROM papers
WHERE topic_label = 'Machine Learning';

-- 3. ¿Cuántos artículos están relacionados con IA Generativa?
SELECT 
    COUNT(*) AS articulos_ia_generativa
FROM papers
WHERE topic_label = 'IA Generativa';

-- 4. ¿Cuántos artículos están relacionados con otros temas estadísticos?
SELECT 
    COUNT(*) AS articulos_estadistica
FROM papers
WHERE topic_label = 'Estadística';

-- 5. ¿Cuál es el número total de descargas de los artículos publicados en 2025?
SELECT 
    SUM(downloads) AS total_descargas_2025
FROM papers
WHERE year = 2025;

-- 6. ¿Cuál es el número promedio de referencias por artículo?
SELECT 
    ROUND(AVG(n_references), 2) AS promedio_referencias_por_articulo
FROM papers;

-- 7. ¿Cuál es la referencia que más se repite entre todos los artículos?
SELECT 
    reference_text_normalized AS referencia,
    COUNT(*) AS veces_repetida
FROM references_table
WHERE reference_text_normalized IS NOT NULL
GROUP BY reference_text_normalized
ORDER BY veces_repetida DESC
LIMIT 1;

-- 8. ¿Cuál es el promedio de citas por artículo?
SELECT 
    ROUND(AVG(citations), 2) AS promedio_citas_por_articulo
FROM papers;

-- 9. ¿Cuál es el paper con más citas?
SELECT 
    title,
    doi,
    citations,
    topic_label
FROM papers
WHERE citations IS NOT NULL
ORDER BY citations DESC
LIMIT 1;

-- 10. ¿Cuál es el paper relacionado con Machine Learning, IA Generativa o Estadística con más citas?
SELECT 
    title,
    doi,
    citations,
    topic_label
FROM papers
WHERE topic_label IN ('Machine Learning', 'IA Generativa', 'Estadística')
  AND citations IS NOT NULL
ORDER BY citations DESC
LIMIT 1;

-- 11. ¿Cuál es el paper relacionado con Machine Learning, IA Generativa o Estadística con más descargas?
SELECT 
    title,
    doi,
    downloads,
    topic_label
FROM papers
WHERE topic_label IN ('Machine Learning', 'IA Generativa', 'Estadística')
  AND downloads IS NOT NULL
ORDER BY downloads DESC
LIMIT 1;
"

writeLines(consultas_sql, CONSULTAS_PATH)

cat("\nARCHIVO DE CONSULTAS SQL CREADO\n")
cat("Ruta:", normalizePath(CONSULTAS_PATH), "\n")

cat("\nEJECUCIÓN DE CONSULTAS PRINCIPALES\n")

cat("\n1. Promedio de autores por paper:\n")
print(dbGetQuery(con, "
SELECT ROUND(AVG(n_authors), 2) AS promedio_autores_por_paper
FROM papers;
"))

cat("\n2. Artículos de Machine Learning:\n")
print(dbGetQuery(con, "
SELECT COUNT(*) AS articulos_machine_learning
FROM papers
WHERE topic_label = 'Machine Learning';
"))

cat("\n3. Artículos de IA Generativa:\n")
print(dbGetQuery(con, "
SELECT COUNT(*) AS articulos_ia_generativa
FROM papers
WHERE topic_label = 'IA Generativa';
"))

cat("\n4. Artículos de Estadística:\n")
print(dbGetQuery(con, "
SELECT COUNT(*) AS articulos_estadistica
FROM papers
WHERE topic_label = 'Estadística';
"))

cat("\n5. Total de descargas 2025:\n")
print(dbGetQuery(con, "
SELECT SUM(downloads) AS total_descargas_2025
FROM papers
WHERE year = 2025;
"))

cat("\n6. Promedio de referencias por artículo:\n")
print(dbGetQuery(con, "
SELECT ROUND(AVG(n_references), 2) AS promedio_referencias_por_articulo
FROM papers;
"))

cat("\n7. Referencia más repetida:\n")
print(dbGetQuery(con, "
SELECT reference_text_normalized AS referencia,
       COUNT(*) AS veces_repetida
FROM references_table
WHERE reference_text_normalized IS NOT NULL
GROUP BY reference_text_normalized
ORDER BY veces_repetida DESC
LIMIT 1;
"))

cat("\n8. Promedio de citas por artículo:\n")
print(dbGetQuery(con, "
SELECT ROUND(AVG(citations), 2) AS promedio_citas_por_articulo
FROM papers;
"))

cat("\n9. Paper con más citas:\n")
print(dbGetQuery(con, "
SELECT title, doi, citations, topic_label
FROM papers
WHERE citations IS NOT NULL
ORDER BY citations DESC
LIMIT 1;
"))

cat("\n10. Paper ML, IA Generativa o Estadística con más citas:\n")
print(dbGetQuery(con, "
SELECT title, doi, citations, topic_label
FROM papers
WHERE topic_label IN ('Machine Learning', 'IA Generativa', 'Estadística')
  AND citations IS NOT NULL
ORDER BY citations DESC
LIMIT 1;
"))

cat("\n11. Paper ML, IA Generativa o Estadística con más descargas:\n")
print(dbGetQuery(con, "
SELECT title, doi, downloads, topic_label
FROM papers
WHERE topic_label IN ('Machine Learning', 'IA Generativa', 'Estadística')
  AND downloads IS NOT NULL
ORDER BY downloads DESC
LIMIT 1;
"))

dbDisconnect(con)

cat("\nPROCESO COMPLETO FINALIZADO\n")
cat("Archivos disponibles en output:\n")
print(list.files(DIR_SALIDA))
