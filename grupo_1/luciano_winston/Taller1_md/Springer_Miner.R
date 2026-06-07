library(chromote)
library(jsonlite)
library(stringr)
library(dplyr)
library(DBI)
library(RSQLite)
library(tidyr)
library(purrr)


############################################################
# Scraping y base de datos SQL
############################################################

# Inicializamos el navegador
bot <- ChromoteSession$new()
bot$view() 

#Convierte listas a JSON para SQLite
safe_to_json <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  return(as.character(toJSON(x, auto_unbox = TRUE)))
}

# Evita que tibble() borre columnas si un dato viene vacío (NULL)
seguro_texto <- function(x) {
  if (is.null(x)) return("NA")
  return(as.character(x))
}


# Recolectar URLS 

todas_las_urls <- c()

for (pagina in 1:35) { 
  
  cat(sprintf("Explorando página %d...\n", pagina))
  
  url_busqueda <- paste0(
    "https://link.springer.com/search/page/", pagina, 
    "?facet-journal-id=10462&date-facet-mode=between&facet-start-year=2025&facet-end-year=2026"
  )
  
  bot$Page$navigate(url_busqueda)
  Sys.sleep(3) 
  
  script_js <- "
    Array.from(document.querySelectorAll('a'))
      .map(a => a.href)
      .filter(href => href.includes('/article/10.'))
  "
  
  resultado_js <- bot$Runtime$evaluate(expression = script_js, returnByValue = TRUE)
  urls_pagina <- unique(unlist(resultado_js$result$value))
  
  if (length(urls_pagina) == 0) {
    cat("  -> No se encontraron artículos en esta página. Fin de la paginación.\n")
    break
  }
  
  todas_las_urls <- c(todas_las_urls, urls_pagina)
  cat(sprintf("  -> %d artículos encontrados. (Total acumulado: %d)\n", length(urls_pagina), length(todas_las_urls)))
}

todas_las_urls <- unique(todas_las_urls)


# Extracción de datos y guardado en SQL

cat(sprintf("\nFASE 2: EXTRACCIÓN MASIVA DE %d ARTÍCULOS\n", length(todas_las_urls)))

con <- dbConnect(RSQLite::SQLite(), dbname = "Springer_Visual_Miner.sqlite")
lote_temporal <- list()
tamaño_lote <- 5 

for (i in seq_along(todas_las_urls)) {
  url_actual <- as.character(todas_las_urls[i])
  cat(sprintf("\n[%d/%d] Extrayendo: %s\n", i, length(todas_las_urls), url_actual))
  
  bot$Page$navigate(url_actual)
  Sys.sleep(sample(3:6, 1)) 
  
  resultado_articulo <- tryCatch({
    
    script_extraccion <- "
    (() => {
      let titulo = document.querySelector('h1.c-article-title, h1.app-article-title')?.innerText || 'NA';
      let fecha = document.querySelector('time')?.getAttribute('datetime') || 'NA';
      let abstract = document.querySelector('#Abs1-content, .c-article-section__content')?.innerText || 'NA';
      
      let autores = Array.from(document.querySelectorAll('.c-article-author-list__item, .app-article-author-list__item')).map(el => el.innerText.trim());
      let referencias = Array.from(document.querySelectorAll('.c-article-references__text')).map(el => el.innerText.trim());
      
      // Extraer Visualizaciones (Accesses)
      let visualizaciones = 'NA';
      let nodoVis = document.querySelector('[data-test=\"access-count\"]');
      if (nodoVis) { visualizaciones = nodoVis.innerText.replace(/\\s+/g, ' ').trim(); }
      
      // Extraer Citas (Citations)
      let citas = '0 Citations';
      let nodoCitas = document.querySelector('[data-test=\"citation-count\"]');
      if (nodoCitas) { citas = nodoCitas.innerText.replace(/\\s+/g, ' ').trim(); }
      
      return { 
        titulo: titulo, 
        fecha: fecha, 
        abstract: abstract, 
        autores: autores, 
        visualizaciones: visualizaciones, 
        citas: citas, 
        referencias: referencias 
      };
    })()
    "
    
    datos_js <- bot$Runtime$evaluate(expression = script_extraccion, returnByValue = TRUE)
    art <- datos_js$result$value
    
    
    tibble(
      titulo = seguro_texto(art$titulo),
      fecha_publicacion = seguro_texto(art$fecha),
      doi = str_extract(url_actual, "10\\.\\d{4,9}/[-._;()/:A-Z0-9a-z]+"),
      url = url_actual,
      autores_json = safe_to_json(art$autores),
      abstract = seguro_texto(art$abstract),
      referencias_json = safe_to_json(art$referencias),
      metricas_visualizaciones = seguro_texto(art$visualizaciones),
      metricas_citas = seguro_texto(art$citas)
    )
    
  }, error = function(e) {
    cat("  -> Error detectado al extraer datos:", e$message, "\n")
    tibble(
      titulo = "ERROR", fecha_publicacion = "NA", doi = NA_character_, url = url_actual,
      autores_json = "NA", abstract = "NA", referencias_json = "NA", 
      metricas_visualizaciones = "NA", metricas_citas = "NA"
    )
  })
  
  lote_temporal[[length(lote_temporal) + 1]] <- resultado_articulo
  
  if (i %% tamaño_lote == 0 || i == length(todas_las_urls)) {
    cat("  [!] Consolidando y guardando lote en SQLite...\n")
    df_lote <- bind_rows(lote_temporal)
    dbWriteTable(con, "articulos_scrapeados", df_lote, append = TRUE)
    lote_temporal <- list() 
  }
}

dbDisconnect(con)
bot$close() 

############################################################
# Creamos la base de datos relacional y limpiamos métricas
############################################################

con <- dbConnect(RSQLite::SQLite(), dbname = "Springer_Visual_Miner.sqlite")
datos_crudos <- dbReadTable(con, "papers")


# Tabla Principal
papers <- datos_crudos %>%
  select(doi, 
         titulo, 
         fecha_publicacion, 
         url, 
         abstract, 
         visualizaciones_num, 
         citas_num
  ) %>%
  distinct(doi, .keep_all = TRUE) %>%  mutate(
    visualizaciones_num = as.integer(str_remove_all(visualizaciones_num, "[^0-9]")),
    citas_num = as.integer(str_remove_all(citas_num, "[^0-9]"))
  ) %>%
  select(-visualizaciones_num, -citas_num)

# Tabla Auxiliar: 'autores'
autores <- datos_crudos %>%
  select(doi, autores_json) %>%
  filter(!is.na(autores_json) & autores_json != "NA") %>%
  mutate(autor = map(autores_json, ~ fromJSON(.x))) %>%
  unnest(autor) %>%
  select(doi, autor)


# Tabla Auxiliar: 'referencias'
referencias <- datos_crudos %>%
  select(doi, referencias_json) %>%
  filter(!is.na(referencias_json) & referencias_json != "NA") %>%
  mutate(referencia = map(referencias_json, ~ fromJSON(.x))) %>%
  unnest(referencia) %>%
  select(doi, referencia)

# Guardamos las nuevas tablas en SQLite 

dbWriteTable(con, "papers", papers, overwrite = TRUE)
dbWriteTable(con, "autores", autores, overwrite = TRUE)
dbWriteTable(con, "referencias", referencias, overwrite = TRUE)

# Borramos la tabla original 
dbRemoveTable(con, "articulos_scrapeados")

# Convertimos las 3 tablas a las 5 sugeridas 

con <- dbConnect(RSQLite::SQLite(), dbname = "Springer_Visual_Miner.sqlite")

# Cargamos las 3 tablas que tenemos actualmente
p_actual <- dbReadTable(con, "papers")
a_actual <- dbReadTable(con, "autores")
r_actual <- dbReadTable(con, "referencias")

# Tabla Authors
authors <- a_actual %>%
  distinct(autor) %>%
  rename(author_name = autor) %>%
  mutate(author_id = row_number())

# Tabla References
references <- r_actual %>%
  distinct(referencia) %>%
  rename(reference_text_normalized = referencia) %>%
  mutate(reference_id = row_number())


# TABLAS DE RELACIÓN (N:M)

# Mapeamos el DOI de las tablas actuales al paper_id
p_con_id <- p_actual %>%
  mutate(paper_id = row_number()) %>%
  select(paper_id, doi)

# Tabla paper_authors
paper_authors <- a_actual %>%
  inner_join(p_con_id, by = "doi") %>%
  inner_join(authors, by = c("autor" = "author_name")) %>%
  group_by(paper_id) %>%
  mutate(author_order = row_number()) %>%
  ungroup() %>%
  select(paper_id, author_id, author_order)

# Tabla paper_references
paper_references <- r_actual %>%
  inner_join(p_con_id, by = "doi") %>%
  inner_join(references, by = c("referencia" = "reference_text_normalized")) %>%
  select(paper_id, reference_id)

#################################
# Tabla principal final -papers
#################################

# Calculamos los campos agregados (n_authors, authors_raw, etc.)
resumen_autores <- a_actual %>%
  group_by(doi) %>%
  summarise(
    n_authors = n(),
    authors_raw = paste(autor, collapse = ", ")
  )

resumen_refs <- r_actual %>%
  group_by(doi) %>%
  summarise(n_references = n())

# Diccionarios para categorizar los papers
kw_ia_gen <- "generative|llm|chatgpt|large language model|diffusion model|gan"
kw_ml <- "machine learning|deep learning|neural network|predictive|classification"
kw_stat <- "statistics|statistical|bayesian|regression|anova|probability"

papers_final <- p_actual %>%
  inner_join(p_con_id, by = "doi") %>%
  left_join(resumen_autores, by = "doi") %>%
  left_join(resumen_refs, by = "doi") %>%
  mutate(
    journal_name = "Artificial Intelligence Review",
    year = as.numeric(str_extract(fecha_publicacion, "\\d{4}")),
    
    title = titulo,
    publication_date = fecha_publicacion,
    
    citations = citas_num, 
    
    downloads = visualizaciones_num, 

    topic_label = case_when(
      str_detect(tolower(paste(titulo, abstract)), kw_ia_gen) ~ "IA Generativa",
      str_detect(tolower(paste(titulo, abstract)), kw_ml) ~ "Machine Learning",
      str_detect(tolower(paste(titulo, abstract)), kw_stat) ~ "Estadística",
      TRUE ~ "Otros"
    )
  ) %>% select(paper_id, journal_name, title, publication_date, year, doi, url, 
         abstract, authors_raw, n_authors, citations, downloads, 
         n_references, topic_label)

dbWriteTable(con, "papers", papers_final, overwrite = TRUE)
dbWriteTable(con, "authors", authors, overwrite = TRUE)
dbWriteTable(con, "paper_authors", paper_authors, overwrite = TRUE)
dbWriteTable(con, "references", references, overwrite = TRUE)
dbWriteTable(con, "paper_references", paper_references, overwrite = TRUE)

dbDisconnect(con)

###############################################################
#CONSULTAS
###############################################################

con <- dbConnect(RSQLite::SQLite(), "Springer_Visual_Miner.sqlite")

query_volumen <- dbGetQuery(con, "
  SELECT 'papers' AS Tabla, COUNT(*) AS Total_Registros FROM papers
  UNION ALL
  SELECT 'authors', COUNT(*) FROM authors
  UNION ALL
  SELECT 'paper_authors', COUNT(*) FROM paper_authors
  UNION ALL
  SELECT 'references', COUNT(*) FROM `references`
  UNION ALL
  SELECT 'paper_references', COUNT(*) FROM paper_references;
")
print(query_volumen)

query_nulos <- dbGetQuery(con,"
  SELECT 
    SUM(CASE WHEN title IS NULL OR title = 'NA' THEN 1 ELSE 0 END) AS null_title,
    SUM(CASE WHEN abstract IS NULL OR abstract = 'NA' THEN 1 ELSE 0 END) AS null_abstract,
    SUM(CASE WHEN doi IS NULL OR doi = 'NA' THEN 1 ELSE 0 END) AS null_doi
  FROM papers;
" )

promedio_autor <- dbGetQuery(con, "
                             SELECT 'N_autores' AS Metrica,
                                     avg(n_authors) as Promedio
                             FROM papers")

machine_l <- dbGetQuery(con, "
                        SELECT topic_label AS Tema,
                               COUNT(*) AS Cantidad
                               FROM papers
                               WHERE topic_label = 'Machine Learning'
                               ")
IA <- dbGetQuery(con, "
                        SELECT topic_label AS Tema,
                               COUNT(*) AS Cantidad
                               FROM papers
                               WHERE topic_label = 'IA Generativa'
                               ")
otros <- dbGetQuery(con, "
                       SELECT topic_label AS Tema,
                               COUNT(*) AS Cantidad
                               FROM papers
                               WHERE topic_label = 'Otros'
                               ")
downloads <- dbGetQuery(con, "
                        SELECT 'Descargas' As Downloads,
                               SUM(downloads) As Número
                        FROM papers
                        ")

pr_ref <- dbGetQuery(con, "
                     SELECT 'Cant. Refs' as N_refs , 
                     ROUND(AVG(n_references), 2) as Promedio
                     FROM papers
                     ")

referencia_mas_repetida <- dbGetQuery(con, "
  SELECT r.reference_text_normalized AS Referencia,
         COUNT(pr.paper_id) AS Cantidad_Apariciones
  FROM paper_references pr
  INNER JOIN `references` r ON pr.reference_id = r.reference_id
  GROUP BY r.reference_id, r.reference_text_normalized
  ORDER BY Cantidad_Apariciones DESC
  LIMIT 5;
")

prom_citas <- dbGetQuery(con, "
                         SELECT 'Promedio' as Citas_art,
                                 AVG(citations) As Promedio
                         FROM papers")

pap_cit <- dbGetQuery(con, "
                      SELECT title as Titulo,
                             citations as Citaciones
                      FROM papers
                      ORDER BY Citaciones DESC
                      LIMIT 3
                      ")
