library(shiny)
library(bslib)
library(highcharter)
library(RSQLite)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(jsonlite)
library(DT)
library(shinyjs)
library(chromote)
library(curl)
library(pagedown)

if (Sys.getenv('SHINY_PORT') != "") { 
  chromote::set_chrome_args(c(
    "--no-sandbox",
    "--disable-dev-shm-usage",
    "--disable-gpu"
  ))
}

# --- CLAVE DE SUPERUSUARIO ---
SUPERUSER_PASS <- "mineria2026"

# ==============================================================================
# HELPERS GENERALES (BLINDADOS CONTRA VECTORES)
# ==============================================================================
safe_to_json <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  return(as.character(toJSON(x, auto_unbox = TRUE)))
}

seguro_texto <- function(x) {
  if (is.null(x) || length(x) == 0) return("NA")
  return(as.character(x[1])) 
}


format_numero <- function(n) {

  if (is.null(n) || length(n) == 0 || any(is.na(n))) return("—")
  
  n <- as.numeric(n[1]) 
  
  if (is.na(n)) return("—")
  if (n >= 1e6) return(paste0(round(n / 1e6, 1), "M"))
  if (n >= 1e3) return(paste0(round(n / 1e3, 1), "K"))
  return(as.character(round(n)))
}
# ==============================================================================
# TEMA HIGHCHARTER — NEXUS DARK
# ==============================================================================
nexus_theme <- hc_theme(
  chart = list(
    backgroundColor = "transparent",
    style = list(fontFamily = "'IBM Plex Sans', 'Inter', sans-serif")
  ),
  colors = c("#6366f1", "#22d3ee", "#f59e0b", "#10b981", "#f43f5e", "#a78bfa", "#34d399"),
  title = list(
    style = list(color = "#f1f5f9", fontWeight = "700", fontSize = "14px")
  ),
  subtitle = list(style = list(color = "#64748b", fontSize = "11px")),
  xAxis = list(
    gridLineColor = "rgba(148,163,184,0.07)",
    lineColor = "rgba(148,163,184,0.15)",
    tickColor = "transparent",
    labels = list(style = list(color = "#64748b", fontSize = "11px")),
    title = list(style = list(color = "#64748b"))
  ),
  yAxis = list(
    gridLineColor = "rgba(148,163,184,0.07)",
    lineColor = "transparent",
    labels = list(style = list(color = "#64748b", fontSize = "11px")),
    title = list(style = list(color = "#64748b"))
  ),
  legend = list(
    itemStyle = list(color = "#94a3b8", fontWeight = "500", fontSize = "12px"),
    itemHoverStyle = list(color = "#f1f5f9")
  ),
  tooltip = list(
    backgroundColor = "rgba(15, 23, 42, 0.95)",
    borderColor = "rgba(99,102,241,0.4)",
    borderWidth = 1,
    borderRadius = 10,
    style = list(color = "#e2e8f0", fontSize = "12px"),
    shadow = list(color = "rgba(99,102,241,0.2)", offsetX = 0, offsetY = 4, opacity = 0.4, width = 12)
  ),
  plotOptions = list(
    series = list(
      animation = list(duration = 900, easing = "easeOutCubic")
    )
  )
)

# ==============================================================================
# FUNCIÓN DE SCRAPING INTELIGENTE
# ==============================================================================
ejecutar_scraping <- function(modo_emergencia = FALSE) {
  con <- dbConnect(RSQLite::SQLite(), dbname = "Springer_Visual_Miner.sqlite")
  bot <- ChromoteSession$new()

  if (modo_emergencia) {
    tablas <- c("articulos_crudos", "papers", "authors", "paper_authors", "references", "paper_references")
    for (t in tablas) if (dbExistsTable(con, t)) dbRemoveTable(con, t)
    urls_existentes <- c()
    todas_las_urls <- c()
    for (pagina in 1:35) {
      url_busqueda <- paste0(
        "https://link.springer.com/search/page/", pagina,
        "?facet-journal-id=10462&date-facet-mode=between",
        "&facet-start-year=2025&facet-end-year=2026&sortOrder=newestFirst"
      )
      bot$Page$navigate(url_busqueda)
      Sys.sleep(3)
      script_js <- "Array.from(document.querySelectorAll('a')).map(a => a.href).filter(href => href.includes('/article/10.'))"
      resultado <- bot$Runtime$evaluate(expression = script_js, returnByValue = TRUE)
      todas_las_urls <- c(todas_las_urls, unique(unlist(resultado$result$value)))
    }
    urls_nuevas <- unique(todas_las_urls)
    mensaje_final <- paste("Reinicio Total: BD reconstruida con", length(urls_nuevas), "artículos.")
  } else {
    urls_existentes <- if (dbExistsTable(con, "articulos_crudos")) {
      dbGetQuery(con, "SELECT url FROM articulos_crudos")$url
    } else {
      c()
    }
    urls_nuevas <- c()
    pagina <- 1
    seguir_buscando <- TRUE
    while (seguir_buscando && pagina <= 35) {
      bot$Page$navigate(paste0(
        "https://link.springer.com/search/page/", pagina,
        "?facet-journal-id=10462&date-facet-mode=between",
        "&facet-start-year=2025&facet-end-year=2026&sortOrder=newestFirst"
      ))
      Sys.sleep(3)
      urls_pagina <- unique(unlist(bot$Runtime$evaluate(
        expression = "Array.from(document.querySelectorAll('a')).map(a => a.href).filter(href => href.includes('/article/10.'))",
        returnByValue = TRUE
      )$result$value))
      nuevas_en_pagina <- setdiff(urls_pagina, urls_existentes)
      urls_nuevas <- c(urls_nuevas, nuevas_en_pagina)
      if (length(nuevas_en_pagina) < length(urls_pagina)) seguir_buscando <- FALSE else pagina <- pagina + 1
    }
    if (length(urls_nuevas) == 0) {
      ultimos_5 <- dbGetQuery(con, "SELECT url FROM articulos_crudos ORDER BY rowid DESC LIMIT 5")$url
      urls_nuevas <- ultimos_5
      mensaje_final <- "Actualización rápida: Se refrescaron las métricas de los 5 artículos más recientes."
      for (u in ultimos_5) dbExecute(con, "DELETE FROM articulos_crudos WHERE url = ?", params = list(u))
    } else {
      mensaje_final <- paste("Sincronización completada:", length(urls_nuevas), "artículos nuevos agregados.")
    }
  }

  lote <- list()
  for (url_act in urls_nuevas) {
    bot$Page$navigate(url_act)
    Sys.sleep(sample(3:5, 1))
    res <- tryCatch({
      script_extraccion <- "(() => {
        let titulo = document.querySelector('h1.c-article-title, h1.app-article-title')?.innerText || 'NA';
        let fecha = document.querySelector('time')?.getAttribute('datetime') || 'NA';
        let abstract = document.querySelector('#Abs1-content, .c-article-section__content')?.innerText || 'NA';
        let autores = Array.from(document.querySelectorAll('.c-article-author-list__item, .app-article-author-list__item')).map(el => el.innerText.trim());
        let referencias = Array.from(document.querySelectorAll('.c-article-references__text')).map(el => el.innerText.trim());
        let visualizaciones = 'NA';
        let nodoVis = document.querySelector('[data-test=\"access-count\"]');
        if (nodoVis) { visualizaciones = nodoVis.innerText.replace(/\\s+/g, ' ').trim(); }
        let citas = '0 Citations';
        let nodoCitas = document.querySelector('[data-test=\"citation-count\"]');
        if (nodoCitas) { citas = nodoCitas.innerText.replace(/\\s+/g, ' ').trim(); }
        return { titulo, fecha, abstract, autores, visualizaciones, citas, referencias };
      })()"
      art <- bot$Runtime$evaluate(expression = script_extraccion, returnByValue = TRUE)$result$value
      tibble(
        titulo = seguro_texto(art$titulo),
        fecha_publicacion = seguro_texto(art$fecha),
        doi = str_extract(url_act, "10\\.\\d{4,9}/[-._;()/:A-Z0-9a-z]+"),
        url = url_act,
        autores_json = safe_to_json(art$autores),
        abstract = seguro_texto(art$abstract),
        referencias_json = safe_to_json(art$referencias),
        metricas_visualizaciones = seguro_texto(art$visualizaciones),
        metricas_citas = seguro_texto(art$citas)
      )
    }, error = function(e) NULL)
    if (!is.null(res)) lote[[length(lote) + 1]] <- res
  }
  if (length(lote) > 0) dbWriteTable(con, "articulos_crudos", bind_rows(lote), append = TRUE)
  dbDisconnect(con)
  bot$close()
  return(mensaje_final)
}

# ==============================================================================
# NORMALIZACIÓN RELACIONAL
# ==============================================================================
normalizar_tablas <- function() {
  con <- dbConnect(RSQLite::SQLite(), dbname = "Springer_Visual_Miner.sqlite")
  if (!dbExistsTable(con, "articulos_crudos")) {
    dbDisconnect(con)
    return()
  }

  datos_crudos <- dbReadTable(con, "articulos_crudos")

  p_actual <- datos_crudos %>%
    select(doi, titulo, fecha_publicacion, url, abstract, metricas_visualizaciones, metricas_citas) %>%
    distinct(doi, .keep_all = TRUE) %>%
    mutate(
      visualizaciones_num = as.integer(str_remove_all(metricas_visualizaciones, "[^0-9]")),
      citas_num = as.integer(str_remove_all(metricas_citas, "[^0-9]"))
    ) %>%
    select(-metricas_visualizaciones, -metricas_citas)

  a_actual <- datos_crudos %>%
    select(doi, autores_json) %>%
    filter(!is.na(autores_json) & autores_json != "NA") %>%
    mutate(autor = map(autores_json, ~ fromJSON(.x))) %>%
    unnest(autor) %>%
    select(doi, autor)

  r_actual <- datos_crudos %>%
    select(doi, referencias_json) %>%
    filter(!is.na(referencias_json) & referencias_json != "NA") %>%
    mutate(referencia = map(referencias_json, ~ fromJSON(.x))) %>%
    unnest(referencia) %>%
    select(doi, referencia)

  authors <- a_actual %>%
    distinct(autor) %>%
    rename(author_name = autor) %>%
    mutate(author_id = row_number())

  references <- r_actual %>%
    distinct(referencia) %>%
    rename(reference_text_normalized = referencia) %>%
    mutate(reference_id = row_number())

  p_con_id <- p_actual %>%
    mutate(paper_id = row_number()) %>%
    select(paper_id, doi)

  paper_authors <- a_actual %>%
    inner_join(p_con_id, by = "doi") %>%
    inner_join(authors, by = c("autor" = "author_name")) %>%
    group_by(paper_id) %>%
    mutate(author_order = row_number()) %>%
    ungroup() %>%
    select(paper_id, author_id, author_order)

  paper_references <- r_actual %>%
    inner_join(p_con_id, by = "doi") %>%
    inner_join(references, by = c("referencia" = "reference_text_normalized")) %>%
    select(paper_id, reference_id)

  resumen_autores <- a_actual %>%
    group_by(doi) %>%
    summarise(n_authors = n(), authors_raw = paste(autor, collapse = ", "))

  resumen_refs <- r_actual %>%
    group_by(doi) %>%
    summarise(n_references = n())

  kw_ia_gen <- "generative|llm|chatgpt|large language model|diffusion model|gan"
  kw_ml    <- "machine learning|deep learning|neural network|predictive|classification"
  kw_stat  <- "statistics|statistical|bayesian|regression|anova|probability"

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
        str_detect(tolower(paste(titulo, abstract)), kw_ml)     ~ "Machine Learning",
        str_detect(tolower(paste(titulo, abstract)), kw_stat)   ~ "Estadística",
        TRUE ~ "Otros"
      )
    ) %>%
    select(paper_id, journal_name, title, publication_date, year, doi, url,
           abstract, authors_raw, n_authors, citations, downloads, n_references, topic_label)

  dbWriteTable(con, "papers",           papers_final,     overwrite = TRUE)
  dbWriteTable(con, "authors",          authors,          overwrite = TRUE)
  dbWriteTable(con, "paper_authors",    paper_authors,    overwrite = TRUE)
  dbWriteTable(con, "references",       references,       overwrite = TRUE)
  dbWriteTable(con, "paper_references", paper_references, overwrite = TRUE)

  dbDisconnect(con)
}
