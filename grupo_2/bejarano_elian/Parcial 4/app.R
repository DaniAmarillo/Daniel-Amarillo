############################################################
# TALLER 2 - MINERÍA DE DATOS
# Dashboard Shiny: Entropy 2025 (MDPI) — v2
# Revista Q1 – Volumen 27
############################################################

library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(highcharter)
library(DT)
library(httr2)
library(rvest)
library(jsonlite)
library(tidyr)
library(htmltools)
source("search_helpers.R", local = TRUE)

Sys.setlocale("LC_TIME", "C")

# ── DB path ────────────────────────────────────────────
DB_PATH <- "entropy_2025.sqlite"
SEARCH_INDEX_PATH <- "search_index.rds"
get_con <- function() dbConnect(SQLite(), DB_PATH)
issue_cache <- new.env(parent = emptyenv())

volumen_a_anio <- function(volumen) {
  1998L + as.integer(volumen)
}

# ── Scraping helpers ───────────────────────────────────
leer_mdpi <- function(url) {
  request(url) |>
    req_headers(
      "User-Agent"      = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
      "Accept"          = "text/html,application/xhtml+xml,*/*;q=0.8",
      "Accept-Language" = "en-US,en;q=0.9,es;q=0.8",
      "Referer"         = "https://www.mdpi.com/",
      "Connection"      = "keep-alive"
    ) |>
    req_perform() |>
    resp_body_html()
}

leer_stats_mdpi <- function(url_articulo) {
  tryCatch({
    request(paste0(url_articulo, "/stats")) |>
      req_headers(
        "User-Agent"       = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
        "Accept"           = "application/json,text/plain,*/*",
        "X-Requested-With" = "XMLHttpRequest"
      ) |>
      req_perform() |>
      resp_body_string() |>
      jsonlite::fromJSON()
  }, error = function(e) NULL)
}

scrapear_articulo_desde_issue <- function(url_articulo, anio_objetivo = 2026L) {
  path_articulo <- str_remove(url_articulo, "^https://www\\.mdpi\\.com")
  issue_url <- str_remove(url_articulo, "/[0-9]+$")
  issue_page <- if (exists(issue_url, envir = issue_cache, inherits = FALSE)) {
    get(issue_url, envir = issue_cache, inherits = FALSE)
  } else {
    page <- tryCatch(leer_mdpi(issue_url), error = function(e) NULL)
    if (!is.null(page)) assign(issue_url, page, envir = issue_cache)
    page
  }
  if (is.null(issue_page)) return(NULL)

  item_nodes <- issue_page |>
    html_elements(xpath = paste0("//a[@href='", path_articulo, "']/ancestor::div[contains(@class,'article-content')][1]"))
  if (length(item_nodes) == 0) return(NULL)
  item <- item_nodes[[1]]

  stats <- leer_stats_mdpi(url_articulo)
  article_number <- str_extract(url_articulo, "[0-9]+$")
  title <- item |> html_element(".title-link") |> html_text2()
  if (length(title) == 0 || is.na(title) || !nzchar(title)) return(NULL)

  authors <- item |> html_elements(".authors strong") |> html_text2()
  authors_raw <- if (length(authors) > 0) paste(authors, collapse = "; ") else NA_character_

  meta_text <- item |> html_element(".color-grey-dark") |> html_text2()
  doi <- item |> html_elements("a[href^='https://doi.org/']") |> html_attr("href") |> dplyr::first()
  if (length(doi) == 0 || is.na(doi)) doi <- NA_character_

  fecha_raw <- str_match(meta_text, "-\\s*([0-9]{1,2}\\s+[A-Za-z]+\\s+[0-9]{4})")[, 2]
  publication_date <- format(as.Date(fecha_raw, format = "%d %B %Y"), "%Y/%m/%d")
  if (is.na(publication_date)) return(NULL)
  year_pub <- as.integer(substr(publication_date, 1, 4))
  if (is.na(year_pub) || year_pub != anio_objetivo) return(NULL)

  item_text <- item |> html_text2()
  parse_metric <- function(pattern) {
    value <- str_match(item_text, pattern)[, 2]
    if (length(value) == 0 || is.na(value)) return(NA_integer_)
    as.integer(str_replace_all(value, ",", ""))
  }

  downloads <- if (!is.null(stats$metrics$downloads))  as.integer(stats$metrics$downloads)  else NA_integer_
  views     <- parse_metric("Viewed by\\s*([0-9,]+)")
  citations <- parse_metric("Cited by\\s*([0-9,]+)")
  if (is.na(views) && !is.null(stats$metrics$views)) views <- as.integer(stats$metrics$views)
  if (is.na(citations) && !is.null(stats$metrics$citations)) citations <- as.integer(stats$metrics$citations)

  abstract <- item |> html_element(".abstract-full") |> html_text2()
  if (length(abstract) == 0 || is.na(abstract) || !nzchar(abstract)) {
    abstract <- item |> html_element(".abstract-cropped") |> html_text2() |> str_remove("\\s*\\[\\.\\.\\.\\]\\s*Read more\\.$")
  }

  data.frame(
    journal_name=     "Entropy",
    article_number=   article_number,
    title=            title,
    publication_date= publication_date,
    year=             year_pub,
    doi=              doi,
    url=              url_articulo,
    abstract=         abstract,
    authors_raw=      authors_raw,
    n_authors=        length(authors),
    citations=        citations,
    downloads=        downloads,
    views=            views,
    n_references=     NA_integer_,
    topic_label=      clasificar_tema(title, abstract),
    stringsAsFactors= FALSE
  )
}

clasificar_tema <- function(title, abstract) {
  texto <- str_to_lower(paste(title, abstract))
  dplyr::case_when(
    str_detect(texto, "generative ai|large language model|llm|gpt|transformer|diffusion model") ~ "IA Generativa",
    str_detect(texto, "machine learning|deep learning|neural network|classification|prediction|random forest|support vector|svm|clustering|reinforcement learning") ~ "Machine Learning",
    str_detect(texto, "statistical|statistics|bayesian|stochastic|regression|probability|entropy|distribution|estimation|inference") ~ "Estadística",
    TRUE ~ "Otros"
  )
}

scrapear_articulo_mdpi <- function(url_articulo, anio_objetivo = 2026L) {
  articulo <- tryCatch(leer_mdpi(url_articulo), error = function(e) NULL)
  if (is.null(articulo)) return(scrapear_articulo_desde_issue(url_articulo, anio_objetivo))
  stats    <- leer_stats_mdpi(url_articulo)
  title    <- articulo |> html_element("h1") |> html_text2()
  if (length(title) == 0 || is.na(title) || !nzchar(title)) return(scrapear_articulo_desde_issue(url_articulo, anio_objetivo))
  
  pubhistory       <- articulo |> html_element("div.pubhistory") |> html_text2()
  fecha_raw        <- str_match(pubhistory, "Published:\\s*([0-9]{1,2}\\s+[A-Za-z]+\\s+[0-9]{4})")[, 2]
  publication_date <- format(as.Date(fecha_raw, format = "%d %B %Y"), "%Y/%m/%d")
  year_pub         <- as.integer(substr(publication_date, 1, 4))
  if (!is.na(year_pub) && year_pub != anio_objetivo) return(NULL)
  if (is.na(publication_date)) publication_date <- paste0(anio_objetivo, "/01/01")
  
  doi            <- articulo |> html_elements("meta[name='citation_doi']")   |> html_attr("content") |> dplyr::first()
  if (!is.na(doi)) doi <- paste0("https://doi.org/", doi)
  article_number <- articulo |> html_elements("meta[name='citation_id']")    |> html_attr("content") |> dplyr::first()
  if (is.na(article_number)) article_number <- str_extract(url_articulo, "[0-9]+$")
  abstract       <- articulo |> html_element(".art-abstract")                |> html_text2() |> str_remove("^Abstract\\s*")
  authors        <- articulo |> html_elements("meta[name='citation_author']")|> html_attr("content") |> unique()
  
  downloads <- if (!is.null(stats$metrics$downloads))  as.integer(stats$metrics$downloads)  else NA_integer_
  views     <- if (!is.null(stats$metrics$views))      as.integer(stats$metrics$views)       else NA_integer_
  citations <- if (!is.null(stats$metrics$citations))  as.integer(stats$metrics$citations)   else NA_integer_
  n_ref     <- length(articulo |> html_elements("#html-references_list li") |> html_text2())
  if (n_ref == 0) n_ref <- NA_integer_
  
  data.frame(
    journal_name=     "Entropy",
    article_number=   article_number,
    title=            title,
    publication_date= publication_date,
    year=             anio_objetivo,
    doi=              doi,
    url=              url_articulo,
    abstract=         abstract,
    authors_raw=      paste(authors, collapse = "; "),
    n_authors=        length(authors),
    citations=        citations,
    downloads=        downloads,
    views=            views,
    n_references=     n_ref,
    topic_label=      clasificar_tema(title, abstract),
    stringsAsFactors= FALSE
  )
}

obtener_urls_volumen <- function(volumen = 28L, issue = "all") {
  tryCatch({
    volumen <- as.integer(volumen)
    pagina <- leer_mdpi(paste0("https://www.mdpi.com/1099-4300/", volumen))
    links  <- pagina |> html_elements("a") |> html_attr("href") |> na.omit()
    issues <- if (!is.null(issue) && issue != "all") {
      as.integer(issue)
    } else {
      links |>
        str_subset(paste0("^/1099-4300/", volumen, "/[0-9]+$")) |>
        str_extract("[0-9]+$") |>
        unique() |> as.integer() |> sort()
    }
    if (length(issues) == 0) return(character(0))
    all_urls <- c()
    for (iss in issues) {
      ip <- tryCatch(leer_mdpi(paste0("https://www.mdpi.com/1099-4300/", volumen, "/", iss)), error = function(e) NULL)
      if (is.null(ip)) next
      art <- ip |> html_elements("a") |> html_attr("href") |> na.omit() |>
        str_subset(paste0("^/1099-4300/", volumen, "/", iss, "/[0-9]+$")) |> unique()
      all_urls <- c(all_urls, paste0("https://www.mdpi.com", art))
      Sys.sleep(1)
    }
    all_urls
  }, error = function(e) character(0))
}

obtener_urls_vol28 <- function() {
  obtener_urls_volumen(28L, "all")
}

# ── Badge helpers ──────────────────────────────────────
badge_tema <- function(t) {
  switch(as.character(t),
         "Machine Learning" = '<span class="badge-ml">Machine Learning</span>',
         "IA Generativa"    = '<span class="badge-ia">IA Generativa</span>',
         "Estadística"      = '<span class="badge-est">Estadística</span>',
         '<span class="badge-otro">Otros</span>'
  )
}

############################################################
# UI
############################################################
ui <- fluidPage(
  tags$head(
    tags$link(rel="stylesheet",
              href="https://fonts.googleapis.com/css2?family=Space+Mono:wght@400;700&family=DM+Sans:ital,wght@0,300;0,400;0,500;0,600;1,300&display=swap"),
    tags$style(HTML("
/* ── Variables ─────────────────────────────────────── */
:root {
  --bg:       #090d10;
  --s1:       #10161b;
  --s2:       #172027;
  --s3:       #202b33;
  --border:   #2b3742;
  --border2:  #3c4b58;
  --acc:      #22d3ee;
  --acc2:     #818cf8;
  --acc3:     #22c55e;
  --acc4:     #f59e0b;
  --acc5:     #fb7185;
  --text:     #e5eef5;
  --muted:    #71808d;
  --muted2:   #a8b5c0;
  --r:        12px;
  --mono:     'Space Mono', monospace;
  --body:     'DM Sans', sans-serif;
}
html,body { background:var(--bg)!important; color:var(--text); font-family:var(--body); margin:0; overflow-x:hidden; }
.container-fluid { padding:0!important; }
*::-webkit-scrollbar { width:6px; height:6px; }
*::-webkit-scrollbar-track { background:var(--s1); }
*::-webkit-scrollbar-thumb { background:var(--border2); border-radius:3px; }

/* ── Topbar ────────────────────────────────────────── */
.topbar {
  background: linear-gradient(to right, #0b1115, #121b22 52%, #101922);
  border-bottom: 1px solid var(--border);
  padding: 0 28px;
  height: 62px;
  display: flex; align-items: center; gap: 14px;
  position: sticky; top: 0; z-index: 100;
}
.topbar-logo {
  width: 36px; height: 36px; border-radius: 9px;
  background: linear-gradient(135deg, var(--acc), var(--acc3));
  display: flex; align-items: center; justify-content: center;
  font-size: 18px; flex-shrink: 0;
  box-shadow: 0 0 18px rgba(34,211,238,.32);
}
.topbar-title { font-family:var(--mono); font-size:17px; font-weight:700; color:var(--text); letter-spacing:1px; }
.topbar-chips { margin-left:auto; display:flex; gap:8px; align-items:center; }
.topbar-chip {
  background:var(--s3); border:1px solid var(--border2);
  border-radius:20px; padding:4px 12px;
  font-family:var(--mono); font-size:11px; color:var(--muted2);
  display:flex; align-items:center; gap:6px;
}
.topbar-chip .dot { width:6px; height:6px; border-radius:50%; flex-shrink:0; }
.chip-blue .dot  { background:var(--acc);  box-shadow:0 0 6px var(--acc); }
.chip-green .dot { background:var(--acc3); box-shadow:0 0 6px var(--acc3); }
.chip-purple .dot{ background:var(--acc2); box-shadow:0 0 6px var(--acc2); }

/* ── Layout ────────────────────────────────────────── */
.main-layout { display:flex; min-height:calc(100vh - 62px); }

/* ── Sidebar ───────────────────────────────────────── */
.sidebar-panel {
  width:268px; min-width:268px;
  background:var(--s1);
  border-right:1px solid var(--border);
  padding:20px 16px;
  display:flex; flex-direction:column; gap:6px;
  overflow-y:auto;
}
.sbar-section {
  border:1px solid var(--border);
  border-radius:var(--r);
  overflow:hidden;
}
.sbar-section-head {
  background:var(--s2);
  padding:10px 14px;
  display:flex; align-items:center; gap:8px;
  cursor:pointer; user-select:none;
  transition:background .15s;
}
.sbar-section-head:hover { background:var(--s3); }
.sbar-section-icon { font-size:14px; }
.sbar-section-label { font-family:var(--mono); font-size:10px; letter-spacing:1.5px; text-transform:uppercase; color:var(--muted2); font-weight:700; }
.sbar-section-arrow { margin-left:auto; color:var(--muted); font-size:12px; transition:transform .2s; }
.sbar-section-arrow.open { transform:rotate(180deg); }
.sbar-body { padding:14px; background:var(--s1); display:flex; flex-direction:column; gap:10px; }

/* ── Controls ──────────────────────────────────────── */
.ctrl-wrap { display:flex; flex-direction:column; gap:4px; }
.ctrl-label { font-size:11px; color:var(--muted); font-weight:500; display:flex; align-items:center; gap:5px; }
.ctrl-label .ico { font-size:12px; }
.form-control, .selectize-input, .selectize-dropdown {
  background:var(--s2)!important; border:1px solid var(--border)!important;
  color:var(--text)!important; border-radius:8px!important;
  font-size:12px!important; font-family:var(--body)!important;
  transition:border-color .15s!important;
}
.form-control:focus, .selectize-input.focus { border-color:var(--acc)!important; box-shadow:0 0 0 3px rgba(34,211,238,.14)!important; }
.selectize-input { box-shadow:none!important; padding:6px 10px!important; min-height:34px!important; }
.selectize-dropdown { background:var(--s2)!important; border-color:var(--border2)!important; z-index:9999!important; }
.selectize-dropdown .option:hover { background:var(--s3)!important; }
.selectize-dropdown .option.selected { background:rgba(34,211,238,.18)!important; color:var(--acc)!important; }
.input-daterange .form-control { text-align:center; font-size:11px!important; }
.input-group-addon { background:var(--s3)!important; border-color:var(--border)!important; color:var(--muted)!important; font-size:12px; }

/* Reactive indicator */
.reactive-badge {
  font-family:var(--mono); font-size:9px; background:rgba(34,197,94,.15);
  color:var(--acc3); border:1px solid rgba(34,197,94,.3); border-radius:10px;
  padding:1px 6px; margin-left:auto;
}

/* Buttons */
.btn-filtrar {
  background:linear-gradient(135deg, var(--acc), #2563eb)!important;
  border:none!important; color:#fff!important; border-radius:8px!important;
  font-size:12px!important; font-weight:600!important; padding:8px!important;
  width:100%; cursor:pointer; transition:opacity .2s, transform .1s;
  letter-spacing:.3px;
}
.btn-filtrar:hover { opacity:.88!important; }
.btn-filtrar:active { transform:scale(.98)!important; }
.btn-reset {
  background:transparent!important; border:1px solid var(--border)!important;
  color:var(--muted)!important; border-radius:8px!important;
  font-size:11px!important; padding:6px!important; width:100%;
  cursor:pointer; transition:all .15s;
}
.btn-reset:hover { border-color:var(--acc)!important; color:var(--text)!important; }
.btn-scrape {
  background:linear-gradient(135deg, var(--acc2), #ec4899)!important;
  border:none!important; color:#fff!important; border-radius:8px!important;
  font-size:12px!important; font-weight:700!important; padding:9px!important;
  width:100%; cursor:pointer; transition:opacity .2s;
  box-shadow:0 4px 14px rgba(129,140,248,.25);
}
.btn-scrape:hover { opacity:.88!important; }

/* ── Content ───────────────────────────────────────── */
.content-panel { flex:1; padding:24px 28px; display:flex; flex-direction:column; gap:22px; overflow-y:auto; }

/* ── KPI grid ──────────────────────────────────────── */
.kpi-grid { display:grid; grid-template-columns:repeat(5,1fr); gap:12px; }
.kpi-card {
  background:var(--s1); border:1px solid var(--border);
  border-radius:var(--r); padding:18px 16px 14px;
  position:relative; overflow:hidden; cursor:default;
  transition:border-color .2s, transform .2s, box-shadow .2s;
}
.kpi-card:hover { transform:translateY(-3px); box-shadow:0 8px 24px rgba(0,0,0,.4); }
.kpi-card.c1:hover { border-color:var(--acc);  box-shadow:0 8px 24px rgba(34,211,238,.14); }
.kpi-card.c2:hover { border-color:var(--acc2); box-shadow:0 8px 24px rgba(129,140,248,.15); }
.kpi-card.c3:hover { border-color:var(--acc3); box-shadow:0 8px 24px rgba(34,197,94,.15); }
.kpi-card.c4:hover { border-color:var(--acc4); box-shadow:0 8px 24px rgba(245,158,11,.15); }
.kpi-card.c5:hover { border-color:var(--acc5); box-shadow:0 8px 24px rgba(251,113,133,.15); }
.kpi-card::before { content:''; position:absolute; top:0; left:0; right:0; height:3px; }
.kpi-card.c1::before { background:linear-gradient(90deg,var(--acc),#2563eb); }
.kpi-card.c2::before { background:linear-gradient(90deg,var(--acc2),#ec4899); }
.kpi-card.c3::before { background:linear-gradient(90deg,var(--acc3),#059669); }
.kpi-card.c4::before { background:linear-gradient(90deg,var(--acc4),#ea580c); }
.kpi-card.c5::before { background:linear-gradient(90deg,var(--acc5),#db2777); }
.kpi-card::after {
  content:''; position:absolute; top:-40px; right:-40px;
  width:100px; height:100px; border-radius:50%;
  opacity:.04; transition:opacity .2s;
}
.kpi-card.c1::after { background:var(--acc); }
.kpi-card.c2::after { background:var(--acc2); }
.kpi-card.c3::after { background:var(--acc3); }
.kpi-card.c4::after { background:var(--acc4); }
.kpi-card.c5::after { background:var(--acc5); }
.kpi-card:hover::after { opacity:.08; }
.kpi-label { font-size:10px; color:var(--muted); font-family:var(--mono); letter-spacing:1.5px; text-transform:uppercase; margin-bottom:10px; }
.kpi-value { font-size:30px; font-weight:700; font-family:var(--mono); line-height:1; }
.kpi-card.c1 .kpi-value { color:var(--acc); }
.kpi-card.c2 .kpi-value { color:var(--acc2); }
.kpi-card.c3 .kpi-value { color:var(--acc3); }
.kpi-card.c4 .kpi-value { color:var(--acc4); }
.kpi-card.c5 .kpi-value { color:var(--acc5); }
.kpi-trend { font-size:11px; color:var(--muted); margin-top:6px; display:flex; align-items:center; gap:4px; }
.trend-up   { color:var(--acc3); }
.trend-down { color:#f87171; }

/* ── Chart grid ────────────────────────────────────── */
.charts-grid { display:grid; grid-template-columns:1.6fr 1fr; gap:14px; }
.chart-card {
  background:var(--s1); border:1px solid var(--border);
  border-radius:var(--r); padding:18px 18px 10px;
  transition:border-color .2s;
}
.chart-card:hover { border-color:var(--border2); }
.chart-hdr { display:flex; align-items:center; gap:10px; margin-bottom:12px; }
.chart-title { font-family:var(--mono); font-size:10px; color:var(--muted); letter-spacing:1.5px; text-transform:uppercase; }
.chart-hint { margin-left:auto; font-size:10px; color:var(--muted); background:var(--s3); border-radius:20px; padding:2px 8px; border:1px solid var(--border); }

/* ── Table card ────────────────────────────────────── */
.table-card {
  background:var(--s1); border:1px solid var(--border);
  border-radius:var(--r); padding:18px;
}
.table-hdr { display:flex; align-items:center; gap:10px; margin-bottom:14px; }
.table-title { font-family:var(--mono); font-size:10px; color:var(--muted); letter-spacing:1.5px; text-transform:uppercase; }
.table-count-badge {
  background:rgba(34,211,238,.15); color:var(--acc);
  border:1px solid rgba(34,211,238,.3); border-radius:20px;
  font-family:var(--mono); font-size:11px; padding:2px 10px;
}
.search-reactive { margin-left:auto; font-size:10px; color:var(--acc3); font-family:var(--mono); }

/* DT ─────────────────────────────────────────────── */
.dataTables_wrapper { color:var(--text)!important; font-size:12px!important; }
table.dataTable thead th {
  background:var(--s2)!important; color:var(--muted)!important;
  border-bottom:1px solid var(--border)!important;
  font-family:var(--mono)!important; font-size:10px!important;
  letter-spacing:.8px; text-transform:uppercase; padding:10px 12px!important;
}
table.dataTable thead th:hover { color:var(--text)!important; }
table.dataTable tbody tr { background:transparent!important; transition:background .1s; }
table.dataTable tbody tr:hover td { background:var(--s2)!important; }
table.dataTable tbody tr.selected td { background:rgba(34,211,238,.08)!important; }
table.dataTable tbody td { border-color:var(--border)!important; color:var(--text)!important; vertical-align:middle!important; padding:10px 12px!important; }
.dataTables_paginate .paginate_button { color:var(--muted)!important; border-radius:6px!important; padding:4px 10px!important; }
.dataTables_paginate .paginate_button:hover { background:var(--s3)!important; color:var(--text)!important; border-color:transparent!important; }
.dataTables_paginate .paginate_button.current { background:var(--acc)!important; color:#fff!important; border:none!important; }
.dataTables_filter input,.dataTables_length select { background:var(--s2)!important; border:1px solid var(--border)!important; color:var(--text)!important; border-radius:6px!important; padding:4px 8px!important; }
.dataTables_info { color:var(--muted)!important; font-size:11px!important; }

/* Badges */
.badge-ml   { background:rgba(34,211,238,.14); color:#67e8f9; padding:3px 9px; border-radius:20px; font-size:10px; font-weight:600; white-space:nowrap; border:1px solid rgba(34,211,238,.25); }
.badge-ia   { background:rgba(129,140,248,.15); color:#a5b4fc; padding:3px 9px; border-radius:20px; font-size:10px; font-weight:600; white-space:nowrap; border:1px solid rgba(129,140,248,.25); }
.badge-est  { background:rgba(34,197,94,.12); color:#86efac; padding:3px 9px; border-radius:20px; font-size:10px; font-weight:600; white-space:nowrap; border:1px solid rgba(34,197,94,.2); }
.badge-otro { background:rgba(245,158,11,.12); color:#fbbf24; padding:3px 9px; border-radius:20px; font-size:10px; font-weight:600; white-space:nowrap; border:1px solid rgba(245,158,11,.2); }

/* Abstract expandible */
.abstract-row { display:none; }
.abstract-panel {
  background:var(--s2); border:1px solid var(--border);
  border-radius:8px; padding:14px 16px; margin:4px 0;
  font-size:12px; color:var(--muted2); line-height:1.65;
  animation: fadeIn .2s ease;
}
.abstract-meta { display:flex; gap:14px; margin-bottom:8px; flex-wrap:wrap; }
.abstract-stat { font-family:var(--mono); font-size:10px; color:var(--muted); background:var(--s3); border-radius:6px; padding:3px 8px; }
.abstract-title { font-size:13px; font-weight:600; color:var(--text); margin-bottom:8px; line-height:1.4; }
@keyframes fadeIn { from{opacity:0;transform:translateY(-6px)} to{opacity:1;transform:translateY(0)} }

/* Scrape UI */
.scrape-box {
  background:var(--s2); border:1px solid var(--border);
  border-radius:8px; padding:12px 14px;
  font-size:11px; font-family:var(--mono); color:var(--muted);
  min-height:38px; line-height:1.5;
}
.scrape-ok   { color:var(--acc3)!important; }
.scrape-warn { color:var(--acc4)!important; }
.scrape-err  { color:#f87171!important; }
.scrape-info { color:var(--acc)!important; }

/* KDD + quality modules */
.kdd-strip { display:grid; grid-template-columns:repeat(6,1fr); gap:10px; }
.kdd-step {
  background:var(--s1); border:1px solid var(--border); border-radius:var(--r);
  padding:12px 12px 10px; min-height:72px;
}
.kdd-step strong {
  display:block; font-family:var(--mono); color:var(--text);
  font-size:10px; letter-spacing:1px; text-transform:uppercase;
}
.kdd-step span { display:block; color:var(--muted); font-size:11px; line-height:1.35; margin-top:5px; }
.mini-grid { display:grid; grid-template-columns:repeat(3,1fr); gap:12px; }
.quality-card {
  background:var(--s1); border:1px solid var(--border); border-radius:var(--r);
  padding:16px; min-height:118px;
}
.quality-label { font-family:var(--mono); color:var(--muted); font-size:10px; letter-spacing:1.2px; text-transform:uppercase; }
.quality-value { font-family:var(--mono); color:var(--text); font-size:25px; margin-top:8px; }
.quality-note { color:var(--muted2); font-size:11px; line-height:1.4; margin-top:6px; }
.quality-ok { color:var(--acc3)!important; }
.quality-warn { color:var(--acc4)!important; }
.insight-card {
  background:linear-gradient(135deg, rgba(34,211,238,.10), rgba(34,197,94,.07));
  border:1px solid rgba(34,211,238,.26);
  border-radius:var(--r);
  padding:18px 20px;
}
.insight-title {
  font-family:var(--mono); color:var(--acc); font-size:10px;
  letter-spacing:1.5px; text-transform:uppercase; margin-bottom:7px;
}
.insight-text {
  color:var(--text); font-size:13px; line-height:1.55; margin:0;
}
.rank-controls {
  margin-left:auto; min-width:160px;
}
.rank-controls .form-group { margin:0; }
.rank-controls .selectize-input { min-height:28px!important; padding:4px 9px!important; }
.search-panel {
  background:linear-gradient(135deg, rgba(34,211,238,.08), rgba(52,211,153,.05));
  border:1px solid rgba(34,211,238,.24);
}
.search-controls {
  display:grid; grid-template-columns:minmax(260px,1fr) 190px 110px 120px;
  gap:10px; align-items:end; margin-bottom:12px;
}
.search-controls .form-group { margin-bottom:0; }
.search-controls label {
  font-family:var(--mono); color:var(--muted); font-size:10px;
  letter-spacing:1px; text-transform:uppercase; font-weight:500;
}
.search-action {
  background:linear-gradient(135deg, var(--acc), var(--acc3))!important;
  border:none!important; color:#021014!important; border-radius:8px!important;
  font-size:12px!important; font-weight:800!important; padding:9px!important; width:100%;
}
.search-meta-grid { display:grid; grid-template-columns:repeat(4,1fr); gap:10px; margin:10px 0 14px; }
.search-meta {
  background:rgba(15,23,42,.55); border:1px solid var(--border);
  border-radius:8px; padding:10px 12px;
}
.search-meta strong { display:block; color:var(--text); font-family:var(--mono); font-size:18px; }
.search-meta span { color:var(--muted); font-size:10px; text-transform:uppercase; letter-spacing:1px; }
.search-empty { color:var(--muted2); font-size:12px; padding:12px 0; }
.method-pill {
  display:inline-block; border-radius:999px; padding:3px 9px;
  background:rgba(129,140,248,.16); color:#c4b5fd;
  border:1px solid rgba(129,140,248,.26); font-size:10px; font-weight:700;
}

@media (max-width:1100px) {
  .kdd-strip, .mini-grid, .charts-grid { grid-template-columns:1fr; }
  .rank-controls { margin-left:0; min-width:100%; }
  .search-controls, .search-meta-grid { grid-template-columns:1fr; }
  .table-hdr { align-items:flex-start; flex-wrap:wrap; }
}

/* Highcharts ─────────────────────────────────────── */
.highcharts-background { fill:transparent!important; }
.highcharts-grid-line { stroke:var(--border)!important; }
.highcharts-axis-line,.highcharts-tick { stroke:var(--border)!important; }

/* Animated counter */
.kpi-value { transition:all .05s; }

/* Loading overlay */
#loading-overlay {
  position:fixed; top:0; left:0; right:0; bottom:0;
  background:rgba(8,11,18,.85); z-index:9999;
  display:flex; align-items:center; justify-content:center;
  flex-direction:column; gap:16px;
  backdrop-filter:blur(4px);
}
.loading-spinner {
  width:40px; height:40px; border-radius:50%;
  border:3px solid var(--border2);
  border-top-color:var(--acc);
  animation:spin .7s linear infinite;
}
@keyframes spin { to{transform:rotate(360deg)} }
.loading-text { font-family:var(--mono); font-size:13px; color:var(--muted2); }

/* Reactive mode indicator */
.reactive-on {
  width:7px; height:7px; border-radius:50%;
  background:var(--acc3); box-shadow:0 0 8px var(--acc3);
  display:inline-block; margin-right:5px;
  animation:pulse 2s ease-in-out infinite;
}
@keyframes pulse { 0%,100%{opacity:1} 50%{opacity:.4} }
    ")),
    
    # JS: counter animation + collapsible sections + debounce + reactive filter message
    tags$script(HTML("
// ── Animated KPI counter ─────────────────────────
function animateCounter(el, target, isFloat) {
  var start = 0, duration = 600, startTime = null;
  var numTarget = parseFloat(String(target).replace(/,/g,'')) || 0;
  function step(ts) {
    if (!startTime) startTime = ts;
    var prog = Math.min((ts - startTime) / duration, 1);
    var ease = 1 - Math.pow(1 - prog, 3);
    var val  = numTarget * ease;
    el.textContent = isFloat
      ? val.toFixed(2)
      : Math.round(val).toLocaleString('es-CO');
    if (prog < 1) requestAnimationFrame(step);
    else el.textContent = target;
  }
  requestAnimationFrame(step);
}

// Observe KPI text changes
var kpiIds = ['kpi_total','kpi_autores','kpi_citas','kpi_refs','kpi_downloads'];
kpiIds.forEach(function(id) {
  var obs = new MutationObserver(function(muts) {
    muts.forEach(function(m) {
      var el  = m.target;
      var txt = el.textContent.trim();
      var isF = txt.includes('.');
      animateCounter(el, txt, isF);
    });
  });
  var el = document.getElementById(id);
  if (el) obs.observe(el, {childList:true, characterData:true, subtree:true});
});

// ── Collapsible sidebar sections ─────────────────
$(document).on('click', '.sbar-section-head', function() {
  var body  = $(this).next('.sbar-body');
  var arrow = $(this).find('.sbar-section-arrow');
  body.slideToggle(180);
  arrow.toggleClass('open');
});

// ── Debounced reactive filters ───────────────────
var debounceTimers = {};
function debounce(fn, delay, key) {
  clearTimeout(debounceTimers[key]);
  debounceTimers[key] = setTimeout(fn, delay);
}

$(document).on('input', '#filtro_titulo, #filtro_autor, #filtro_doi', function() {
  var id = this.id;
  debounce(function() {
    Shiny.setInputValue('debounced_text', {
      id: id, val: $('#' + id).val(), ts: Date.now()
    });
  }, 420, id);
});

$(document).on('change', '#filtro_tema', function() {
  Shiny.setInputValue('debounced_tema', { val: $(this).val(), ts: Date.now() });
});
    "))
  ),
  
  # Loading overlay (hidden by default via JS after load)
  tags$div(id = "loading-overlay", style = "display:none;",
           tags$div(class = "loading-spinner"),
           tags$div(class = "loading-text", "Cargando datos…")
  ),
  
  # ── Topbar ───────────────────────────────────────────
  div(class = "topbar",
      div(class = "topbar-logo", "📊"),
      span(style = "font-family:var(--mono);font-size:17px;font-weight:700;letter-spacing:1px;color:var(--text);",
           "ENTROPY DASHBOARD"),
      div(class = "topbar-chips",
          div(class = "topbar-chip chip-blue",  span(class="dot"), textOutput("chip_total",  inline=TRUE)),
          div(class = "topbar-chip chip-green", span(class="dot"), "Vol. 27 · 2025"),
          div(class = "topbar-chip chip-purple",span(class="dot"), "MDPI · Q1"),
          div(class = "topbar-chip",
              span(class="reactive-on"), "Filtros reactivos")
      )
  ),
  
  div(class = "main-layout",
      
      # ── SIDEBAR ─────────────────────────────────────────
      div(class = "sidebar-panel",
          
          # Sección Filtros
          div(class = "sbar-section",
              div(class = "sbar-section-head",
                  span(class="sbar-section-icon","🔍"),
                  span(class="sbar-section-label","Filtros"),
                  span(class="sbar-section-arrow open","▾")
              ),
              div(class = "sbar-body",
                  div(class="ctrl-wrap",
                      div(class="ctrl-label", span(class="ico","📅"), "Rango de fechas"),
                      div(class="input-daterange input-group",
                          tags$input(id="fecha_inicio",type="text",class="input-sm form-control",
                                     value="2025-01-01",`data-date-format`="yyyy-mm-dd"),
                          tags$span(class="input-group-addon","—"),
                          tags$input(id="fecha_fin",type="text",class="input-sm form-control",
                                     value="2025-12-31",`data-date-format`="yyyy-mm-dd")
                      )
                  ),
                  div(class="ctrl-wrap",
                      div(class="ctrl-label",span(class="ico","🏷"),"Temática"),
                      selectInput("filtro_tema","",
                                  choices  = c("Todas"="","Machine Learning","IA Generativa","Estadística","Otros"),
                                  selected = "")
                  ),
                  div(class="ctrl-wrap",
                      div(class="ctrl-label",span(class="ico","📝"),"Título",
                          span(class="reactive-badge","reactivo")),
                      textInput("filtro_titulo","",placeholder="ej. entropy, deep learning…")
                  ),
                  div(class="ctrl-wrap",
                      div(class="ctrl-label",span(class="ico","👤"),"Autor",
                          span(class="reactive-badge","reactivo")),
                      textInput("filtro_autor","",placeholder="ej. Chen, Smith…")
                  ),
                  div(class="ctrl-wrap",
                      div(class="ctrl-label",span(class="ico","🔗"),"DOI",
                          span(class="reactive-badge","reactivo")),
                      textInput("filtro_doi","",placeholder="ej. 10.3390/e27…")
                  ),
                  div(style="display:grid;grid-template-columns:1fr 1fr;gap:8px;",
                      div(class="ctrl-wrap",
                          div(class="ctrl-label","Citas mín."),
                          numericInput("filtro_citas_min","",value=0,min=0,step=1,width="100%")
                      ),
                      div(class="ctrl-wrap",
                          div(class="ctrl-label","Descargas mín."),
                          numericInput("filtro_descargas_min","",value=0,min=0,step=25,width="100%")
                      )
                  ),
                  checkboxInput("filtro_metricas", "Solo métricas completas", value = FALSE),
                  div(style="display:grid;grid-template-columns:1fr 1fr;gap:8px;",
                      actionButton("btn_filtrar","Aplicar",class="btn btn-filtrar"),
                      actionButton("btn_reset",  "Reset",   class="btn btn-reset")
                  )
              )
          ),
          
          # Sección Actualización
          div(class = "sbar-section", style="margin-top:6px;",
              div(class="sbar-section-head",
                  span(class="sbar-section-icon","🔄"),
                  span(class="sbar-section-label","Actualización"),
                  span(class="sbar-section-arrow open","▾")
              ),
              div(class="sbar-body",
                  tags$p(style="font-size:11px;color:var(--muted);margin:0 0 8px;line-height:1.5;",
                         "Elige exactamente que parte de Entropy (MDPI) quieres consultar."),
                  div(style="display:grid;grid-template-columns:1fr 1fr;gap:8px;",
                      div(class="ctrl-wrap",
                          div(class="ctrl-label","Año"),
                          numericInput("scrape_year","",value=2026,min=2025,max=2028,step=1,width="100%")
                      ),
                      div(class="ctrl-wrap",
                          div(class="ctrl-label","Volumen"),
                          numericInput("scrape_volume","",value=28,min=27,max=30,step=1,width="100%")
                      )
                  ),
                  div(style="display:grid;grid-template-columns:1fr 1fr;gap:8px;",
                      div(class="ctrl-wrap",
                          div(class="ctrl-label","Issue"),
                          selectInput("scrape_issue","",
                                      choices=c("Todos"="all", as.character(1:12)),
                                      selected="all", width="100%")
                      ),
                      div(class="ctrl-wrap",
                          div(class="ctrl-label","Máximo"),
                          numericInput("scrape_limit","",value=20,min=1,max=250,step=1,width="100%")
                      )
                  ),
                  div(class="ctrl-wrap",
                      div(class="ctrl-label","Modo"),
                      selectInput("scrape_mode","",
                                  choices=c("Solo artículos nuevos"="new",
                                            "Reconsultar selección"="refresh"),
                                  selected="new", width="100%")
                  ),
                  actionButton("btn_scrape","🚀  Ejecutar scraping",class="btn btn-scrape"),
                  div(class="scrape-box",style="margin-top:8px;", uiOutput("scrape_status"))
              )
          )
      ),
      
      # ── CONTENT ─────────────────────────────────────────
      div(class = "content-panel",

          div(class="kdd-strip",
              div(class="kdd-step", strong("Adquisición"), span("Scraping MDPI y endpoint /stats")),
              div(class="kdd-step", strong("Almacenamiento"), span("SQLite · tabla papers")),
              div(class="kdd-step", strong("Preparación"), span("Fechas, temas y métricas")),
              div(class="kdd-step", strong("Consulta"), span("Filtros reactivos y tabla")),
              div(class="kdd-step", strong("Minería"), span("Impacto, rankings y outliers")),
              div(class="kdd-step", strong("Actualización"), span("Nuevos papers o verificación"))
          ),
          
          # KPI row
          div(class = "kpi-grid",
              div(class="kpi-card c1",
                  div(class="kpi-label","Total artículos"),
                  div(class="kpi-value",id="kpi_total",  textOutput("kpi_total",  inline=TRUE)),
                  div(class="kpi-trend","en la selección actual")
              ),
              div(class="kpi-card c2",
                  div(class="kpi-label","Prom. autores"),
                  div(class="kpi-value",id="kpi_autores",textOutput("kpi_autores",inline=TRUE)),
                  div(class="kpi-trend","por artículo")
              ),
              div(class="kpi-card c3",
                  div(class="kpi-label","Prom. citas"),
                  div(class="kpi-value",id="kpi_citas",  textOutput("kpi_citas",  inline=TRUE)),
                  div(class="kpi-trend","por artículo")
              ),
              div(class="kpi-card c4",
                  div(class="kpi-label","Prom. referencias"),
                  div(class="kpi-value",id="kpi_refs",   textOutput("kpi_refs",   inline=TRUE)),
                  div(class="kpi-trend","por artículo")
              ),
              div(class="kpi-card c5",
                  div(class="kpi-label","Prom. descargas"),
                  div(class="kpi-value",id="kpi_downloads",textOutput("kpi_downloads",inline=TRUE)),
                  div(class="kpi-trend","por artículo")
              )
          ),

          # Buscador cientifico - Taller 4
          div(class="table-card search-panel",
              div(class="table-hdr",
                  span(class="table-title","Buscador cientifico"),
                  span(class="table-count-badge",textOutput("search_count",inline=TRUE)),
                  span(class="search-reactive","TF-IDF vs LSA/SVD")
              ),
              div(class="search-controls",
                  textInput("ir_query","Consulta en lenguaje natural",
                            value="generative artificial intelligence entropy",
                            placeholder="ej. applications of generative artificial intelligence in complex systems"),
                  selectInput("ir_strategy","Estrategia",
                              choices=c("TF-IDF + coseno"="tfidf",
                                        "LSA/SVD + coseno"="lsa",
                                        "Comparar ambas"="compare"),
                              selected="tfidf"),
                  selectInput("ir_top_n","Top N",choices=c(5,10,20),selected=10),
                  actionButton("btn_ir_search","Buscar",class="btn search-action")
              ),
              uiOutput("search_index_meta"),
              highchartOutput("chart_search_scores",height="210px"),
              DTOutput("tabla_busqueda")
          ),
          
          # Charts row 1
          div(class="charts-grid",
              div(class="chart-card",
                  div(class="chart-hdr",
                      span(class="chart-title","Evolución temporal de publicaciones"),
                      span(class="chart-hint","hover para detalles")
                  ),
                  highchartOutput("chart_temporal",height="250px")
              ),
              div(class="chart-card",
                  div(class="chart-hdr",
                      span(class="chart-title","Artículos por temática"),
                      span(class="chart-hint","click para filtrar")
                  ),
                  highchartOutput("chart_temas",height="250px")
              )
          ),
          
          # Charts row 2
          div(class="charts-grid",
              div(class="chart-card",
                  div(class="chart-hdr",
                      span(class="chart-title","Distribución de citas"),
                      span(class="chart-hint","≤30 citas")
                  ),
                  highchartOutput("chart_citas",height="220px")
              ),
              div(class="chart-card",
                  div(class="chart-hdr",
                      span(class="chart-title","Top 10 autores más frecuentes"),
                      span(class="chart-hint","click para filtrar")
                  ),
                  highchartOutput("chart_autores",height="220px")
              )
          ),

          # Impact analytics
          div(class="charts-grid",
              div(class="chart-card",
                  div(class="chart-hdr",
                      span(class="chart-title","Impacto: vistas vs descargas"),
                      span(class="chart-hint","tamaño = citas")
                  ),
                  highchartOutput("chart_impacto",height="300px")
              ),
              div(class="chart-card",
                  div(class="chart-hdr",
                      span(class="chart-title","Promedios por temática"),
                      span(class="chart-hint","comparativo")
                  ),
                  highchartOutput("chart_metricas_tema",height="300px")
              )
          ),

          div(class="mini-grid",
              uiOutput("quality_dates"),
              uiOutput("quality_missing"),
              uiOutput("quality_topics")
          ),

          div(class="table-card",
              div(class="table-hdr",
                  span(class="table-title","Calidad de datos"),
                  span(class="table-count-badge",textOutput("quality_count",inline=TRUE)),
                  span(class="search-reactive","faltantes, sin fecha o sin categoria")
              ),
              DTOutput("tabla_calidad")
          ),

          div(class="insight-card",
              div(class="insight-title","Lectura rapida de hallazgos"),
              p(class="insight-text", textOutput("insight_summary"))
          ),

          div(class="charts-grid",
              div(class="table-card",
                  div(class="table-hdr",
                      span(class="table-title","Ranking de articulos"),
                      span(class="table-count-badge","top 10"),
                      div(class="rank-controls",
                          selectInput("ranking_metric","",
                                      choices=c("Citas"="citations",
                                                "Descargas"="downloads",
                                                "Visualizaciones"="views",
                                                "Referencias"="n_references",
                                                "Autores"="n_authors"),
                                      selected="citations")
                      )
                  ),
                  DTOutput("tabla_ranking")
              ),
              div(class="table-card",
                  div(class="table-hdr",
                      span(class="table-title","Registros a revisar"),
                      span(class="table-count-badge",textOutput("outliers_count",inline=TRUE))
                  ),
                  DTOutput("tabla_outliers")
              )
          ),

          # Resultado inmediato del scraping
          conditionalPanel("output.hay_scrape_result == true",
                           div(class="table-card",
                               div(class="table-hdr",
                                   span(class="table-title","Nuevos o verificados"),
                                   span(class="table-count-badge",textOutput("nuevos_count_top",inline=TRUE)),
                                   span(class="search-reactive","resultado inmediato del scraping")
                               ),
                               DTOutput("tabla_nuevos_top")
                           )
          ),
          
          # Tabla principal
          div(class="table-card",
              div(class="table-hdr",
                  span(class="table-title","📋 Artículos"),
                  span(class="table-count-badge",textOutput("tabla_count",inline=TRUE)),
                  span(class="search-reactive","▸ click en fila para ver abstract")
              ),
              DTOutput("tabla_articulos")
          ),
          
          # Panel de abstract expandido
          uiOutput("abstract_panel_ui"),
          
          # Artículos nuevos o verificados (scraping)
          conditionalPanel("output.hay_scrape_result == true",
                           div(class="table-card",
                               div(class="table-hdr",
                                   span(class="table-title","🆕 Nuevos o verificados"),
                                   span(class="table-count-badge",textOutput("nuevos_count",inline=TRUE))
                               ),
                               DTOutput("tabla_nuevos")
                           )
          )
      )
  )
)

############################################################
# SERVER
############################################################
server <- function(input, output, session) {
  
  # ── Estado reactivo ────────────────────────────────
  rv <- reactiveValues(
    filtros = list(
      fecha_inicio = as.Date("2025-01-01"),
      fecha_fin    = as.Date("2025-12-31"),
      tema         = "",
      titulo       = "",
      autor        = "",
      doi          = "",
      citas_min    = 0,
      descargas_min = 0,
      metricas_completas = FALSE
    ),
    fila_sel    = NULL,
    datos_nuevos = NULL,
    datos_verificados = NULL,
    db_version = 0,
    scrape_msg  = list(texto = "Presiona el botón para buscar artículos de 2026.", clase = "scrape-info")
  )
  
  # ── Leer BD ────────────────────────────────────────
  todos <- reactive({
    rv$db_version
    con <- get_con(); on.exit(dbDisconnect(con))
    df  <- dbGetQuery(con, "SELECT * FROM papers")
    df$publication_date <- as.Date(df$publication_date, format="%Y/%m/%d")
    df |>
      filter(is.na(publication_date) | publication_date >= as.Date("2025-01-01"))
  })
  
  # ── Función filtrar ────────────────────────────────
  filtrar <- function(df, f) {
    if (!is.na(f$fecha_inicio)) df <- df[!is.na(df$publication_date) & df$publication_date >= f$fecha_inicio, ]
    if (!is.na(f$fecha_fin))    df <- df[!is.na(df$publication_date) & df$publication_date <= f$fecha_fin, ]
    if (nchar(f$tema)   > 0)    df <- df[!is.na(df$topic_label) & df$topic_label == f$tema, ]
    if (nchar(f$titulo) > 0)    df <- df[str_detect(tolower(df$title), fixed(tolower(f$titulo))), ]
    if (nchar(f$autor)  > 0)    df <- df[!is.na(df$authors_raw) & str_detect(tolower(df$authors_raw), fixed(tolower(f$autor))), ]
    if (nchar(f$doi)    > 0)    df <- df[!is.na(df$doi) & str_detect(tolower(df$doi), fixed(tolower(f$doi))), ]
    if (!is.null(f$citas_min) && !is.na(f$citas_min)) df <- df[is.na(df$citations) | df$citations >= f$citas_min, ]
    if (!is.null(f$descargas_min) && !is.na(f$descargas_min)) df <- df[is.na(df$downloads) | df$downloads >= f$descargas_min, ]
    if (isTRUE(f$metricas_completas)) {
      df <- df[!is.na(df$citations) & !is.na(df$downloads) & !is.na(df$views) & !is.na(df$n_references), ]
    }
    df
  }
  
  datos_filtrados <- reactive({ filtrar(todos(), rv$filtros) })

  # Taller 4: indice cacheado de recuperacion de informacion.
  indice_busqueda <- reactive({
    rv$db_version
    load_or_build_search_index(DB_PATH, SEARCH_INDEX_PATH)
  })

  resultados_busqueda <- eventReactive(input$btn_ir_search, {
    req(input$ir_query)
    idx <- indice_busqueda()
    top_n <- as.integer(input$ir_top_n)
    strategy <- input$ir_strategy
    if (identical(strategy, "compare")) {
      bind_rows(
        search_articles(input$ir_query, idx, "tfidf", top_n),
        search_articles(input$ir_query, idx, "lsa", top_n)
      )
    } else {
      search_articles(input$ir_query, idx, strategy, top_n)
    }
  }, ignoreInit = FALSE)

  output$search_count <- renderText({
    res <- resultados_busqueda()
    if (is.null(res) || nrow(res) == 0) "0 resultados" else paste0(nrow(res), " resultado(s)")
  })

  output$search_index_meta <- renderUI({
    idx <- indice_busqueda()
    div(class="search-meta-grid",
        div(class="search-meta", strong(format(idx$meta$n_docs, big.mark=",")), span("documentos")),
        div(class="search-meta", strong(format(idx$meta$original_dim, big.mark=",")), span("dimension original")),
        div(class="search-meta", strong(idx$meta$reduced_dim), span("dimension LSA")),
        div(class="search-meta", strong("P@5"), span("evaluacion en taller_4"))
    )
  })

  output$chart_search_scores <- renderHighchart({
    res <- resultados_busqueda()
    if (is.null(res) || nrow(res) == 0) {
      return(highchart() |> hc_title(text="Sin resultados para la consulta") |> hc_credits(enabled=FALSE))
    }
    plot_df <- res |>
      mutate(label = paste0("#", rank, " ", str_trunc(title, 45)),
             method_rank = paste(strategy, rank)) |>
      arrange(score) |>
      tail(20)
    highchart() |>
      hc_chart(type="bar", backgroundColor="transparent") |>
      hc_xAxis(categories=plot_df$label,
               labels=list(style=list(color="#94a3b8",fontSize="10px")),
               lineColor="#232a3e") |>
      hc_yAxis(title=list(text=NULL),
               labels=list(style=list(color="#64748b",fontSize="10px")),
               gridLineColor="#1c2235") |>
      hc_add_series(name="Similitud", data=as.list(plot_df$score),
                    color="#22d3ee", borderRadius=4) |>
      hc_tooltip(backgroundColor="#141824", borderColor="#2e3855",
                 pointFormat="<b>{point.y:.4f}</b>") |>
      hc_legend(enabled=FALSE) |>
      hc_credits(enabled=FALSE)
  })

  output$tabla_busqueda <- renderDT({
    res <- resultados_busqueda()
    if (is.null(res) || nrow(res) == 0) {
      return(datatable(data.frame(Mensaje="Escribe una consulta y ejecuta la busqueda."),
                       rownames=FALSE, options=list(dom="t")))
    }
    tabla <- res |>
      mutate(
        Metodo = paste0("<span class='method-pill'>", strategy, "</span>"),
        Pos = rank,
        Puntaje = sprintf("%.4f", score),
        Titulo = ifelse(nzchar(url),
                        paste0("<a href='", url, "' target='_blank'>", htmltools::htmlEscape(str_trunc(title, 95)), "</a>"),
                        htmltools::htmlEscape(str_trunc(title, 95))),
        Autores = htmltools::htmlEscape(str_trunc(authors_raw, 85)),
        Fecha = as.character(publication_date),
        Tema = topic_label,
        DOI = ifelse(nzchar(doi),
                     paste0("<a href='", doi, "' target='_blank'>DOI</a>"),
                     "-"),
        Fragmento = htmltools::htmlEscape(fragment)
      ) |>
      select(Metodo, Pos, Puntaje, Titulo, Autores, Fecha, Tema, DOI, Fragmento)
    datatable(
      tabla,
      escape = FALSE,
      rownames = FALSE,
      options = list(pageLength=10, scrollX=TRUE, dom="tip",
                     order=list(list(0, "asc"), list(1, "asc")),
                     language=list(info="Mostrando _START_-_END_ de _TOTAL_", paginate=list(previous="Anterior", `next`="Siguiente")))
    )
  })
  
  # ── Debounced text inputs ──────────────────────────
  observeEvent(input$debounced_text, {
    txt <- input$debounced_text
    if (txt$id == "filtro_titulo") rv$filtros$titulo <- txt$val
    if (txt$id == "filtro_autor")  rv$filtros$autor  <- txt$val
    if (txt$id == "filtro_doi")    rv$filtros$doi    <- txt$val
  })
  
  observeEvent(input$debounced_tema, {
    rv$filtros$tema <- input$debounced_tema$val
  })
  
  # ── Botón aplicar filtros ──────────────────────────
  observeEvent(input$btn_filtrar, {
    rv$filtros <- list(
      fecha_inicio = tryCatch(as.Date(input$fecha_inicio), error=function(e) as.Date("2025-01-01")),
      fecha_fin    = tryCatch(as.Date(input$fecha_fin),    error=function(e) as.Date("2025-12-31")),
      tema         = if (!is.null(input$filtro_tema))   input$filtro_tema   else "",
      titulo       = if (!is.null(input$filtro_titulo)) input$filtro_titulo else "",
      autor        = if (!is.null(input$filtro_autor))  input$filtro_autor  else "",
      doi          = if (!is.null(input$filtro_doi))    input$filtro_doi    else "",
      citas_min    = if (!is.null(input$filtro_citas_min)) input$filtro_citas_min else 0,
      descargas_min = if (!is.null(input$filtro_descargas_min)) input$filtro_descargas_min else 0,
      metricas_completas = isTRUE(input$filtro_metricas)
    )
  })
  
  # ── Botón reset ────────────────────────────────────
  observeEvent(input$btn_reset, {
    rv$filtros <- list(
      fecha_inicio = as.Date("2025-01-01"),
      fecha_fin    = as.Date("2025-12-31"),
      tema = "", titulo = "", autor = "", doi = "",
      citas_min = 0, descargas_min = 0, metricas_completas = FALSE
    )
    updateSelectInput(session, "filtro_tema",   selected = "")
    updateTextInput(session,   "filtro_titulo", value    = "")
    updateTextInput(session,   "filtro_autor",  value    = "")
    updateTextInput(session,   "filtro_doi",    value    = "")
    updateNumericInput(session, "filtro_citas_min", value = 0)
    updateNumericInput(session, "filtro_descargas_min", value = 0)
    updateCheckboxInput(session, "filtro_metricas", value = FALSE)
    rv$fila_sel <- NULL
  })

  observeEvent(input$scrape_year, {
    nuevo_volumen <- as.integer(input$scrape_year) - 1998L
    if (!is.na(nuevo_volumen)) updateNumericInput(session, "scrape_volume", value = nuevo_volumen)
  }, ignoreInit=TRUE)
  
  # ── Click en donut → filtra tema ──────────────────
  observeEvent(input$chart_temas_click, {
    clicked_tema <- input$chart_temas_click$name
    if (!is.null(clicked_tema) && nchar(clicked_tema) > 0) {
      if (rv$filtros$tema == clicked_tema) {
        rv$filtros$tema <- ""
        updateSelectInput(session, "filtro_tema", selected = "")
      } else {
        rv$filtros$tema <- clicked_tema
        updateSelectInput(session, "filtro_tema", selected = clicked_tema)
      }
    }
  })
  
  # ── Click en bar de autores → filtra autor ────────
  observeEvent(input$chart_autores_click, {
    clicked_autor <- input$chart_autores_click$category
    if (!is.null(clicked_autor) && nchar(clicked_autor) > 0) {
      rv$filtros$autor <- clicked_autor
      updateTextInput(session, "filtro_autor", value = clicked_autor)
    }
  })
  
  # ── KPIs ───────────────────────────────────────────
  df_kpi <- reactive({
    df <- datos_filtrados()
    if (is.null(df) || nrow(df) == 0)
      return(list(n=0, aut="0.0", cit="0.00", ref="0.0", dl="0"))
    list(
      n   = format(nrow(df), big.mark=","),
      aut = sprintf("%.1f", mean(df$n_authors,    na.rm=TRUE)),
      cit = sprintf("%.2f", mean(df$citations,    na.rm=TRUE)),
      ref = sprintf("%.1f", mean(df$n_references, na.rm=TRUE)),
      dl  = format(round(mean(df$downloads, na.rm=TRUE)), big.mark=",")
    )
  })
  
  output$chip_total    <- renderText({ paste0(df_kpi()$n, " artículos") })
  output$kpi_total     <- renderText({ df_kpi()$n })
  output$kpi_autores   <- renderText({ df_kpi()$aut })
  output$kpi_citas     <- renderText({ df_kpi()$cit })
  output$kpi_refs      <- renderText({ df_kpi()$ref })
  output$kpi_downloads <- renderText({ df_kpi()$dl })
  
  # ── Gráfico temporal con tooltip enriquecido ──────
  output$chart_temporal <- renderHighchart({
    df <- datos_filtrados()
    req(!is.null(df), nrow(df) > 0)
    
    by_month <- df |>
      mutate(mes = format(publication_date, "%Y-%m")) |>
      filter(!is.na(mes)) |>
      group_by(mes) |>
      summarise(
        n       = n(),
        avg_cit = round(mean(citations, na.rm=TRUE), 2),
        top_art = first(title[which.max(replace(citations, is.na(citations), -1))]),
        .groups = "drop"
      ) |>
      arrange(mes)
    
    # Truncate long titles for tooltip
    by_month$top_art <- str_trunc(by_month$top_art, 60)
    
    pts <- lapply(seq_len(nrow(by_month)), function(i) {
      list(
        y       = by_month$n[i],
        avg_cit = by_month$avg_cit[i],
        top_art = by_month$top_art[i]
      )
    })
    
    highchart() |>
      hc_chart(type="areaspline", backgroundColor="transparent",
               animation=list(duration=600)) |>
      hc_xAxis(
        categories = by_month$mes,
        labels = list(style=list(color="#64748b",fontSize="10px")),
        lineColor="#232a3e", tickColor="#232a3e"
      ) |>
      hc_yAxis(
        title = list(text=NULL),
        labels = list(style=list(color="#64748b",fontSize="10px")),
        gridLineColor="#1c2235"
      ) |>
      hc_add_series(
        name = "Artículos",
        data = pts,
        color = "#22d3ee",
        lineWidth = 2.5,
        marker = list(radius=4, fillColor="#22d3ee", lineColor="#fff", lineWidth=1.5),
        fillColor = list(
          linearGradient = list(x1=0,y1=0,x2=0,y2=1),
          stops = list(list(0,"rgba(34,211,238,.26)"),list(1,"rgba(34,211,238,0)"))
        )
      ) |>
      hc_tooltip(
        backgroundColor="#141824", borderColor="#2e3855", borderRadius=10,
        style=list(color="#e2e8f0", fontSize="12px"),
        useHTML = TRUE,
        formatter = JS("function(){
          var p = this.point;
          return '<div style=\"font-family:monospace;padding:4px 0\">' +
            '<b style=\"color:#4f8ef7;font-size:14px\">' + this.y + '</b> artículos<br>' +
            '<span style=\"color:#94a3b8;font-size:10px\">Prom. citas: </span>' +
            '<b style=\"color:#34d399\">' + (p.avg_cit||0) + '</b><br>' +
            '<span style=\"color:#64748b;font-size:10px;display:block;margin-top:4px\">📌 ' + (p.top_art||'') + '</span>' +
            '</div>';
        }")
      ) |>
      hc_legend(enabled=FALSE) |>
      hc_credits(enabled=FALSE)
  })
  
  # ── Gráfico donut con click ────────────────────────
  output$chart_temas <- renderHighchart({
    df <- datos_filtrados()
    req(!is.null(df), nrow(df) > 0)
    
    by_tema <- df |>
      filter(!is.na(topic_label)) |>
      count(topic_label) |>
      arrange(desc(n))
    
    total_art <- sum(by_tema$n)
    
    colores <- c(
      "Machine Learning"="#4f8ef7",
      "Estadística"     ="#34d399",
      "IA Generativa"   ="#a78bfa",
      "Otros"           ="#fb923c"
    )
    
    highchart() |>
      hc_chart(type="pie", backgroundColor="transparent",
               animation=list(duration=500),
               events=list(
                 click=JS("function(e){ if(e.point){ Shiny.setInputValue('chart_temas_click', {name:e.point.name, ts:Date.now()}, {priority:'event'}); } }")
               )) |>
      hc_plotOptions(pie=list(
        innerSize="58%",
        cursor="pointer",
        dataLabels=list(
          enabled=TRUE,
          distance=14,
          style=list(color="#94a3b8",fontSize="11px",fontWeight="400",textOutline="none")
        ),
        point=list(events=list(
          click=JS("function(){ Shiny.setInputValue('chart_temas_click', {name:this.name, ts:Date.now()}, {priority:'event'}); }")
        )),
        states=list(hover=list(brightness=0.08))
      )) |>
      hc_add_series(
        name="Artículos",
        data=lapply(seq_len(nrow(by_tema)), function(i){
          list(
            name  = by_tema$topic_label[i],
            y     = by_tema$n[i],
            color = unname(colores[by_tema$topic_label[i]])
          )
        })
      ) |>
      hc_subtitle(
        text    = paste0('<span style="font-size:22px;font-weight:700;color:#e2e8f0">', total_art, '</span><br><span style="font-size:10px;color:#64748b">artículos</span>'),
        useHTML = TRUE,
        floating= TRUE,
        align   = "center", verticalAlign="middle", y=10
      ) |>
      hc_tooltip(
        backgroundColor="#141824", borderColor="#2e3855", borderRadius=10,
        style=list(color="#e2e8f0",fontSize="12px"),
        pointFormat="<b>{point.y}</b> artículos ({point.percentage:.1f}%)"
      ) |>
      hc_legend(itemStyle=list(color="#94a3b8",fontSize="11px"),
                itemHoverStyle=list(color="#e2e8f0")) |>
      hc_credits(enabled=FALSE)
  })
  
  # ── Histograma de citas ────────────────────────────
  output$chart_citas <- renderHighchart({
    df <- datos_filtrados()
    req(!is.null(df), nrow(df) > 0)
    
    citas_ok <- df$citations[!is.na(df$citations) & df$citations <= 30]
    if (length(citas_ok) == 0) return(highchart())
    h <- hist(citas_ok, breaks=0:31, plot=FALSE)
    
    highchart() |>
      hc_chart(type="column", backgroundColor="transparent", animation=list(duration=500)) |>
      hc_xAxis(
        title=list(text=NULL),
        categories=as.character(0:30),
        labels=list(style=list(color="#64748b",fontSize="10px")),
        lineColor="#232a3e"
      ) |>
      hc_yAxis(
        title=list(text=NULL),
        labels=list(style=list(color="#64748b",fontSize="10px")),
        gridLineColor="#1c2235"
      ) |>
      hc_add_series(
        name="Artículos",
        data=as.list(h$counts),
        color="#fb923c",
        borderColor="transparent",
        borderRadius=3,
        pointPadding=0.05, groupPadding=0
      ) |>
      hc_tooltip(
        backgroundColor="#141824", borderColor="#2e3855", borderRadius=10,
        style=list(color="#e2e8f0",fontSize="12px"),
        pointFormat="<b>{point.y}</b> artículos con {point.x} cita(s)"
      ) |>
      hc_legend(enabled=FALSE) |>
      hc_credits(enabled=FALSE)
  })
  
  # ── Top autores con click ──────────────────────────
  output$chart_autores <- renderHighchart({
    df <- datos_filtrados()
    req(!is.null(df), nrow(df) > 0)
    
    top_aut <- df |>
      filter(!is.na(authors_raw)) |>
      separate_rows(authors_raw, sep="; ") |>
      mutate(authors_raw=trimws(authors_raw)) |>
      filter(nchar(authors_raw) > 2) |>
      count(authors_raw, sort=TRUE) |>
      slice_head(n=10) |>
      arrange(n)
    
    highchart() |>
      hc_chart(type="bar", backgroundColor="transparent", animation=list(duration=500)) |>
      hc_xAxis(
        categories=top_aut$authors_raw,
        labels=list(style=list(color="#94a3b8",fontSize="11px")),
        lineColor="#232a3e"
      ) |>
      hc_yAxis(
        title=list(text=NULL),
        labels=list(style=list(color="#64748b",fontSize="10px")),
        gridLineColor="#1c2235"
      ) |>
      hc_add_series(
        name="Artículos",
        data=as.list(top_aut$n),
        color="#34d399",
        borderRadius=4,
        cursor="pointer",
        point=list(events=list(
          click=JS("function(){ Shiny.setInputValue('chart_autores_click', {category:this.category, ts:Date.now()}, {priority:'event'}); }")
        ))
      ) |>
      hc_tooltip(
        backgroundColor="#141824", borderColor="#2e3855", borderRadius=10,
        style=list(color="#e2e8f0",fontSize="12px"),
        pointFormat="<b>{point.category}</b><br>{point.y} artículo(s)"
      ) |>
      hc_legend(enabled=FALSE) |>
      hc_credits(enabled=FALSE)
  })

  output$chart_impacto <- renderHighchart({
    df <- datos_filtrados()
    req(!is.null(df), nrow(df) > 0)
    df <- df |>
      filter(!is.na(views), !is.na(downloads)) |>
      mutate(
        topic_label = ifelse(is.na(topic_label), "Otros", topic_label),
        z = pmax(ifelse(is.na(citations), 0, citations), 1)
      ) |>
      arrange(desc(views)) |>
      slice_head(n = 500)
    req(nrow(df) > 0)

    colores <- c(
      "Machine Learning"="#4f8ef7",
      "Estadística"     ="#34d399",
      "IA Generativa"   ="#a78bfa",
      "Otros"           ="#fb923c"
    )

    hc <- highchart() |>
      hc_chart(type="bubble", zoomType="xy", backgroundColor="transparent", animation=list(duration=500)) |>
      hc_xAxis(title=list(text="Visualizaciones"), labels=list(style=list(color="#64748b",fontSize="10px")), gridLineColor="#1c2235") |>
      hc_yAxis(title=list(text="Descargas"), labels=list(style=list(color="#64748b",fontSize="10px")), gridLineColor="#1c2235") |>
      hc_tooltip(
        useHTML=TRUE, backgroundColor="#141824", borderColor="#2e3855", borderRadius=10,
        style=list(color="#e2e8f0",fontSize="12px"),
        pointFormat="<b>{point.name}</b><br>Vistas: {point.x}<br>Descargas: {point.y}<br>Citas: {point.z}"
      ) |>
      hc_legend(itemStyle=list(color="#94a3b8",fontSize="11px"), itemHoverStyle=list(color="#e2e8f0")) |>
      hc_credits(enabled=FALSE)

    for (tema in unique(df$topic_label)) {
      d <- df[df$topic_label == tema, ]
      hc <- hc |>
        hc_add_series(
          name = tema,
          data = lapply(seq_len(nrow(d)), function(i) {
            list(x=d$views[i], y=d$downloads[i], z=d$z[i], name=str_trunc(d$title[i], 70))
          }),
          color = unname(colores[tema])
        )
    }
    hc
  })

  output$chart_metricas_tema <- renderHighchart({
    df <- datos_filtrados()
    req(!is.null(df), nrow(df) > 0)
    by_tema <- df |>
      mutate(topic_label = ifelse(is.na(topic_label), "Otros", topic_label)) |>
      group_by(topic_label) |>
      summarise(
        citas = round(mean(citations, na.rm=TRUE), 2),
        descargas = round(mean(downloads, na.rm=TRUE), 1),
        vistas = round(mean(views, na.rm=TRUE) / 10, 1),
        refs = round(mean(n_references, na.rm=TRUE), 1),
        .groups="drop"
      )

    highchart() |>
      hc_chart(type="column", backgroundColor="transparent", animation=list(duration=500)) |>
      hc_xAxis(categories=by_tema$topic_label, labels=list(style=list(color="#94a3b8",fontSize="10px"))) |>
      hc_yAxis(title=list(text=NULL), labels=list(style=list(color="#64748b",fontSize="10px")), gridLineColor="#1c2235") |>
      hc_add_series(name="Citas", data=as.list(by_tema$citas), color="#34d399") |>
      hc_add_series(name="Descargas", data=as.list(by_tema$descargas), color="#fb923c") |>
      hc_add_series(name="Vistas / 10", data=as.list(by_tema$vistas), color="#4f8ef7") |>
      hc_add_series(name="Referencias", data=as.list(by_tema$refs), color="#a78bfa") |>
      hc_tooltip(backgroundColor="#141824", borderColor="#2e3855", borderRadius=10, style=list(color="#e2e8f0",fontSize="12px")) |>
      hc_legend(itemStyle=list(color="#94a3b8",fontSize="11px"), itemHoverStyle=list(color="#e2e8f0")) |>
      hc_credits(enabled=FALSE)
  })

  calidad_df <- reactive({
    df <- todos()
    df |>
      filter(
        is.na(publication_date) |
          is.na(topic_label) |
          is.na(citations) | is.na(downloads) | is.na(views) | is.na(n_references)
      )
  })

  output$quality_dates <- renderUI({
    df <- todos()
    registros_2026 <- sum(!is.na(df$publication_date) & format(df$publication_date, "%Y") == "2026")
    div(class="quality-card",
        div(class="quality-label","Rango de fechas"),
        div(class="quality-value", paste(min(df$publication_date, na.rm=TRUE), "→", max(df$publication_date, na.rm=TRUE))),
        div(class="quality-note quality-ok",
            paste(registros_2026, "registro(s) de 2026 cargado(s)"))
    )
  })

  output$quality_missing <- renderUI({
    df <- todos()
    faltan <- colSums(is.na(df[, c("citations","downloads","views","n_references")]))
    div(class="quality-card",
        div(class="quality-label","Métricas faltantes"),
        div(class="quality-value", sum(faltan)),
        div(class="quality-note", paste(names(faltan), faltan, collapse=" · "))
    )
  })

  output$quality_topics <- renderUI({
    df <- todos()
    sin_tema <- sum(is.na(df$topic_label) | df$topic_label == "")
    div(class="quality-card",
        div(class="quality-label","Categorías"),
        div(class="quality-value", length(unique(na.omit(df$topic_label)))),
        div(class=ifelse(sin_tema > 0, "quality-note quality-warn", "quality-note quality-ok"),
            paste(sin_tema, "registro(s) sin temática"))
    )
  })

  output$quality_count <- renderText({
    paste0(nrow(calidad_df()), " registros")
  })

  output$tabla_calidad <- renderDT({
    tbl <- calidad_df() |>
      mutate(
        Fecha = format(publication_date, "%Y-%m-%d"),
        Motivo = paste(
          ifelse(is.na(publication_date), "fecha", ""),
          ifelse(is.na(topic_label) | topic_label == "", "tema", ""),
          ifelse(is.na(citations) | is.na(downloads) | is.na(views) | is.na(n_references), "métricas", "")
        )
      ) |>
      select(paper_id, Titulo=title, Fecha, Tematica=topic_label, Citas=citations, Descargas=downloads, Vistas=views, Referencias=n_references, Motivo)
    datatable(tbl, rownames=FALSE, options=list(pageLength=6, scrollX=TRUE, dom="tip"), class="cell-border")
  })

  output$insight_summary <- renderText({
    df <- datos_filtrados()
    if (is.null(df) || nrow(df) == 0) return("No hay registros con los filtros actuales.")

    tema_top <- df |>
      mutate(topic_label = ifelse(is.na(topic_label) | topic_label == "", "Sin tema", topic_label)) |>
      count(topic_label, sort=TRUE) |>
      slice_head(n=1)

    best_citas <- df |>
      filter(!is.na(citations)) |>
      arrange(desc(citations)) |>
      slice_head(n=1)

    prom_citas <- round(mean(df$citations, na.rm=TRUE), 2)
    prom_descargas <- round(mean(df$downloads, na.rm=TRUE), 1)

    if (nrow(best_citas) == 0) {
      paste0(
        "La seleccion contiene ", format(nrow(df), big.mark=","),
        " articulos. La tematica dominante es ", tema_top$topic_label,
        " con ", tema_top$n, " registros. Promedio de descargas: ", prom_descargas, "."
      )
    } else {
      paste0(
        "La seleccion contiene ", format(nrow(df), big.mark=","),
        " articulos. La tematica dominante es ", tema_top$topic_label,
        " con ", tema_top$n, " registros. El articulo con mas citas es \"",
        str_trunc(best_citas$title[1], 90), "\" (", best_citas$citations[1],
        " citas). Promedios: ", prom_citas, " citas y ", prom_descargas, " descargas por articulo."
      )
    }
  })

  output$tabla_ranking <- renderDT({
    df <- datos_filtrados()
    req(!is.null(df))
    metric <- input$ranking_metric %||% "citations"
    metric_labels <- c(
      citations="Citas", downloads="Descargas", views="Visualizaciones",
      n_references="Referencias", n_authors="Autores"
    )

    tbl <- df |>
      mutate(metric_value = .data[[metric]]) |>
      filter(!is.na(metric_value)) |>
      arrange(desc(metric_value)) |>
      slice_head(n=10) |>
      mutate(
        Fecha = format(publication_date, "%Y-%m-%d"),
        Tema = ifelse(is.na(topic_label) | topic_label == "", "Sin tema", topic_label)
      ) |>
      transmute(
        Titulo = str_trunc(title, 72),
        Tema,
        Fecha,
        Valor = metric_value,
        DOI = doi
      )

    datatable(
      tbl, rownames=FALSE,
      options=list(pageLength=10, scrollX=TRUE, dom="tip"),
      class="cell-border",
      caption=htmltools::tags$caption(
        style="caption-side:top;text-align:left;color:#a8b5c0;font-size:11px;",
        paste("Ordenado por", unname(metric_labels[metric]))
      )
    )
  })

  outliers_df <- reactive({
    df <- datos_filtrados()
    req(!is.null(df))
    if (nrow(df) == 0) return(df[0, ])

    q_citas <- quantile(df$citations, .95, na.rm=TRUE)
    q_descargas <- quantile(df$downloads, .95, na.rm=TRUE)
    q_vistas <- quantile(df$views, .95, na.rm=TRUE)

    df |>
      mutate(
        Motivo = case_when(
          is.na(publication_date) ~ "sin fecha",
          is.na(topic_label) | topic_label == "" ~ "sin tema",
          is.na(citations) | is.na(downloads) | is.na(views) | is.na(n_references) ~ "metricas incompletas",
          !is.na(citations) & citations >= q_citas ~ "citas muy altas",
          !is.na(downloads) & downloads >= q_descargas ~ "descargas muy altas",
          !is.na(views) & views >= q_vistas ~ "vistas muy altas",
          TRUE ~ NA_character_
        )
      ) |>
      filter(!is.na(Motivo)) |>
      arrange(desc(coalesce(citations, 0)), desc(coalesce(downloads, 0))) |>
      slice_head(n=12)
  })

  output$outliers_count <- renderText({
    paste0(nrow(outliers_df()), " items")
  })

  output$tabla_outliers <- renderDT({
    tbl <- outliers_df() |>
      mutate(Fecha = format(publication_date, "%Y-%m-%d")) |>
      transmute(
        Titulo = str_trunc(title, 58),
        Fecha,
        Tema = ifelse(is.na(topic_label) | topic_label == "", "Sin tema", topic_label),
        Citas = citations,
        Descargas = downloads,
        Motivo
      )

    datatable(tbl, rownames=FALSE, options=list(pageLength=6, scrollX=TRUE, dom="tip"), class="cell-border")
  })
  
  # ── Tabla ──────────────────────────────────────────
  output$tabla_count <- renderText({
    df <- datos_filtrados()
    if (is.null(df)) return("0")
    paste0(format(nrow(df), big.mark=","), " registros")
  })
  
  tabla_df <- reactive({
    df <- datos_filtrados()
    req(!is.null(df))
    df |>
      mutate(
        Fecha    = format(publication_date, "%Y-%m-%d"),
        Tematica = sapply(ifelse(is.na(topic_label),"Otros",topic_label), badge_tema),
        DOI_link = ifelse(!is.na(doi),
                          sprintf('<a href="%s" target="_blank" style="color:var(--acc);font-size:11px;">🔗</a>', doi),"—"),
        Titulo   = ifelse(!is.na(url),
                          sprintf('<a href="%s" target="_blank" style="color:var(--text);text-decoration:none;font-weight:500;">%s</a>', url, title),
                          title)
      ) |>
      select(Titulo, Autores=authors_raw, Fecha, Tematica, DOI=DOI_link,
             Citas=citations, Descargas=downloads)
  })
  
  output$tabla_articulos <- renderDT({
    datatable(
      tabla_df(),
      escape    = FALSE,
      rownames  = FALSE,
      selection = "single",
      options   = list(
        pageLength = 10,
        scrollX    = TRUE,
        dom        = "ftip",
        language   = list(
          search   = "Buscar:",
          paginate = list(previous="Ant.",`next`="Sig."),
          info     = "Mostrando _START_–_END_ de _TOTAL_"
        ),
        columnDefs = list(
          list(width="36%", targets=0),
          list(width="22%", targets=1),
          list(className="dt-center", targets=2:6)
        )
      ),
      class = "cell-border"
    )
  })
  
  # ── Click en fila → preview abstract ──────────────
  observeEvent(input$tabla_articulos_rows_selected, {
    rv$fila_sel <- input$tabla_articulos_rows_selected
  })
  
  output$abstract_panel_ui <- renderUI({
    sel <- rv$fila_sel
    if (is.null(sel) || length(sel) == 0) return(NULL)
    
    df  <- datos_filtrados()
    fila <- df[sel, ]
    
    abs_txt <- if (!is.na(fila$abstract) && nchar(fila$abstract) > 0) fila$abstract else "Abstract no disponible."
    tema    <- if (!is.na(fila$topic_label)) fila$topic_label else "Otros"
    
    div(class="table-card", style="border-color:var(--acc);",
        div(class="table-hdr",
            span(class="table-title","📖 Abstract"),
            actionButton("close_abstract","✕ cerrar",
                         style="background:transparent;border:1px solid var(--border);color:var(--muted);font-size:11px;border-radius:6px;padding:3px 10px;cursor:pointer;")
        ),
        div(class="abstract-panel",
            div(class="abstract-title", fila$title),
            div(class="abstract-meta",
                span(class="abstract-stat", paste0("👤 ", fila$authors_raw |> str_trunc(60))),
                span(class="abstract-stat", paste0("📅 ", fila$publication_date)),
                span(class="abstract-stat", paste0("⭐ ", ifelse(is.na(fila$citations),  "—", fila$citations), " citas")),
                span(class="abstract-stat", paste0("⬇ ",  ifelse(is.na(fila$downloads), "—", fila$downloads), " descargas")),
                span(class="abstract-stat", paste0("📚 ", ifelse(is.na(fila$n_references),"—", fila$n_references), " refs")),
                HTML(badge_tema(tema))
            ),
            p(abs_txt, style="margin:0;")
        )
    )
  })
  
  observeEvent(input$close_abstract, { rv$fila_sel <- NULL })
  
  # ── SCRAPING ───────────────────────────────────────
  observeEvent(input$btn_scrape, {
    target_year <- as.integer(input$scrape_year %||% 2026)
    target_volume <- as.integer(input$scrape_volume %||% 28)
    target_issue <- input$scrape_issue %||% "all"
    target_limit <- as.integer(input$scrape_limit %||% 20)
    scrape_mode <- input$scrape_mode %||% "new"
    issue_txt <- if (target_issue == "all") "todos los issues" else paste("issue", target_issue)
    mode_txt <- if (scrape_mode == "new") "solo URLs nuevas" else "reconsultar selección"

    rv$scrape_msg  <- list(
      texto=paste0("⟳ Buscando Entropy ", target_year, ", vol. ", target_volume, ", ", issue_txt, " (", mode_txt, ")…"),
      clase="scrape-info")
    rv$datos_nuevos <- NULL
    rv$datos_verificados <- NULL
    
    withProgress(message=paste0("Scrapeando Entropy ", target_year, "…"), value=0, {
      setProgress(0.1, detail=paste0("Leyendo vol. ", target_volume, " / ", issue_txt))
      urls_vol28 <- obtener_urls_volumen(target_volume, target_issue)
      if (!is.na(target_limit) && target_limit > 0) urls_vol28 <- head(urls_vol28, target_limit)
      
      if (length(urls_vol28) == 0) {
        rv$scrape_msg <- list(
          texto=paste0("⚠ No se encontraron URLs para Entropy ", target_year, ", vol. ", target_volume, ", ", issue_txt, "."),
          clase="scrape-warn")
        setProgress(1); return()
      }
      
      con        <- get_con()
      urls_en_bd <- dbGetQuery(con,"SELECT url FROM papers")$url
      dbDisconnect(con)
      urls_nuevas <- if (scrape_mode == "new") setdiff(urls_vol28, urls_en_bd) else urls_vol28
      
      if (length(urls_nuevas) == 0) {
        con <- get_con()
        rv$datos_verificados <- dbGetQuery(con, "SELECT * FROM papers WHERE publication_date >= '2025/01/01' ORDER BY paper_id DESC LIMIT 5")
        dbDisconnect(con)
        rv$db_version <- rv$db_version + 1
        rv$scrape_msg <- list(
          texto=paste0("✔ No hay URLs nuevas para Entropy ", target_year, ", vol. ", target_volume, ", ", issue_txt, ". Cambia el modo a 'Reconsultar selección' si quieres actualizar esos artículos."),
          clase="scrape-ok")
        setProgress(1); return()
      }
      
      total  <- length(urls_nuevas)
      nuevos <- list()
      for (i in seq_along(urls_nuevas)) {
        setProgress(0.3 + 0.65*(i/total), detail=paste0("Artículo ",i," de ",total))
        Sys.sleep(runif(1,0.15,0.45))
        res <- tryCatch(scrapear_articulo_mdpi(urls_nuevas[i], anio_objetivo=target_year), error=function(e) NULL)
        if (!is.null(res)) nuevos[[length(nuevos)+1]] <- res
      }
      
      if (length(nuevos) == 0) {
        con <- get_con()
        rv$datos_verificados <- dbGetQuery(con, "SELECT * FROM papers WHERE publication_date >= '2025/01/01' ORDER BY paper_id DESC LIMIT 5")
        dbDisconnect(con)
        rv$db_version <- rv$db_version + 1
        rv$scrape_msg <- list(
          texto=paste0("⚠ ",total," URLs seleccionadas, pero ninguna pudo leerse como artículo de ", target_year, "."),
          clase="scrape-warn")
        setProgress(1); return()
      }
      
      df_nuevos <- bind_rows(nuevos)
      con <- get_con()
      urls_en_bd_actual <- dbGetQuery(con,"SELECT url FROM papers")$url
      df_insert <- df_nuevos |> filter(!url %in% urls_en_bd_actual)
      df_update <- df_nuevos |> filter(url %in% urls_en_bd_actual)

      if (nrow(df_update) > 0) {
        for (j in seq_len(nrow(df_update))) {
          dbExecute(con,
                    "UPDATE papers SET title=?, publication_date=?, year=?, doi=?, abstract=?, authors_raw=?, n_authors=?, citations=?, downloads=?, views=?, n_references=?, topic_label=? WHERE url=?",
                    params=list(df_update$title[j], df_update$publication_date[j], df_update$year[j],
                                df_update$doi[j], df_update$abstract[j], df_update$authors_raw[j],
                                df_update$n_authors[j], df_update$citations[j], df_update$downloads[j],
                                df_update$views[j], df_update$n_references[j], df_update$topic_label[j],
                                df_update$url[j]))
        }
      }

      if (nrow(df_insert) > 0) {
        max_id <- dbGetQuery(con,"SELECT MAX(paper_id) AS m FROM papers")$m
        df_insert$paper_id <- seq(max_id+1, max_id+nrow(df_insert))
        dbWriteTable(con,"papers",df_insert,append=TRUE)
      }
      dbDisconnect(con)
      
      rv$datos_nuevos <- df_nuevos
      rv$db_version <- rv$db_version + 1
      rv$filtros$fecha_inicio <- as.Date("2025-01-01")
      rv$filtros$fecha_fin <- as.Date(paste0(target_year, "-12-31"))
      updateTextInput(session, "fecha_inicio", value = "2025-01-01")
      updateTextInput(session, "fecha_fin", value = paste0(target_year, "-12-31"))
      rv$scrape_msg   <- list(
        texto=paste0("✅ ", nrow(df_insert), " nuevo(s), ", nrow(df_update), " actualizado(s) para Entropy ", target_year, ", vol. ", target_volume, ", ", issue_txt, "."),
        clase="scrape-ok")
      setProgress(1)
    })
  })
  
  output$scrape_status <- renderUI({
    m <- rv$scrape_msg
    tags$span(class=m$clase, m$texto)
  })
  
  output$hay_nuevos <- reactive({
    !is.null(rv$datos_nuevos) && nrow(rv$datos_nuevos) > 0
  })
  outputOptions(output, "hay_nuevos", suspendWhenHidden=FALSE)

  output$hay_scrape_result <- reactive({
    (!is.null(rv$datos_nuevos) && nrow(rv$datos_nuevos) > 0) ||
      (!is.null(rv$datos_verificados) && nrow(rv$datos_verificados) > 0)
  })
  outputOptions(output, "hay_scrape_result", suspendWhenHidden=FALSE)

  scrape_result_df <- reactive({
    if (!is.null(rv$datos_nuevos) && nrow(rv$datos_nuevos) > 0) rv$datos_nuevos else rv$datos_verificados
  })
  
  output$nuevos_count <- renderText({
    df <- scrape_result_df()
    if (is.null(df)) return("")
    paste0(nrow(df)," registro(s)")
  })

  output$nuevos_count_top <- renderText({
    df <- scrape_result_df()
    if (is.null(df)) return("")
    paste0(nrow(df)," registro(s)")
  })
  
  output$tabla_nuevos <- renderDT({
    df <- scrape_result_df()
    req(!is.null(df), nrow(df) > 0)
    tbl <- df |>
      mutate(
        Tematica=sapply(ifelse(is.na(topic_label),"Otros",topic_label), badge_tema),
        DOI_link=ifelse(!is.na(doi),
                        sprintf('<a href="%s" target="_blank" style="color:#4f8ef7;font-size:11px;">🔗</a>',doi),"—"),
        Titulo=ifelse(!is.na(url),
                      sprintf('<a href="%s" target="_blank" style="color:var(--text);">%s</a>',url,title),title)
      ) |>
      select(Titulo,Autores=authors_raw,Fecha=publication_date,
             Tematica,DOI=DOI_link,Citas=citations,Descargas=downloads)
    datatable(tbl,escape=FALSE,rownames=FALSE,
              options=list(pageLength=5,scrollX=TRUE,dom="fti",
                           language=list(search="Buscar:",info="Mostrando _START_–_END_ de _TOTAL_")),
              class="cell-border")
  })

  output$tabla_nuevos_top <- renderDT({
    df <- scrape_result_df()
    req(!is.null(df), nrow(df) > 0)
    tbl <- df |>
      mutate(
        Tema=sapply(ifelse(is.na(topic_label),"Otros",topic_label), badge_tema),
        DOI_link=ifelse(!is.na(doi),
                        sprintf('<a href="%s" target="_blank" style="color:var(--acc);font-size:11px;">link</a>',doi),""),
        Titulo=ifelse(!is.na(url),
                      sprintf('<a href="%s" target="_blank" style="color:var(--text);">%s</a>',url,title),title),
        Fecha=as.character(publication_date)
      ) |>
      select(Titulo, Autores=authors_raw, Fecha, Tema, DOI=DOI_link, Citas=citations, Descargas=downloads, Vistas=views)
    datatable(tbl, escape=FALSE, rownames=FALSE,
              options=list(pageLength=6, scrollX=TRUE, dom="fti",
                           language=list(search="Buscar:", info="Mostrando _START_–_END_ de _TOTAL_")),
              class="cell-border")
  })
}

shinyApp(ui=ui, server=server)
