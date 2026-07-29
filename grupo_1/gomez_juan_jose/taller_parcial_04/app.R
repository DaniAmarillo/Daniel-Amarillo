
library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(dplyr)
library(DT)
library(tibble)
library(highcharter)
library(rvest)
library(httr)
library(stringr)
library(purrr)
library(Matrix)
library(irlba)
library(SnowballC)


raiz_proyecto <- function() {
  for (fr in rev(sys.frames())) {
    if (!is.null(fr$ofile)) return(dirname(normalizePath(fr$ofile)))
  }
  getwd()
}
RAIZ <- raiz_proyecto()
ruta <- function(...) file.path(RAIZ, ...)

if (!file.exists(ruta("R", "text_processing.R")))
  stop("No se encontró la carpeta R/ junto a app.R.\n",
       "  Carpeta detectada: ", RAIZ, "\n",
       "  La app necesita el proyecto completo: app.R, R/, data/ e index/.\n",
       "  Lánzala con shiny::runApp(\"ruta/al/taller_4\") o con el botón Run App.")

source(ruta("R", "text_processing.R"))
source(ruta("R", "retrieval.R"))

DB_PATH    <- ruta("data", "jmlr_q1_2025.sqlite")
INDEX_PATH <- ruta("index", "indice_jmlr.rds")

URL_BASE <- "https://www.jmlr.org"
UA <- "Mineria-de-Datos-UNAL-Taller4 (research, contact: jugomezgar@unal.edu.co)"
httr::set_config(httr::user_agent(UA))

PALETA <- c("#2C3E50", "#18BC9C", "#E74C3C", "#3498DB", "#F39C12", "#9B59B6")

# --- Índice precalculado (carga única, compartida por todas las sesiones) ----
IDX <- if (file.exists(INDEX_PATH)) readRDS(INDEX_PATH) else NULL

ETIQUETAS_ESTRATEGIA <- c(
  "BM25 (léxica, sin reducción)" = "bm25",
  "LSA (TF-IDF + Truncated SVD)" = "lsa",
  "Híbrida (fusión RRF)"         = "hibrido"
)


`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}



clasificar_topic <- function(title, abstract = NA) {
  ab <- if (length(abstract) == 0 || is.na(abstract)) "" else abstract
  texto <- tolower(paste(title, ab))

  re_genai <- "generative\\s+(adversarial|model)|diffusion\\s+(model|probabilistic)|score-based|\\bgan\\b|\\bllm\\b|large\\s+language\\s+model|text-to-image|flow\\s+matching|bitnet|vision\\s+language|language\\s+model|denoising"
  re_ml    <- "neural\\s+network|deep\\s+learning|reinforcement\\s+learning|bandit|representation\\s+learning|graph\\s+neural|federated|meta-learning|online\\s+learning|self-supervised|contrastive\\s+learning|transfer\\s+learning|domain\\s+adaptation|graph\\s+representation|conformal|adversarial\\s+robust|node\\s+classification"
  re_est   <- "bayesian|inference|regression|causal\\s+inference|causal\\s+(effect|graph|discovery)|hypothesis\\s+test|variational|mcmc|markov\\s+chain|posterior|gaussian\\s+process|quantile|nonparametric|non-parametric|hidden\\s+markov|sample\\s+complexity|confidence\\s+interval|change.point|change-point"

  if (grepl(re_genai, texto)) return("IA Generativa")
  if (grepl(re_ml,    texto)) return("Machine Learning")
  if (grepl(re_est,   texto)) return("Estadística")
  "Otros"
}

desglosar_autores <- function(authors_raw) {
  if (length(authors_raw) == 0 || is.na(authors_raw)) return(character(0))
  authors_raw |>
    stringr::str_split(",\\s*|\\s+and\\s+") |>
    purrr::pluck(1) |>
    stringr::str_squish() |>
    purrr::keep(\(x) nchar(x) > 0)
}

scrape_jmlr_index <- function(vol_url, vol_tag) {
  pagina <- rvest::read_html(vol_url)
  dts <- rvest::html_elements(pagina, "dl > dt")
  dds <- rvest::html_elements(pagina, "dl > dd")
  if (length(dts) == 0 || length(dts) != length(dds))
    stop("No se pudo parsear el índice de ", vol_tag,
         " (dts=", length(dts), ", dds=", length(dds), ").")

  registros <- purrr::map2_dfr(dts, dds, function(dt, dd) {
    title   <- stringr::str_squish(rvest::html_text2(dt))
    dd_text <- rvest::html_text2(dd)

    autores_node <- rvest::html_element(dd, "i")
    authors_raw  <- if (!is.na(autores_node))
      stringr::str_squish(rvest::html_text2(autores_node)) else NA_character_

    meta <- stringr::str_match(
      dd_text,
      "\\((\\d+)\\):(\\d+)\\s*[\u2212\u2013\u2014\\-]\\s*(\\d+),\\s*(\\d{4})")
    paper_num <- as.integer(meta[, 2])
    year      <- as.integer(meta[, 5])

    enlaces <- rvest::html_elements(dd, "a")
    href <- rvest::html_attr(enlaces, "href")
    txt  <- tolower(stringr::str_squish(rvest::html_text2(enlaces)))
    get_url <- function(clave) {
      idx <- which(stringr::str_detect(txt, clave))
      if (length(idx) >= 1) href[idx[1]] else NA_character_
    }
    abs_url <- get_url("^abs$"); pdf_url <- get_url("^pdf$")

    absol <- function(u) {
      if (is.na(u)) return(NA_character_)
      if (stringr::str_starts(u, "http")) u
      else paste0(URL_BASE, ifelse(stringr::str_starts(u, "/"), u, paste0("/", u)))
    }
    tibble::tibble(
      paper_num = paper_num, title = title, authors_raw = authors_raw,
      year = year, abs_url = absol(abs_url), pdf_url = absol(pdf_url))
  })

  registros |>
    dplyr::filter(!is.na(paper_num), !is.na(year)) |>
    dplyr::mutate(
      paper_id_str = stringr::str_extract(
        abs_url, paste0("(?<=/", vol_tag, "/)[\\w\\-]+(?=\\.html)")),
      doi       = paste0("10.5555/jmlr.", vol_tag, ".", paper_id_str),
      url       = abs_url,
      n_authors = stringr::str_count(authors_raw, ",") + 1L) |>
    dplyr::filter(!is.na(paper_id_str)) |>
    dplyr::arrange(paper_num)
}

fetch_abstract <- function(abs_url, intentos = 2) {
  if (is.na(abs_url)) return(NA_character_)
  for (i in seq_len(intentos)) {
    out <- tryCatch({
      p <- rvest::read_html(abs_url)
      ab <- p |> rvest::html_elements("p") |> rvest::html_text2() |>
        stringr::str_trim()
      if (length(ab) == 0) NA_character_ else ab[which.max(nchar(ab))]
    }, error = function(e) NA_character_)
    if (length(out) == 1 && !is.na(out)) return(out)
    Sys.sleep(1.5 * i)
  }
  NA_character_
}

# CORRECCIÓN respecto al Taller 2: la versión anterior insertaba sin
# `paper_id` y recuperaba la clave con last_insert_rowid(). Como paper_id es
# TEXT PRIMARY KEY (no un alias de rowid), eso dejaba la clave en NULL y
# rompía la relación con paper_authors. Ahora se inserta explícitamente.
insertar_papers <- function(con, df) {
  if (nrow(df) == 0) return(0L)
  n_new <- 0L
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    topic <- if (!is.null(r$topic_label) && !is.na(r$topic_label))
      r$topic_label else clasificar_topic(r$title, r$abstract)
    pid <- r$paper_id_str %||% r$doi
    res <- DBI::dbExecute(con,
      "INSERT OR IGNORE INTO papers
       (paper_id, paper_num, journal_name, title, publication_date, year, doi,
        url, abstract, authors_raw, n_authors, citations, downloads,
        n_references, topic_label)
       VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
      params = list(pid, r$paper_num,
                    "Journal of Machine Learning Research", r$title, NA,
                    r$year, r$doi, r$url, r$abstract %||% NA, r$authors_raw,
                    r$n_authors, NA, NA, NA, topic))
    if (res == 1) {
      n_new <- n_new + 1L
      autores <- desglosar_autores(r$authors_raw)
      for (k in seq_along(autores)) {
        DBI::dbExecute(con,
          "INSERT OR IGNORE INTO authors (author_name) VALUES (?)",
          params = list(autores[k]))
        aid <- DBI::dbGetQuery(con,
          "SELECT author_id FROM authors WHERE author_name = ?",
          params = list(autores[k]))$author_id[1]
        DBI::dbExecute(con,
          "INSERT OR IGNORE INTO paper_authors (paper_id, author_id, author_order)
           VALUES (?,?,?)", params = list(pid, aid, k))
      }
    }
  }
  n_new
}


css <- "
.app-header{background:#2C3E50;color:#fff;padding:16px 22px;border-radius:0 0 12px 12px;margin-bottom:18px}
.app-header h2{margin:0;font-weight:700}
.app-header p{margin:2px 0 0;opacity:.85;font-size:.9rem}
.ind-card{background:#fff;border:1px solid #e6ebf2;border-radius:12px;padding:14px 12px;height:100%;box-shadow:0 1px 2px rgba(0,0,0,.04);text-align:center}
.ind-value{font-size:1.6rem;font-weight:700;color:#2C3E50;line-height:1.1}
.ind-label{font-size:.78rem;color:#5b6b7f;margin-top:6px}
.ind-sub{font-size:.68rem;color:#95a5a6;margin-top:2px}
.callout{background:#f7f9fc;border-left:4px solid #18BC9C;border-radius:8px;padding:10px 14px;margin-top:6px;font-size:.9rem}
.res-card{background:#fff;border:1px solid #e6ebf2;border-left:4px solid #18BC9C;border-radius:10px;padding:12px 16px;margin-bottom:12px}
.res-rank{display:inline-block;background:#2C3E50;color:#fff;border-radius:6px;padding:1px 9px;font-weight:700;font-size:.85rem;margin-right:8px}
.res-title{font-weight:600;color:#2C3E50;font-size:1.02rem;line-height:1.3}
.res-meta{font-size:.78rem;color:#5b6b7f;margin:6px 0 4px}
.res-frag{font-size:.86rem;color:#34495e;background:#f7f9fc;border-radius:6px;padding:8px 10px;margin-top:6px}
.res-score{float:right;font-family:monospace;font-size:.85rem;color:#E74C3C;font-weight:700}
.badge-tema{background:#eef3f8;color:#2C3E50;border-radius:10px;padding:1px 8px;font-size:.72rem}
"

metric_card <- function(label, value, sub = "") {
  shiny::div(class = "ind-card",
    shiny::div(class = "ind-value", value),
    shiny::div(class = "ind-label", label),
    if (nzchar(sub)) shiny::div(class = "ind-sub", sub))
}

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(tags$style(HTML(css))),
  div(class = "app-header",
      h2("Dashboard analítico y buscador — JMLR"),
      p("Minería de Datos · Talleres 2 y 4 · Journal of Machine Learning Research")),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # ---- Controles del DASHBOARD (Taller 2) ----
      conditionalPanel(
        condition = "input.tab_principal == 'dashboard'",
        h4("Filtros"),
        sliderInput("anios", "Rango de años (fecha)",
                    min = 2025, max = 2026, value = c(2025, 2026),
                    step = 1, sep = ""),
        selectInput("temas", "Tema / categoría", choices = NULL, multiple = TRUE),
        textInput("autor", "Autor (contiene)"),
        textInput("doi", "DOI (contiene)"),
        textInput("kw", "Palabra clave / título"),
        hr(),
        h4("Actualización por scraping"),
        helpText("Busca artículos de 2026 en JMLR Vol. 27, los guarda en SQLite e",
                 "informa cuántos son nuevos. Si no hay nuevos, reconsulta los",
                 "últimos 5 artículos almacenados."),
        actionButton("scrape", "Buscar artículos nuevos (2026)",
                     class = "btn-primary", width = "100%"),
        br(), br(),
        actionButton("load2025", "Cargar histórico 2025 (Vol. 26)",
                     class = "btn-outline-secondary", width = "100%"),
        helpText("Usa este botón solo si la base está vacía.")
      ),

      # ---- Controles del BUSCADOR (Taller 4) ----
      conditionalPanel(
        condition = "input.tab_principal == 'buscador'",
        h4("Consulta"),
        textAreaInput("q", NULL, rows = 3,
                      placeholder = "Escribe una consulta en lenguaje natural, p. ej. 'uncertainty quantification with conformal prediction'"),
        actionButton("buscar", "Buscar", class = "btn-primary", width = "100%"),
        br(), br(),
        radioButtons("modo", "Modo",
                     choices = c("Una estrategia" = "una",
                                 "Comparar las dos principales" = "comparar"),
                     selected = "una"),
        conditionalPanel(
          condition = "input.modo == 'una'",
          selectInput("estrategia", "Estrategia de recuperación",
                      choices = ETIQUETAS_ESTRATEGIA, selected = "bm25")
        ),
        selectInput("topn", "Resultados a mostrar",
                    choices = c(5, 10, 20), selected = 10),
        hr(),
        uiOutput("info_indice")
      )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tab_principal", type = "tabs",

        # =====================  PESTAÑA 1 · DASHBOARD  =====================
        tabPanel(
          "Dashboard", value = "dashboard",
          br(),
          uiOutput("banner"),
          h4("Indicadores descriptivos"),
          uiOutput("indicadores"),
          br(),
          h4("Visualizaciones interactivas"),
          tabsetPanel(
            tabPanel("Por categoría",      highchartOutput("g_categoria", height = "380px")),
            tabPanel("Top autores",        highchartOutput("g_autores",   height = "420px")),
            tabPanel("N.º de autores",     highchartOutput("g_naut",      height = "380px")),
            tabPanel("Evolución temporal", highchartOutput("g_anio",      height = "380px"))
          ),
          br(),
          h4(textOutput("tabla_titulo")),
          DTOutput("tabla"),
          br(),
          uiOutput("scrape_result"),
          tableOutput("scrape_new")
        ),

        # =====================  PESTAÑA 2 · BUSCADOR  ======================
        tabPanel(
          "Buscador", value = "buscador",
          br(),
          uiOutput("buscador_estado"),
          uiOutput("resultados_busqueda")
        )
      )
    )
  )
)


server <- function(input, output, session) {

  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  session$onSessionEnded(function() DBI::dbDisconnect(con))

  data_version <- reactiveVal(0)
  scrape_msg   <- reactiveVal(NULL)
  scrape_tbl   <- reactiveVal(NULL)

  papers_all <- reactive({
    data_version()
    DBI::dbGetQuery(con, "SELECT * FROM papers")
  })

  observe({
    d <- papers_all()
    temas <- sort(unique(d$topic_label[!is.na(d$topic_label)]))
    updateSelectInput(session, "temas", choices = temas,
                      selected = intersect(isolate(input$temas), temas))
    if (nrow(d) > 0 && any(!is.na(d$year))) {
      yr <- range(d$year, na.rm = TRUE)
      lo <- min(2025, yr[1]); hi <- max(2026, yr[2])
      updateSliderInput(session, "anios", min = lo, max = hi, value = c(lo, hi))
    }
  })

  filtered <- reactive({
    d <- papers_all()
    if (nrow(d) == 0) return(d)
    d <- d[which(d$year >= input$anios[1] & d$year <= input$anios[2]), , drop = FALSE]
    if (length(input$temas) > 0)
      d <- d[which(d$topic_label %in% input$temas), , drop = FALSE]
    if (nzchar(input$autor))
      d <- d[which(grepl(tolower(input$autor), tolower(d$authors_raw), fixed = TRUE)), , drop = FALSE]
    if (nzchar(input$doi))
      d <- d[which(grepl(tolower(input$doi), tolower(ifelse(is.na(d$doi), "", d$doi)), fixed = TRUE)), , drop = FALSE]
    if (nzchar(input$kw)) {
      hay <- grepl(tolower(input$kw), tolower(d$title), fixed = TRUE) |
             grepl(tolower(input$kw), tolower(ifelse(is.na(d$abstract), "", d$abstract)), fixed = TRUE)
      d <- d[which(hay), , drop = FALSE]
    }
    d
  })

  output$banner <- renderUI({
    if (nrow(papers_all()) == 0)
      div(class = "alert alert-warning",
          strong("La base de datos está vacía. "),
          "Pulsa \"Cargar histórico 2025 (Vol. 26)\" para poblarla desde JMLR, ",
          "o reemplaza ", code(DB_PATH), " por tu base del Taller 1 ya poblada.")
  })

  fmt_avg <- function(x, dec = 2) {
    v <- suppressWarnings(mean(x, na.rm = TRUE))
    if (is.nan(v) || is.na(v)) "N/D"
    else formatC(round(v, dec), format = "f", digits = dec, big.mark = ",")
  }

  output$indicadores <- renderUI({
    d <- filtered(); total <- nrow(d)
    prom_aut <- if (total) fmt_avg(d$n_authors) else "0"
    autores_unicos <- if (total)
      length(unique(unlist(lapply(d$authors_raw, desglosar_autores)))) else 0
    n_temas <- if (total) length(unique(d$topic_label[!is.na(d$topic_label)])) else 0

    tema_top <- if (total) {
      tb <- sort(table(d$topic_label), decreasing = TRUE); names(tb)[1]
    } else "N/D"
    art_aut <- if (total) d$title[which.max(d$n_authors)] else "N/D"

    tagList(
      fluidRow(
        column(2, metric_card("Total de artículos", total)),
        column(2, metric_card("Prom. autores/artículo", prom_aut)),
        column(2, metric_card("Autores únicos", autores_unicos)),
        column(2, metric_card("Temáticas distintas", n_temas)),
        column(2, metric_card("Promedio de citas", fmt_avg(d$citations),
                              if (any(!is.na(d$citations))) "OpenAlex" else "JMLR no expone")),
        column(2, metric_card("Promedio de referencias", fmt_avg(d$n_references),
                              if (any(!is.na(d$n_references))) "OpenAlex" else "JMLR no expone"))
      ),
      fluidRow(
        column(6, div(class = "callout", HTML(paste0("<b>Tema más frecuente:</b> ", tema_top)))),
        column(6, div(class = "callout", HTML(paste0("<b>Artículo con más autores:</b> ", art_aut))))
      )
    )
  })

  output$g_categoria <- renderHighchart({
    d <- filtered(); if (nrow(d) == 0) return(highchart())
    tb <- as.data.frame(table(d$topic_label)); names(tb) <- c("tema", "n")
    hchart(tb, "column", hcaes(x = tema, y = n), name = "Artículos") |>
      hc_colors(PALETA) |> hc_title(text = "Artículos por categoría") |>
      hc_xAxis(title = list(text = "")) |> hc_yAxis(title = list(text = "Artículos")) |>
      hc_plotOptions(column = list(colorByPoint = TRUE)) |> hc_legend(enabled = FALSE)
  })

  output$g_autores <- renderHighchart({
    d <- filtered(); if (nrow(d) == 0) return(highchart())
    aut <- unlist(lapply(d$authors_raw, desglosar_autores))
    if (length(aut) == 0) return(highchart())
    tb <- sort(table(aut), decreasing = TRUE); tb <- head(tb, 15)
    df <- data.frame(autor = names(tb), n = as.integer(tb))
    df <- df[order(df$n), ]
    hchart(df, "bar", hcaes(x = autor, y = n), name = "Artículos") |>
      hc_colors(PALETA[4]) |> hc_title(text = "Top 15 autores") |>
      hc_xAxis(title = list(text = "")) |> hc_yAxis(title = list(text = "Artículos"))
  })

  output$g_naut <- renderHighchart({
    d <- filtered(); if (nrow(d) == 0) return(highchart())
    tb <- as.data.frame(table(d$n_authors)); names(tb) <- c("k", "n")
    hchart(tb, "column", hcaes(x = k, y = n), name = "Artículos") |>
      hc_colors(PALETA[2]) |>
      hc_title(text = "Distribución del número de autores por artículo") |>
      hc_xAxis(title = list(text = "Autores por artículo")) |>
      hc_yAxis(title = list(text = "Artículos")) |> hc_legend(enabled = FALSE)
  })

  output$g_anio <- renderHighchart({
    d <- filtered(); if (nrow(d) == 0) return(highchart())
    tb <- as.data.frame(table(d$year)); names(tb) <- c("anio", "n")
    hchart(tb, "column", hcaes(x = anio, y = n), name = "Publicaciones") |>
      hc_colors(PALETA[1]) |> hc_title(text = "Evolución temporal de publicaciones") |>
      hc_xAxis(title = list(text = "Año")) |>
      hc_yAxis(title = list(text = "Publicaciones")) |> hc_legend(enabled = FALSE)
  })

  output$tabla_titulo <- renderText(
    paste0("Artículos filtrados (", nrow(filtered()), ")"))

  output$tabla <- renderDT({
    d <- filtered()
    if (nrow(d) == 0)
      return(datatable(data.frame(Mensaje = "Sin resultados"), rownames = FALSE))
    disp <- data.frame(
      Titulo = d$title,
      Autores = d$authors_raw,
      `Año` = d$year,
      Tema = d$topic_label,
      DOI = ifelse(is.na(d$doi), "",
        sprintf('<a href="https://doi.org/%s" target="_blank">%s</a>', d$doi, d$doi)),
      Citas = d$citations,
      Descargas = d$downloads,
      Refs = d$n_references,
      Enlace = ifelse(is.na(d$url), "",
        sprintf('<a href="%s" target="_blank">abrir</a>', d$url)),
      check.names = FALSE, stringsAsFactors = FALSE)
    datatable(disp, escape = FALSE, rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE,
        language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_",
          info = "_START_–_END_ de _TOTAL_", paginate = list(
            previous = "Anterior", `next` = "Siguiente"))))
  })

  observeEvent(input$scrape, {
    scrape_msg(NULL); scrape_tbl(NULL)
    withProgress(message = "Consultando JMLR Vol. 27 (2026)…", value = 0.3, {
      df <- tryCatch(scrape_jmlr_index("https://www.jmlr.org/papers/v27/", "v27"),
                     error = function(e) {
                       scrape_msg(paste("No se pudo acceder al Vol. 27:", conditionMessage(e)))
                       NULL
                     })
      if (is.null(df)) return()
      if (nrow(df) == 0) {
        scrape_msg("El Vol. 27 aún no tiene artículos disponibles.")
        return()
      }
      existing <- DBI::dbGetQuery(con, "SELECT doi FROM papers")$doi
      nuevos <- df[!(df$doi %in% existing), , drop = FALSE]

      if (nrow(nuevos) > 0) {
        incProgress(0.3, message = sprintf("Enriqueciendo %d artículos nuevos…", nrow(nuevos)))
        nuevos$abstract <- vapply(nuevos$url, function(u) { Sys.sleep(0.4); fetch_abstract(u) }, character(1))
        nuevos$topic_label <- mapply(clasificar_topic, nuevos$title, nuevos$abstract)
        n <- insertar_papers(con, nuevos)
        data_version(data_version() + 1)
        scrape_msg(sprintf(paste("Se encontraron %d artículos nuevos de 2026 y se almacenaron en SQLite.",
                                 "Para que aparezcan en el buscador debe volver a ejecutarse build_index.R."), n))
        scrape_tbl(data.frame(Titulo = nuevos$title, Autores = nuevos$authors_raw,
                              `Año` = nuevos$year, Tema = nuevos$topic_label,
                              DOI = nuevos$doi, check.names = FALSE))
        showNotification(sprintf("%d artículos nuevos almacenados.", n), type = "message")
      } else {
        incProgress(0.4, message = "Sin novedades. Reconsultando los últimos 5…")
        ult <- DBI::dbGetQuery(con,
          "SELECT paper_id, url, title FROM papers ORDER BY rowid DESC LIMIT 5")
        verificados <- 0
        for (j in seq_len(nrow(ult))) {
          ab <- fetch_abstract(ult$url[j])
          if (!is.na(ab)) {
            DBI::dbExecute(con, "UPDATE papers SET abstract = ? WHERE paper_id = ?",
                           params = list(ab, ult$paper_id[j]))
            verificados <- verificados + 1
          }
        }
        data_version(data_version() + 1)
        scrape_msg(sprintf(
          "No se encontraron artículos nuevos. Se reconsultaron %d de los últimos 5 artículos para verificar actualizaciones.",
          verificados))
        showNotification("Sin artículos nuevos; se reconsultaron los últimos 5.", type = "warning")
      }
    })
  })

  observeEvent(input$load2025, {
    scrape_msg(NULL); scrape_tbl(NULL)
    withProgress(message = "Cargando JMLR Vol. 26 (2025)…", value = 0.4, {
      df <- tryCatch(scrape_jmlr_index("https://www.jmlr.org/papers/v26/", "v26"),
                     error = function(e) {
                       scrape_msg(paste("No se pudo acceder al Vol. 26:", conditionMessage(e)))
                       NULL
                     })
      if (is.null(df) || nrow(df) == 0) return()
      df$abstract <- NA_character_
      df$topic_label <- mapply(clasificar_topic, df$title, df$abstract)
      incProgress(0.4, message = "Insertando en SQLite…")
      n <- insertar_papers(con, df)
      data_version(data_version() + 1)
      scrape_msg(sprintf("Se cargaron %d artículos de 2025 (Vol. 26) en la base.", n))
      showNotification(sprintf("%d artículos de 2025 cargados.", n), type = "message")
    })
  })

  output$scrape_result <- renderUI({
    msg <- scrape_msg()
    if (is.null(msg)) return(NULL)
    div(class = "alert alert-info", msg)
  })

  output$scrape_new <- renderTable({
    req(scrape_tbl())
    scrape_tbl()
  })

  

  output$info_indice <- renderUI({
    if (is.null(IDX))
      return(div(class = "alert alert-danger",
                 "No se encontró ", code(INDEX_PATH),
                 ". Ejecuta ", code("Rscript build_index.R"), "."))
    p <- IDX$params
    div(class = "callout", style = "font-size:.78rem",
        HTML(sprintf(
          "<b>Índice precalculado</b><br>%d artículos · %s términos<br>
           SVD: %d componentes (%.0f %% de energía)<br>BM25: k1=%.1f, b=%.2f<br>
           Construido: %s",
          nrow(IDX$meta), format(length(IDX$vocab), big.mark = ","),
          p$k_svd, 100 * IDX$lsa$energia_retenida[p$k_svd], p$k1, p$b,
          format(p$construido, "%Y-%m-%d"))))
  })

  busqueda <- eventReactive(input$buscar, {
    req(IDX)
    q <- trimws(input$q %||% "")
    if (!nzchar(q)) return(NULL)
    n <- as.integer(input$topn)
    estrategias <- if (input$modo == "comparar") c("bm25", "lsa") else input$estrategia
    setNames(lapply(estrategias, function(e) {
      t0 <- Sys.time()
      r <- ejecutar_busqueda(IDX, q, e, n = n)
      r$ms <- 1000 * as.numeric(difftime(Sys.time(), t0, units = "secs"))
      r
    }), estrategias)
  }, ignoreNULL = FALSE)

  output$buscador_estado <- renderUI({
    if (is.null(IDX))
      return(div(class = "alert alert-danger",
                 "El índice no está disponible. Ejecuta ", code("Rscript build_index.R"),
                 " antes de lanzar la aplicación."))
    b <- busqueda()
    if (is.null(b))
      return(div(class = "callout",
                 "Escribe una consulta en lenguaje natural y pulsa ", strong("Buscar"),
                 ". El ranking se calcula sobre el título y el resumen de los ",
                 nrow(IDX$meta), " artículos del corpus."))
    info <- b[[1]]$info
    div(class = "callout",
        HTML(sprintf(
          "Consulta procesada: <b>%d</b> término(s) reconocido(s), <b>%d</b> fuera de vocabulario%s",
          info$n_tokens - info$n_oov, info$n_oov,
          if (info$n_oov > 0)
            paste0(" (<i>", paste(utils::head(info$terminos_oov, 8), collapse = ", "), "</i>)")
          else "")))
  })

  # --- Tarjeta HTML de un resultado ---
  tarjeta_resultado <- function(r, i, escala) {
    doi <- r$doi[i]
    enlace <- if (!is.na(doi) && nzchar(doi))
      sprintf('<a href="https://doi.org/%s" target="_blank">%s</a>', doi, doi)
    else if (!is.na(r$url[i]))
      sprintf('<a href="%s" target="_blank">%s</a>', r$url[i], r$url[i]) else "—"

    HTML(sprintf(
      '<div class="res-card">
         <span class="res-score">%s %.4f</span>
         <span class="res-rank">%d</span><span class="res-title">%s</span>
         <div class="res-meta">
           <b>Autores:</b> %s<br>
           <b>Publicación:</b> %s (JMLR, vol. 26) &nbsp;·&nbsp;
           <span class="badge-tema">%s</span> &nbsp;·&nbsp; <b>DOI:</b> %s
         </div>
         <div class="res-frag">%s</div>
       </div>',
      escala, r$puntaje[i], r$posicion[i], htmltools::htmlEscape(r$title[i]),
      htmltools::htmlEscape(r$authors_raw[i] %||% "—"),
      r$year[i], r$topic_label[i] %||% "—", enlace,
      htmltools::htmlEscape(r$fragmento[i])))
  }

  panel_estrategia <- function(res, clave) {
    r <- res$resultados
    etiqueta <- names(ETIQUETAS_ESTRATEGIA)[match(clave, ETIQUETAS_ESTRATEGIA)]
    escala <- switch(clave, bm25 = "BM25", lsa = "cos", hibrido = "RRF")
    if (nrow(r) == 0)
      return(tagList(h4(etiqueta),
                     div(class = "alert alert-warning",
                         "Ningún artículo obtuvo puntaje positivo para esta consulta.")))
    tagList(
      h4(etiqueta),
      p(class = "text-muted", style = "font-size:.8rem",
        sprintf("%d resultados · %.0f ms", nrow(r), res$ms)),
      lapply(seq_len(nrow(r)), function(i) tarjeta_resultado(r, i, escala))
    )
  }

  output$resultados_busqueda <- renderUI({
    b <- busqueda()
    if (is.null(b)) return(NULL)
    if (length(b) == 1) {
      panel_estrategia(b[[1]], names(b)[1])
    } else {
      fluidRow(
        column(6, panel_estrategia(b[[1]], names(b)[1])),
        column(6, panel_estrategia(b[[2]], names(b)[2])),
        column(12, div(class = "callout",
          HTML("<b>Nota:</b> los puntajes de BM25 (no acotado) y de LSA (coseno en [-1,1])
                están en escalas distintas y <b>no son comparables entre sí</b>.
                Compare el <i>orden</i> de los resultados, no los valores.")))
      )
    }
  })
}

shinyApp(ui, server)
