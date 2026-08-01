
DB_PATH <- "tog_q1_2025.sqlite"   

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
URL_BASE <- "https://www.jmlr.org"
UA <- "Mineria-de-Datos-UNAL-Taller2 (research, contact: jugomezgar@unal.edu.co)"
httr::set_config(httr::user_agent(UA))

PALETA <- c("#2C3E50", "#18BC9C", "#E74C3C", "#3498DB", "#F39C12", "#9B59B6")


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


insertar_papers <- function(con, df) {
  if (nrow(df) == 0) return(0L)
  n_new <- 0L
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    topic <- if (!is.null(r$topic_label) && !is.na(r$topic_label))
      r$topic_label else clasificar_topic(r$title, r$abstract)
    res <- DBI::dbExecute(con,
      "INSERT OR IGNORE INTO papers
       (journal_name, title, publication_date, year, doi, url, abstract,
        authors_raw, n_authors, citations, downloads, n_references, topic_label)
       VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)",
      params = list("Journal of Machine Learning Research", r$title, NA, r$year,
                    r$doi, r$url, r$abstract %||% NA, r$authors_raw, r$n_authors,
                    NA, NA, NA, topic))
    if (res == 1) {
      n_new <- n_new + 1L
      pid <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
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
      h2("Dashboard analítico — JMLR (proceso KDD)"),
      p("Minería de Datos · Taller 2 · Journal of Machine Learning Research")),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filtros"),
      sliderInput("anios", "Rango de años (fecha)",
                  min = 2025, max = 2026, value = c(2025, 2026), step = 1, sep = ""),
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
    mainPanel(
      width = 9,
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
        column(2, metric_card("Promedio de citas", fmt_avg(d$citations), "JMLR no expone")),
        column(2, metric_card("Promedio de referencias", fmt_avg(d$n_references), "JMLR no expone"))
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
        scrape_msg(sprintf("Se encontraron %d artículos nuevos de 2026 y se almacenaron en SQLite.", n))
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
      df$abstract <- NA_character_                         # carga rápida (sin visitar cada página)
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
}

shinyApp(ui, server)
