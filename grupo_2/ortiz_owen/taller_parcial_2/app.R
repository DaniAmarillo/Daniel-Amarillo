# ─────────────────────────────────────────────────────────────
#  Taller 2 — Minería de Datos | Scientific Reports Dashboard
#  Shiny + highcharter + DT + SQLite
# ─────────────────────────────────────────────────────────────

library(shiny)
library(shinydashboard)
library(highcharter)
library(DT)
library(dplyr)
library(DBI)
library(RSQLite)
library(rvest)
library(httr2)
library(xml2)
library(rsconnect)

DB_PATH <- "nature_srep_2025.db"

# ── Helpers ───────────────────────────────────────────────────
get_papers <- function(con = NULL) {
  close_con <- is.null(con)
  if (close_con) con <- dbConnect(SQLite(), DB_PATH)
  df <- dbGetQuery(con, "SELECT * FROM papers")
  if (close_con) dbDisconnect(con)
  df$pub_date <- as.Date(df$publication_date, format = "%d %B %Y")
  df
}

get_html_safe <- function(url) {
  tryCatch({
    Sys.sleep(2)
    request(url) |>
      req_headers(`User-Agent` = "Mozilla/5.0") |>
      req_perform() |>
      resp_body_html()
  }, error = function(e) NULL)
}

get_li <- function(nodo) {
  for (i in 1:6) {
    nodo <- xml_parent(nodo)
    if (xml_name(nodo) == "li") return(nodo)
  }
  NULL
}

scrape_year <- function(year = 2026, max_pages = 3) {
  base <- paste0("https://www.nature.com/srep/articles?searchType=journalSearch&sort=PubDate&year=", year, "&page=")
  result <- data.frame()
  for (p in seq_len(max_pages)) {
    html <- get_html_safe(paste0(base, p))
    if (is.null(html)) next
    all_a <- html |> html_elements("h3 a")
    hrefs <- all_a |> html_attr("href")
    ok    <- grepl("^/articles/s41598", hrefs)
    if (!any(ok)) next
    enlaces <- all_a[ok]
    titulos <- enlaces |> html_text2()
    urls    <- paste0("https://www.nature.com", hrefs[ok])
    result  <- rbind(result, data.frame(titulo = titulos, url = urls,
                                        stringsAsFactors = FALSE))
  }
  result
}

scrape_detail <- function(url) {
  html <- get_html_safe(url)
  if (is.null(html)) return(list())
  meta <- function(n) html |> html_element(paste0("meta[name='",n,"']")) |> html_attr("content")
  txt  <- html |> html_text2()
  fm   <- regmatches(txt, regexpr("Published:\\s*\\d{1,2}\\s+\\w+\\s+\\d{4}", txt))
  metr <- html |> html_elements("p.c-article-metrics-bar__count") |> html_text2()
  num  <- function(p) {
    m <- grep(p, metr, value=TRUE, ignore.case=TRUE)
    if (!length(m)) return(NA_integer_)
    as.integer(gsub(",","", regmatches(m[1], regexpr("[0-9,]+", m[1]))))
  }
  clasificar <- function(titulo, resumen) {
    texto <- tolower(paste(titulo, resumen))
    gen  <- c("generative","llm","gpt","diffusion model","gan ","variational autoencoder","text generation","image generation")
    ml   <- c("machine learning","deep learning","neural network","classification","random forest","gradient boosting","convolutional","transformer","prediction model","supervised","unsupervised","reinforcement")
    stat <- c("regression","bayesian","statistical","survival analysis","meta-analysis","hypothesis test","probability","multivariate","time series","correlation","variance")
    if (any(sapply(gen,  grepl, x=texto))) return("IA Generativa")
    if (any(sapply(ml,   grepl, x=texto))) return("Machine Learning")
    if (any(sapply(stat, grepl, x=texto))) return("Estadística")
    "Otros"
  }
  doi     <- meta("citation_doi")
  abstract <- meta("description")
  autores <- paste(html |> html_elements("meta[name='citation_author']") |> html_attr("content"), collapse="; ")
  fecha   <- if (length(fm)) gsub("Published:\\s*","",fm) else NA_character_
  yr      <- as.integer(regmatches(fecha %||% "", regexpr("\\d{4}", fecha %||% "")))
  list(doi=doi, title=html |> html_element("title") |> html_text2(),
       publication_date=fecha, year=yr, url=url,
       abstract=abstract %||% "", authors_raw=autores,
       n_authors=length(strsplit(autores,"; ")[[1]]),
       citations=num("itat"), downloads=num("ccess|ownload"),
       n_references=html |> html_elements("ol.c-article-references li") |> length(),
       topic_label=clasificar(html |> html_element("h1") |> html_text2(), abstract %||% ""))
}

`%||%` <- function(a,b) if(!is.null(a)&&length(a)>0&&!is.na(a[1])) a else b

# ── CSS personalizado ─────────────────────────────────────────
custom_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Space+Mono:wght@400;700&family=Inter:wght@300;400;600&display=swap');
  body, .content-wrapper, .main-footer { background: #0d1117 !important; font-family: 'Inter', sans-serif; }
  .skin-blue .main-header .logo, .skin-blue .main-header .navbar { background: #161b22 !important; border-bottom: 1px solid #21262d; }
  .skin-blue .main-sidebar { background: #161b22 !important; }
  .skin-blue .sidebar-menu > li.active > a, .skin-blue .sidebar-menu > li > a:hover { background: #1f2937 !important; border-left: 3px solid #58a6ff; }
  .skin-blue .sidebar-menu > li > a { color: #8b949e !important; font-size: 13px; }
  .box { background: #161b22 !important; border: 1px solid #21262d !important; border-top: none !important; border-radius: 8px !important; box-shadow: 0 4px 24px rgba(0,0,0,0.4) !important; }
  .box-header { background: #161b22 !important; border-bottom: 1px solid #21262d !important; border-radius: 8px 8px 0 0 !important; }
  .box-title { color: #e6edf3 !important; font-family: 'Space Mono', monospace !important; font-size: 13px !important; letter-spacing: 0.05em; }
  .small-box { border-radius: 10px !important; box-shadow: 0 4px 20px rgba(0,0,0,0.5) !important; }
  .small-box h3 { font-family: 'Space Mono', monospace !important; font-size: 28px !important; }
  .small-box p { font-size: 12px !important; font-weight: 600; letter-spacing: 0.08em; text-transform: uppercase; }
  .small-box .icon { opacity: 0.15; }
  .small-box-footer { background: rgba(0,0,0,0.2) !important; }
  .form-control { background: #0d1117 !important; border: 1px solid #30363d !important; color: #e6edf3 !important; border-radius: 6px; }
  .form-control:focus { border-color: #58a6ff !important; box-shadow: 0 0 0 3px rgba(88,166,255,0.1) !important; }
  label { color: #8b949e !important; font-size: 11px !important; text-transform: uppercase; letter-spacing: 0.08em; font-weight: 600; }
  .btn-primary { background: #238636 !important; border-color: #238636 !important; font-family: 'Space Mono', monospace; font-size: 12px; width: 100%; border-radius: 6px; }
  .btn-primary:hover { background: #2ea043 !important; }
  .btn-success { background: #1f6feb !important; border-color: #1f6feb !important; font-family: 'Space Mono', monospace; font-size: 12px; border-radius: 6px; }
  .btn-success:hover { background: #388bfd !important; }
  .sidebar-form .input-group { margin-bottom: 8px; }
  hr { border-color: #21262d !important; }
  .dataTables_wrapper { color: #e6edf3 !important; }
  table.dataTable thead th { background: #161b22 !important; color: #8b949e !important; border-bottom: 1px solid #21262d !important; font-size: 11px; text-transform: uppercase; letter-spacing: 0.06em; }
  table.dataTable tbody tr { background: #0d1117 !important; }
  table.dataTable tbody tr:hover { background: #1c2128 !important; }
  table.dataTable tbody td { color: #e6edf3 !important; border-top: 1px solid #21262d !important; font-size: 13px; }
  .dataTables_info, .dataTables_length label, .dataTables_filter label { color: #8b949e !important; font-size: 12px; }
  .paginate_button { color: #8b949e !important; }
  .paginate_button.current { background: #1f6feb !important; color: white !important; border-radius: 4px; }
  select option { background: #161b22; color: #e6edf3; }
  .main-header .logo { font-family: 'Space Mono', monospace !important; font-size: 14px !important; letter-spacing: 0.05em; }
  pre { background: #0d1117 !important; color: #3fb950 !important; border: 1px solid #21262d !important; border-radius: 6px; font-family: 'Space Mono', monospace; font-size: 12px; }
"

# ── UI ────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "srep // dashboard"),

  dashboardSidebar(
    tags$style(HTML(custom_css)),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dash", icon = icon("chart-bar")),
      menuItem("Actualización", tabName = "update", icon = icon("rotate"))
    ),
    tags$hr(),
    tags$div(style = "padding: 0 12px;",
      dateRangeInput("fecha", "Rango de fechas",
                     start = "2025-01-01", end = Sys.Date(),
                     language = "es"),
      selectInput("tema", "Categoría",
                  choices = c("Todas", "Machine Learning", "IA Generativa", "Estadística", "Otros"),
                  selected = "Todas"),
      textInput("autor",   "Autor",   placeholder = "Ej: Smith, John"),
      textInput("doi",     "DOI",     placeholder = "10.1038/..."),
      textInput("keyword", "Título / Palabras clave", placeholder = "Buscar..."),
      actionButton("filtrar", "Aplicar filtros", class = "btn-primary")
    )
  ),

  dashboardBody(
    tabItems(
      # ── TAB 1: Dashboard ──────────────────────────────────
      tabItem("dash",
        fluidRow(
          valueBoxOutput("vb_total",    width = 3),
          valueBoxOutput("vb_autores",  width = 3),
          valueBoxOutput("vb_citas",    width = 3),
          valueBoxOutput("vb_refs",     width = 3)
        ),
        fluidRow(
          valueBoxOutput("vb_citado",    width = 6),
          valueBoxOutput("vb_descargado", width = 6)
        ),
        fluidRow(
          box(title = "Artículos por categoría", width = 6, status = "primary",
              highchartOutput("chart_cat", height = "300px")),
          box(title = "Distribución de descargas por categoría", width = 6, status = "primary",
              highchartOutput("chart_downloads", height = "300px"))
        ),
        fluidRow(
          box(title = "Top 10 artículos más descargados", width = 6, status = "primary",
              highchartOutput("chart_top10", height = "350px")),
          box(title = "Top 10 autores más frecuentes", width = 6, status = "primary",
              highchartOutput("chart_autores", height = "350px"))
        ),
        fluidRow(
          box(title = "Tabla de artículos", width = 12, status = "primary",
              DTOutput("tabla"))
        )
      ),

      # ── TAB 2: Actualización ─────────────────────────────
      tabItem("update",
        fluidRow(
          box(title = "Scraping de nuevos artículos", width = 12, status = "primary",
              tags$p(style = "color:#8b949e; font-size:13px;",
                "Busca artículos publicados en 2026 no almacenados en la base de datos. ",
                "Si no hay nuevos, re-raspa los últimos 5 artículos para verificar actualizaciones."),
              tags$br(),
              actionButton("scrape_btn", "Buscar nuevos artículos (2026)",
                           class = "btn-success", icon = icon("magnifying-glass")),
              tags$hr(),
              verbatimTextOutput("scrape_log"),
              DTOutput("nuevos_tabla")
          )
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Datos reactivos (se recargan cuando cambia la DB)
  rv <- reactiveValues(refresh = 0)

  datos_base <- reactive({
    rv$refresh
    get_papers()
  })

  # Filtros — solo se aplican al hacer click
  filtros <- reactiveValues(aplicado = FALSE,
    tema = "Todas", autor = "", doi = "", keyword = "",
    fecha_ini = NULL, fecha_fin = NULL)

  observeEvent(input$filtrar, {
    filtros$aplicado  <- TRUE
    filtros$tema      <- input$tema
    filtros$autor     <- input$autor
    filtros$doi       <- input$doi
    filtros$keyword   <- input$keyword
    filtros$fecha_ini <- input$fecha[1]
    filtros$fecha_fin <- input$fecha[2]
  })

  df_actual <- reactive({
    df <- datos_base()
    if (!filtros$aplicado) return(df)   # sin filtros al inicio
    if (filtros$tema != "Todas")
      df <- df[df$topic_label == filtros$tema, ]
    if (nchar(filtros$autor) > 0)
      df <- df[grepl(filtros$autor, df$authors_raw, ignore.case = TRUE), ]
    if (nchar(filtros$doi) > 0)
      df <- df[grepl(filtros$doi, df$doi %||% "", ignore.case = TRUE), ]
    if (nchar(filtros$keyword) > 0)
      df <- df[grepl(filtros$keyword, df$title, ignore.case = TRUE), ]
    if (!is.null(filtros$fecha_ini) && !is.null(filtros$fecha_fin))
      df <- df[is.na(df$pub_date) |
               (df$pub_date >= filtros$fecha_ini &
                df$pub_date <= filtros$fecha_fin), ]
    df
  })

  # ── Value boxes ──────────────────────────────────────────
  output$vb_total <- renderValueBox({
    valueBox(nrow(df_actual()), "Artículos", icon = icon("newspaper"),
             color = "blue")
  })
  output$vb_autores <- renderValueBox({
    val <- round(mean(df_actual()$n_authors, na.rm = TRUE), 1)
    valueBox(val, "Promedio autores", icon = icon("users"), color = "aqua")
  })
  output$vb_citas <- renderValueBox({
    val <- round(mean(df_actual()$citations, na.rm = TRUE), 1)
    valueBox(ifelse(is.nan(val), "N/A", val), "Promedio citas",
             icon = icon("quote-right"), color = "green")
  })
  output$vb_refs <- renderValueBox({
    val <- round(mean(df_actual()$n_references, na.rm = TRUE), 1)
    valueBox(val, "Promedio referencias", icon = icon("book"), color = "yellow")
  })
  output$vb_citado <- renderValueBox({
    df <- df_actual()
    df <- df[!is.na(df$citations), ]
    if (nrow(df) == 0) return(valueBox("N/A", "Más citado", icon = icon("star"), color = "purple"))
    top <- df[which.max(df$citations), ]
    valueBox(
      paste0(top$citations, " citas"),
      paste0("Más citado: ", substr(top$title, 1, 50), "..."),
      icon = icon("star"), color = "purple"
    )
  })
  output$vb_descargado <- renderValueBox({
    df <- df_actual()
    df <- df[!is.na(df$downloads), ]
    if (nrow(df) == 0) return(valueBox("N/A", "Más descargado", icon = icon("download"), color = "orange"))
    top <- df[which.max(df$downloads), ]
    valueBox(
      paste0(format(top$downloads, big.mark=","), " descargas"),
      paste0("Más descargado: ", substr(top$title, 1, 50), "..."),
      icon = icon("download"), color = "orange"
    )
  })

  # ── Gráficas ─────────────────────────────────────────────
  output$chart_cat <- renderHighchart({
    df <- df_actual() |>
      group_by(topic_label) |>
      summarise(n = n(), .groups = "drop")
    if (nrow(df) == 0) return(highchart())
    hchart(df, "pie", hcaes(name = topic_label, y = n)) |>
      hc_colors(c("#1f6feb","#3fb950","#d29922","#f85149")) |>
      hc_chart(backgroundColor = "#161b22") |>
      hc_title(text = NULL) |>
      hc_plotOptions(pie = list(
        dataLabels = list(enabled = TRUE, color = "#e6edf3",
                          style = list(fontFamily = "Space Mono", fontSize = "11px"))
      )) |>
      hc_legend(itemStyle = list(color = "#8b949e"))
  })

  output$chart_downloads <- renderHighchart({
    df <- df_actual() |>
      filter(!is.na(downloads)) |>
      group_by(topic_label) |>
      summarise(total = sum(downloads, na.rm=TRUE), .groups="drop") |>
      arrange(desc(total))
    if (nrow(df) == 0) return(highchart())
    hchart(df, "bar", hcaes(x = topic_label, y = total)) |>
      hc_colors("#1f6feb") |>
      hc_chart(backgroundColor = "#161b22") |>
      hc_title(text = NULL) |>
      hc_xAxis(labels = list(style = list(color = "#8b949e", fontFamily = "Inter"))) |>
      hc_yAxis(labels = list(style = list(color = "#8b949e")),
               gridLineColor = "#21262d") |>
      hc_plotOptions(bar = list(
        dataLabels = list(enabled = TRUE, color = "#e6edf3",
                          style = list(fontFamily = "Space Mono", fontSize = "11px"))
      ))
  })

  # ── Tabla ────────────────────────────────────────────────
  output$tabla <- renderDT({
    df <- df_actual() |>
      select(title, authors_raw, publication_date, topic_label, doi, citations, downloads) |>
      rename(Título=title, Autores=authors_raw, Fecha=publication_date,
             Categoría=topic_label, DOI=doi, Citas=citations, Descargas=downloads)
    datatable(df,
      options = list(pageLength = 10, scrollX = TRUE,
                     dom = "lfrtip",
                     language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")),
      rownames = FALSE, escape = FALSE,
      style = "bootstrap"
    )
  })

  output$chart_top10 <- renderHighchart({
    df <- df_actual() |>
      filter(!is.na(downloads)) |>
      arrange(desc(downloads)) |>
      head(10) |>
      mutate(titulo_corto = substr(title, 1, 50))
    if (nrow(df) == 0) return(highchart())
    hchart(df, "bar", hcaes(x = titulo_corto, y = downloads)) |>
      hc_colors("#3fb950") |>
      hc_chart(backgroundColor = "#161b22") |>
      hc_title(text = NULL) |>
      hc_xAxis(labels = list(style = list(color = "#8b949e", fontSize = "11px",
                                          fontFamily = "Inter"))) |>
      hc_yAxis(labels = list(style = list(color = "#8b949e")),
               gridLineColor = "#21262d", title = list(text = "Descargas",
               style = list(color = "#8b949e"))) |>
      hc_plotOptions(bar = list(
        dataLabels = list(enabled = TRUE, color = "#e6edf3",
                          style = list(fontFamily = "Space Mono", fontSize = "10px"))
      )) |>
      hc_tooltip(pointFormat = "<b>{point.y:,.0f}</b> descargas")
  })

  output$chart_autores <- renderHighchart({
    con <- dbConnect(SQLite(), DB_PATH)
    # filtrar por paper_ids del df_actual
    ids <- df_actual()$paper_id
    if (length(ids) == 0) { dbDisconnect(con); return(highchart()) }
    ids_str <- paste(ids, collapse = ",")
    df <- dbGetQuery(con, paste0(
      "SELECT autor, COUNT(*) as n FROM autores
       WHERE paper_id IN (", ids_str, ")
       AND autor != ''
       GROUP BY autor ORDER BY n DESC LIMIT 10"))
    dbDisconnect(con)
    if (nrow(df) == 0) return(highchart())
    df$autor <- factor(df$autor, levels = rev(df$autor))
    hchart(df, "bar", hcaes(x = autor, y = n)) |>
      hc_colors("#d29922") |>
      hc_chart(backgroundColor = "#161b22") |>
      hc_title(text = NULL) |>
      hc_xAxis(labels = list(style = list(color = "#8b949e", fontSize = "11px",
                                          fontFamily = "Inter"))) |>
      hc_yAxis(labels = list(style = list(color = "#8b949e")),
               gridLineColor = "#21262d",
               title = list(text = "Artículos", style = list(color = "#8b949e"))) |>
      hc_plotOptions(bar = list(
        dataLabels = list(enabled = TRUE, color = "#e6edf3",
                          style = list(fontFamily = "Space Mono", fontSize = "10px"))
      )) |>
      hc_tooltip(pointFormat = "<b>{point.y}</b> artículos")
  })

  # ── Scraping ─────────────────────────────────────────────
  scrape_result <- reactiveVal(NULL)
  log_text      <- reactiveVal("Esperando...")

  observeEvent(input$scrape_btn, {
    log_text("Iniciando scraping de artículos 2026...")
    scrape_result(NULL)

    withProgress(message = "Scrapeando Nature...", value = 0, {
      con <- dbConnect(SQLite(), DB_PATH)
      dois_existentes <- dbGetQuery(con, "SELECT doi FROM papers")$doi

      incProgress(0.2, detail = "Obteniendo listado 2026...")
      lista <- scrape_year(2026, max_pages = 2)

      if (nrow(lista) == 0) {
        log_text("No se encontraron artículos de 2026 en el listado.")
        dbDisconnect(con)
        return()
      }

      log_text(paste0("Listado obtenido: ", nrow(lista), " artículos encontrados. Verificando novedades..."))

      nuevos <- list()
      for (i in seq_len(nrow(lista))) {
        incProgress(0.6 / nrow(lista), detail = paste("Artículo", i, "/", nrow(lista)))
        det <- scrape_detail(lista$url[i])
        if (is.null(det$doi) || is.na(det$doi)) next
        if (det$doi %in% dois_existentes) next
        nuevos[[length(nuevos)+1]] <- det
      }

      if (length(nuevos) == 0) {
        log_text("No hay artículos nuevos de 2026. Re-raspando los últimos 5 artículos para verificar actualizaciones...")
        ultimos <- dbGetQuery(con, "SELECT url, doi, citations, downloads FROM papers ORDER BY paper_id DESC LIMIT 5")
        updates <- 0
        for (i in seq_len(nrow(ultimos))) {
          det <- scrape_detail(ultimos$url[i])
          if (length(det) == 0) next
          citas_new <- det$citations %||% NA
          desc_new  <- det$downloads %||% NA
          if (!is.na(citas_new) && !is.na(ultimos$citations[i]) && citas_new != ultimos$citations[i]) {
            dbExecute(con, "UPDATE papers SET citations=? WHERE doi=?", list(citas_new, ultimos$doi[i]))
            updates <- updates + 1
          }
          if (!is.na(desc_new) && !is.na(ultimos$downloads[i]) && desc_new != ultimos$downloads[i]) {
            dbExecute(con, "UPDATE papers SET downloads=? WHERE doi=?", list(desc_new, ultimos$doi[i]))
            updates <- updates + 1
          }
        }
        log_text(paste0("Sin artículos nuevos. Se actualizaron ", updates, " métricas en los últimos 5 artículos."))
        dbDisconnect(con)
        rv$refresh <- rv$refresh + 1
        return()
      }

      # Insertar nuevos
      for (art in nuevos) {
        dbExecute(con,
          "INSERT INTO papers (journal_name,title,publication_date,year,doi,url,
           abstract,authors_raw,n_authors,citations,downloads,n_references,topic_label)
           VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)",
          list("Scientific Reports", art$title, art$publication_date, art$year,
               art$doi, art$url, art$abstract, art$authors_raw, art$n_authors,
               art$citations, art$downloads, art$n_references, art$topic_label))
      }

      dbDisconnect(con)
      rv$refresh <- rv$refresh + 1

      df_nuevos <- do.call(rbind, lapply(nuevos, function(a)
        data.frame(title=a$title, doi=a$doi %||% NA,
                   publication_date=a$publication_date %||% NA,
                   topic_label=a$topic_label %||% NA,
                   stringsAsFactors=FALSE)))
      scrape_result(df_nuevos)
      log_text(paste0("✓ ", length(nuevos), " artículos nuevos agregados a la base de datos."))
    })
  })

  output$scrape_log    <- renderText(log_text())
  output$nuevos_tabla  <- renderDT({
    req(scrape_result())
    datatable(scrape_result(), rownames = FALSE,
              options = list(pageLength = 5, scrollX = TRUE))
  })
}

shinyApp(ui, server)
