# ============================================================
# app.R — Taller 2 · Minería de Datos (2016325)
# Dashboard analítico: Nature Ecology & Evolution
# Sebastián Tabares-Segovia
# ============================================================

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(stringr)
library(tibble)
library(DBI)
library(RSQLite)
library(httr2)
library(purrr)
library(highcharter)

DB_PATH <- "nature_eco_evo_2025.sqlite"

# ════════════════════════════════════════════════════════════
# FUNCIONES AUXILIARES
# ════════════════════════════════════════════════════════════

hacer_peticion <- function(url) {
  request(url) |>
    req_user_agent("Mozilla/5.0 (compatible; shiny-dashboard/1.0)") |>
    req_perform()
}
hacer_peticion_segura <- purrr::possibly(hacer_peticion, otherwise = NULL)

leer_papers <- function(filtros = list()) {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(dbDisconnect(con))

  where <- "WHERE 1=1"
  if (!is.null(filtros$fecha_ini) && nchar(filtros$fecha_ini) > 0)
    where <- paste0(where, " AND publication_date >= '", filtros$fecha_ini, "'")
  if (!is.null(filtros$fecha_fin) && nchar(filtros$fecha_fin) > 0)
    where <- paste0(where, " AND publication_date <= '", filtros$fecha_fin, "'")
  if (!is.null(filtros$topics) && length(filtros$topics) > 0)
    where <- paste0(where, " AND topic_label IN (",
                    paste0("'", filtros$topics, "'", collapse = ","), ")")
  if (!is.null(filtros$titulo) && nchar(filtros$titulo) > 0)
    where <- paste0(where, " AND title LIKE '%", filtros$titulo, "%'")
  if (!is.null(filtros$autor) && nchar(filtros$autor) > 0)
    where <- paste0(where, " AND authors_raw LIKE '%", filtros$autor, "%'")
  if (!is.null(filtros$doi) && nchar(filtros$doi) > 0)
    where <- paste0(where, " AND doi LIKE '%", filtros$doi, "%'")

  dbGetQuery(con, paste(
    "SELECT paper_id, title, authors_raw, publication_date,",
    "topic_label, doi, url, citations, downloads, n_authors, n_references",
    "FROM papers", where, "ORDER BY citations DESC"
  ))
}

leer_indicadores <- function(df = NULL) {
  if (is.null(df)) {
    con <- dbConnect(RSQLite::SQLite(), DB_PATH)
    on.exit(dbDisconnect(con))
    return(dbGetQuery(con, "
      SELECT COUNT(*)                    AS total_papers,
             ROUND(AVG(n_authors),    1) AS avg_autores,
             ROUND(AVG(citations),    1) AS avg_citas,
             ROUND(AVG(n_references), 1) AS avg_referencias,
             SUM(downloads)             AS total_accesses,
             COUNT(downloads)           AS papers_con_accesses
      FROM papers
    "))
  }
  tibble(
    total_papers        = nrow(df),
    avg_autores         = round(mean(df$n_authors,    na.rm = TRUE), 1),
    avg_citas           = round(mean(df$citations,    na.rm = TRUE), 1),
    avg_referencias     = round(mean(df$n_references, na.rm = TRUE), 1),
    total_accesses      = sum(df$downloads,           na.rm = TRUE),
    papers_con_accesses = sum(!is.na(df$downloads))
  )
}

kw_ia   <- paste(c("generative ai","large language model","llm","gpt","chatgpt",
                   "diffusion model","generative model","foundation model",
                   "variational autoencoder","stable diffusion"), collapse="|")
kw_ml   <- paste(c("machine learning","deep learning","neural network","random forest",
                   "gradient boosting","supervised learning","unsupervised learning",
                   "reinforcement learning","convolutional","support vector","xgboost",
                   "artificial intelligence","prediction model"), collapse="|")
kw_stat <- paste(c("bayesian","statistical inference","regression analysis",
                   "hypothesis test","confidence interval","causal inference",
                   "statistical model","time series","monte carlo",
                   "markov chain","survival analysis","stochastic"), collapse="|")

clasificar_tema <- function(df) {
  df |>
    mutate(
      texto       = str_to_lower(paste(title, coalesce(abstract, ""), sep = " ")),
      topic_label = case_when(
        str_detect(texto, kw_ia)   ~ "IA Generativa",
        str_detect(texto, kw_ml)   ~ "Machine Learning",
        str_detect(texto, kw_stat) ~ "Estadística",
        .default                   = "Otros"
      )
    ) |>
    select(-texto)
}

scrape_accesses_articulo <- function(url_articulo) {
  if (is.na(url_articulo)) return(NA_integer_)
  peticion_segura <- purrr::possibly(
    function(u) {
      request(u) |>
        req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36") |>
        req_headers(`accept-language` = "en-US,en;q=0.9") |>
        req_perform() |>
        rvest::resp_body_html()
    }, otherwise = NULL
  )
  pagina <- peticion_segura(url_articulo)
  if (is.null(pagina)) return(NA_integer_)
  accesses_texto <- pagina |>
    rvest::html_elements("span, p, li, div") |>
    rvest::html_text2() |>
    (\(x) x[grepl("^[0-9]+\\.?[0-9]*k?\\s+Accesses$", x, ignore.case = TRUE)])() |>
    head(1)
  if (length(accesses_texto) == 0 || is.na(accesses_texto)) return(NA_integer_)
  numero <- str_extract(accesses_texto, "[0-9]+\\.?[0-9]*k?")
  if (str_detect(numero, "k")) as.integer(as.numeric(str_remove(numero, "k")) * 1000)
  else as.integer(numero)
}

buscar_papers_nuevos <- function(fecha_desde) {
  url <- paste0(
    "https://api.openalex.org/works",
    "?filter=primary_location.source.issn:2397-334X",
    ",from_publication_date:", fecha_desde,
    "&per_page=50",
    "&select=id,title,publication_date,publication_year,doi,",
    "abstract_inverted_index,authorships,cited_by_count,referenced_works_count",
    "&mailto=setabaress@unal.edu.co"
  )
  resp <- hacer_peticion_segura(url)
  if (is.null(resp)) return(tibble())
  datos      <- resp |> resp_body_json(simplifyVector = FALSE)
  resultados <- datos$results
  if (length(resultados) == 0) return(tibble())
  filas <- lapply(resultados, function(p) {
    autores    <- p$authorships
    nombres    <- sapply(autores, function(a) { nm <- a$author$display_name; if (is.null(nm)) NA_character_ else nm })
    nombres    <- nombres[!is.na(nombres)]
    doi_limpio <- if (!is.null(p$doi)) str_remove(p$doi, "https://doi.org/") else NA_character_
    autores_c  <- if (length(nombres) > 0) str_squish(paste(nombres, collapse="; ")) else NA_character_
    url_art    <- if (!is.null(doi_limpio) && !is.na(doi_limpio))
                    paste0("https://www.nature.com/articles/", str_remove(doi_limpio, "10.1038/"))
                  else NA_character_
    tibble(
      paper_id         = if (!is.null(p$id)) p$id else NA_character_,
      journal_name     = "Nature Ecology and Evolution",
      title            = if (!is.null(p$title)) p$title else NA_character_,
      publication_date = if (!is.null(p$publication_date)) p$publication_date else NA_character_,
      year             = if (!is.null(p$publication_year)) as.integer(p$publication_year) else NA_integer_,
      doi              = doi_limpio,
      url              = url_art,
      abstract         = NA_character_,
      authors_raw      = autores_c,
      n_authors        = length(autores),
      citations        = if (!is.null(p$cited_by_count)) as.integer(p$cited_by_count) else NA_integer_,
      downloads        = NA_integer_,
      n_references     = if (!is.null(p$referenced_works_count)) as.integer(p$referenced_works_count) else NA_integer_,
      topic_label      = NA_character_
    )
  })
  bind_rows(filas)
}

COLORES_TEMA <- c(
  "Otros"            = "#94a3b8",
  "Machine Learning" = "#8CBF3C",
  "Estadística"      = "#7c3aed",
  "IA Generativa"    = "#16a34a"
)

# ════════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════════

ui <- navbarPage(
  # MEJORA: logos + nombre + correo en el navbar
  # Logo UNAL más grande (44px), título desplazado a la derecha con padding
  title = tags$div(
    style = "display:flex; align-items:center; gap:14px; padding-left:20px; padding-right:16px;",
    tags$img(src="logo_nature.png", height="34px", style="border-radius:3px;"),
    tags$div(
      style = "padding-left:4px;",
      tags$span("Nature Eco & Evo — Dashboard KDD",
                style="font-weight:700; font-size:1rem; display:block;"),
      tags$span("Sebastián Tabares-Segovia · setabaress@unal.edu.co",
                style="font-size:0.68rem; opacity:0.85; font-weight:400;")
    ),
    tags$img(src="logo_unal.png", height="44px",
             style="margin-left:8px; border-radius:50%;")
  ),
  theme = bslib::bs_theme(
    version    = 5,
    bootswatch = "flatly",
    primary    = "#8CBF3C",
    base_font  = bslib::font_google("Inter")
  ),

  # ── PESTAÑA 1: DASHBOARD ─────────────────────────────────
  tabPanel("Dashboard",
    sidebarLayout(
      sidebarPanel(width = 3,
        tags$h5("Filtros", class="text-primary fw-bold"), tags$hr(),
        dateRangeInput("rango_fecha", "Rango de fechas:",
                       start="2025-01-01", end=Sys.Date(),
                       format="dd/mm/yyyy", language="es"),
        checkboxGroupInput("temas", "Categoría temática:",
          choices  = c("Machine Learning","IA Generativa","Estadística","Otros"),
          selected = c("Machine Learning","IA Generativa","Estadística","Otros")),
        tags$hr(),
        textInput("filtro_titulo", "Buscar por título:", placeholder="ej. climate change..."),
        textInput("filtro_autor",  "Buscar por autor:",  placeholder="ej. Johnson..."),
        textInput("doi_filtro",    "Filtrar por DOI:",   placeholder="ej. 10.1038/..."),
        tags$hr(),
        actionButton("aplicar_filtros", "Aplicar filtros",
                     icon=icon("filter"), class="btn-primary w-100"),
        tags$br(), tags$br(),
        actionButton("limpiar_filtros", "Limpiar filtros",
                     icon=icon("rotate-left"), class="btn-outline-secondary w-100"),
        tags$hr(),
        helpText("Fuente: OpenAlex · nature.com · Nature Ecology & Evolution")
      ),
      mainPanel(width = 9,
        tags$div(class="alert alert-light border-start border-primary border-3 p-2 mb-3",
          style="font-size:0.85rem;",
          tags$strong("Nature Ecology & Evolution"),
          " es una revista Q1 del grupo Springer Nature lanzada en 2017. ",
          "Este dashboard explora sus ", tags$strong("325 artículos de 2025"),
          " clasificados en cuatro categorías: ",
          tags$span(style="color:#8CBF3C;font-weight:600;","Machine Learning"), ", ",
          tags$span(style="color:#7c3aed;font-weight:600;","Estadística"), ", ",
          tags$span(style="color:#16a34a;font-weight:600;","IA Generativa"), " y ",
          tags$span(style="color:#94a3b8;font-weight:600;","Otros"), ". ",
          "La métrica de Accesses fue extraída directamente de nature.com."
        ),
        tags$h5("Indicadores", class="text-primary fw-bold mb-3"),
        fluidRow(
          column(2, uiOutput("card_total")),
          column(2, uiOutput("card_autores")),
          column(2, uiOutput("card_citas")),
          column(2, uiOutput("card_referencias")),
          column(2, uiOutput("card_accesses")),
          column(2, uiOutput("card_ml"))
        ),
        tags$br(),
        fluidRow(
          column(6,
            tags$div(class="card border-0 shadow-sm h-100",
              tags$div(class="card-header bg-primary text-white py-2",
                icon("trophy"), tags$strong(" Artículo más citado")),
              tags$div(class="card-body p-2", uiOutput("card_mas_citado"))
            )
          ),
          column(6,
            tags$div(class="card border-0 shadow-sm h-100",
              tags$div(class="card-header bg-success text-white py-2",
                icon("eye"), tags$strong(" Artículo con más Accesses")),
              tags$div(class="card-body p-2", uiOutput("card_mas_descargado"))
            )
          )
        ),
        tags$hr(),
        tags$h5("Visualizaciones",
                class="text-primary fw-bold mb-1"),
        tags$p(class="text-muted mb-3", style="font-size:0.82rem;",
               icon("hand-pointer"),
               " Pasa el ratón para ver el valor exacto · Haz clic en cualquier elemento para ver los artículos correspondientes."),
        fluidRow(
          column(6,
            highchartOutput("chart_temas",    height="300px"),
            uiOutput("interp_temas")),
          column(6,
            highchartOutput("chart_timeline", height="300px"),
            uiOutput("interp_timeline"))
        ),
        tags$br(),
        fluidRow(
          column(6,
            highchartOutput("chart_citas",       height="300px"),
            uiOutput("interp_citas")),
          column(6,
            highchartOutput("chart_top_autores", height="300px"),
            uiOutput("interp_autores"))
        )
      )
    )
  ),

  # ── PESTAÑA 2: TABLA ─────────────────────────────────────
  tabPanel("Artículos",
    sidebarLayout(
      sidebarPanel(width = 3,
        tags$h5("Filtros", class="text-primary fw-bold"), tags$hr(),
        dateRangeInput("rango_fecha2", "Rango de fechas:",
                       start="2025-01-01", end=Sys.Date(),
                       format="dd/mm/yyyy", language="es"),
        selectInput("tema_tabla", "Categoría:",
          choices=c("Todos","Machine Learning","IA Generativa","Estadística","Otros"),
          selected="Todos"),
        textInput("titulo_tabla", "Buscar por título:", placeholder="ej. coral..."),
        textInput("autor_tabla",  "Buscar por autor:",  placeholder="ej. Smith..."),
        tags$hr(),
        downloadButton("descargar_csv", "Descargar CSV", class="btn-success w-100")
      ),
      mainPanel(width = 9,
        tags$h5("Artículos filtrados", class="text-primary fw-bold"),
        DT::dataTableOutput("tabla_articulos")
      )
    )
  ),

  # ── PESTAÑA 3: ACTUALIZAR ────────────────────────────────
  tabPanel("Actualizar datos",
    fluidRow(column(10, offset=1,
      tags$br(),
      tags$h4("Actualización de la base de datos", class="text-primary fw-bold"),
      tags$p("Consulta OpenAlex para detectar artículos publicados después del",
             "último registro. Si no hay nuevos, actualiza las citas de los últimos 5."),
      tags$hr(),
      fluidRow(
        column(4,
          actionButton("btn_actualizar", "Buscar artículos nuevos",
                       icon=icon("magnifying-glass"), class="btn-primary btn-lg w-100"),
          tags$br(), tags$br(),
          uiOutput("contador_resultado")
        ),
        column(8,
          tags$div(class="card p-3",
            tags$div(class="d-flex justify-content-between align-items-center mb-2",
              tags$h6("Log de ejecución:", class="text-secondary mb-0"),
              tags$small(class="text-muted", textOutput("timestamp_actualizacion", inline=TRUE))
            ),
            verbatimTextOutput("log_actualizacion")
          )
        )
      ),
      tags$hr(),
      tags$h5("Últimos 5 artículos en la base de datos:", class="text-info fw-bold"),
      tags$p(class="text-muted", style="font-size:0.85rem;",
             "Se actualiza automáticamente tras cada ejecución."),
      DT::dataTableOutput("tabla_ultimos_siempre"),
      tags$br(),
      uiOutput("nuevos_papers_ui")
    ))
  ),

  # ── PESTAÑA 4: METODOLOGÍA ───────────────────────────────
  tabPanel("Metodología",
    fluidRow(column(10, offset=1,
      tags$br(),
      tags$h4("Proceso KDD implementado", class="text-primary fw-bold"),
      tags$div(class="row g-3 mb-4",
        tags$div(class="col-md-4",
          tags$div(class="card border-primary h-100",
            tags$div(class="card-header bg-primary text-white","① Recolección"),
            tags$div(class="card-body",
              tags$p("325 artículos de Nature Ecology & Evolution 2025 via API OpenAlex."),
              tags$p("Accesses extraídos con ", tags$code("rvest")," de nature.com."))
          )
        ),
        tags$div(class="col-md-4",
          tags$div(class="card border-success h-100",
            tags$div(class="card-header bg-success text-white","② Almacenamiento"),
            tags$div(class="card-body",
              tags$p("SQLite con tabla ", tags$code("papers"), " (14 cols) y ",
                     tags$code("paper_references"), " (14.981 pares)."))
          )
        ),
        tags$div(class="col-md-4",
          tags$div(class="card border-warning h-100",
            tags$div(class="card-header bg-warning","③ Clasificación"),
            tags$div(class="card-body",
              tags$p(tags$code("case_when()"), " + palabras clave en título/abstract.",
                     " Orden: IA Generativa → ML → Estadística → Otros."))
          )
        )
      ),
      tags$div(class="row g-3 mb-4",
        tags$div(class="col-md-6",
          tags$div(class="card border-info h-100",
            tags$div(class="card-header bg-info text-white","④ Consulta y visualización"),
            tags$div(class="card-body",
              tags$p("Queries SQL dinámicos según filtros. Gráficos con ",
                     tags$code("highcharter"), " con click interactivo."))
          )
        ),
        tags$div(class="col-md-6",
          tags$div(class="card border-danger h-100",
            tags$div(class="card-header bg-danger text-white","⑤ Actualización"),
            tags$div(class="card-body",
              tags$p("Botón detecta artículos de 2026, scrapeea Accesses y guarda en SQLite.",
                     " Si no hay nuevos, actualiza citas de los últimos 5."))
          )
        )
      ),
      tags$hr(),
      tags$h5("Funciones nuevas respecto al contenido de clase", class="text-primary fw-bold"),
      tags$table(class="table table-sm table-bordered",
        tags$thead(tags$tr(tags$th("Función"), tags$th("Motivo"))),
        tags$tbody(
          tags$tr(tags$td(tags$code("highcharter")),
                  tags$td("Requerido por el enunciado sección 2.1.5.")),
          tags$tr(tags$td(tags$code("resp_body_json()")),
                  tags$td("Hermana de resp_body_html() para respuestas JSON.")),
          tags$tr(tags$td(tags$code("dbWriteTable()")),
                  tags$td("Inserción masiva de data frame en SQLite.")),
          tags$tr(tags$td(tags$code("dbExecute()")),
                  tags$td("UPDATE desde R. Concepto DDL visto en Taller SQL P1."))
        )
      ),
      tags$hr(),
      tags$h5("Estructura de la base de datos", class="text-primary fw-bold"),
      DT::dataTableOutput("tabla_metadata")
    ))
  )
)

# ════════════════════════════════════════════════════════════
# SERVER
# ════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # Modal de bienvenida
  observe({
    showModal(modalDialog(
      title = tags$span(icon("chart-bar"), " Bienvenido al Dashboard KDD"),
      tags$p("Explora, visualiza y actualiza los",
             tags$strong("325 artículos"), " de Nature Ecology & Evolution 2025."),
      tags$hr(),
      fluidRow(
        column(6,
          tags$p(icon("filter"), tags$strong(" Dashboard:"),   " indicadores y gráficos interactivos"),
          tags$p(icon("table"),  tags$strong(" Artículos:"),   " tabla filtrable con descarga CSV")
        ),
        column(6,
          tags$p(icon("rotate"), tags$strong(" Actualizar:"),  " scraping de artículos nuevos (2026)"),
          tags$p(icon("book"),   tags$strong(" Metodología:"), " pipeline KDD completo")
        )
      ),
      tags$hr(),
      tags$p(class="text-muted mb-0", style="font-size:0.82rem;",
             icon("hand-pointer"),
             " En los gráficos, haz clic en cualquier barra o punto para ver los artículos correspondientes."),
      footer = modalButton("Entrar al dashboard"),
      easyClose = TRUE, size = "m"
    ))
  }) |> bindEvent(TRUE, once = TRUE)

  # Estado reactivo
  rv <- reactiveValues(
    papers        = NULL,
    indicadores   = NULL,
    nuevos_papers = NULL,
    log_lines     = character(0),
    ultima_exec   = NULL,
    modal_data    = NULL,
    modal_titulo  = ""
  )

  agregar_log <- function(msg) {
    ts <- format(Sys.time(), "%H:%M:%S")
    rv$log_lines <- c(rv$log_lines, paste0("[", ts, "] ", msg))
    if (length(rv$log_lines) > 25) rv$log_lines <- tail(rv$log_lines, 25)
  }

  observe({
    rv$papers      <- leer_papers()
    rv$indicadores <- leer_indicadores()
  })

  papers_filtrados <- eventReactive(input$aplicar_filtros, {
    req(rv$papers)
    leer_papers(filtros = list(
      fecha_ini = as.character(input$rango_fecha[1]),
      fecha_fin = as.character(input$rango_fecha[2]),
      topics    = input$temas,
      titulo    = input$filtro_titulo,
      autor     = input$filtro_autor,
      doi       = input$doi_filtro
    ))
  }, ignoreNULL = FALSE)

  datos_actuales <- reactive({
    if (input$aplicar_filtros == 0) { req(rv$papers); rv$papers }
    else papers_filtrados()
  })

  indicadores_actuales <- reactive({
    req(datos_actuales())
    leer_indicadores(df = datos_actuales())
  })

  observeEvent(input$limpiar_filtros, {
    updateDateRangeInput(session,     "rango_fecha",   start="2025-01-01", end=Sys.Date())
    updateCheckboxGroupInput(session, "temas",         selected=c("Machine Learning","IA Generativa","Estadística","Otros"))
    updateTextInput(session, "filtro_titulo", value="")
    updateTextInput(session, "filtro_autor",  value="")
    updateTextInput(session, "doi_filtro",    value="")
  })

  # ════════════════════════════════════════════════════════
  # INDICADORES
  # ════════════════════════════════════════════════════════

  hacer_card <- function(valor, etiqueta, color="#8CBF3C") {
    tags$div(
      class="card text-center p-2 mb-2",
      style=paste0("border-left:4px solid ",color,"; border-radius:6px;"),
      tags$div(style=paste0("font-size:1.4rem;font-weight:700;color:",color), valor),
      tags$div(style="font-size:0.75rem;color:#64748b;", etiqueta)
    )
  }

  output$card_total       <- renderUI({ req(indicadores_actuales()); hacer_card(format(indicadores_actuales()$total_papers, big.mark=","), "Artículos",        "#8CBF3C") })
  output$card_autores     <- renderUI({ req(indicadores_actuales()); hacer_card(indicadores_actuales()$avg_autores,          "Prom. autores",    "#16a34a") })
  output$card_citas       <- renderUI({ req(indicadores_actuales()); hacer_card(indicadores_actuales()$avg_citas,             "Prom. citas",      "#dc2626") })
  output$card_referencias <- renderUI({ req(indicadores_actuales()); hacer_card(indicadores_actuales()$avg_referencias,      "Prom. referencias","#7c3aed") })
  output$card_accesses    <- renderUI({ req(indicadores_actuales()); hacer_card(indicadores_actuales()$papers_con_accesses,  "Con Accesses",     "#ea580c") })
  output$card_ml          <- renderUI({
    req(datos_actuales())
    n_ml <- datos_actuales() |> filter(topic_label %in% c("Machine Learning","IA Generativa")) |> nrow()
    hacer_card(n_ml, "ML + IA papers", "#0891b2")
  })

  # MEJORA: hipervínculo en el título del artículo más citado
  hacer_titulo_link <- function(titulo, doi) {
    if (!is.na(doi) && nchar(doi) > 0) {
      tags$a(href=paste0("https://doi.org/", doi), target="_blank",
             style="color:inherit; text-decoration:underline dotted;",
             str_sub(titulo, 1, 80), if(nchar(titulo)>80)"..." else "")
    } else {
      tags$span(str_sub(titulo, 1, 80), if(nchar(titulo)>80)"..." else "")
    }
  }

  output$card_mas_citado <- renderUI({
    req(datos_actuales())
    top <- datos_actuales() |> filter(!is.na(citations)) |> arrange(desc(citations)) |> head(1)
    validate(need(nrow(top) > 0, "Sin datos en el filtro actual."))
    tags$div(
      tags$p(style="font-size:0.85rem;font-weight:600;margin-bottom:4px;",
             hacer_titulo_link(top$title, top$doi)),
      tags$p(style="font-size:0.78rem;color:#64748b;margin-bottom:6px;",
             str_sub(top$authors_raw, 1, 60), "..."),
      fluidRow(
        column(6, tags$span(class="badge bg-primary fs-6", paste("🏆", top$citations, "citas"))),
        column(6, tags$span(class="badge bg-secondary",    top$publication_date))
      )
    )
  })

  output$card_mas_descargado <- renderUI({
    req(datos_actuales())
    top <- datos_actuales() |> filter(!is.na(downloads)) |> arrange(desc(downloads)) |> head(1)
    if (nrow(top) == 0) return(tags$p(class="text-muted", "Sin datos de Accesses."))
    tags$div(
      tags$p(style="font-size:0.85rem;font-weight:600;margin-bottom:4px;",
             hacer_titulo_link(top$title, top$doi)),
      tags$p(style="font-size:0.78rem;color:#64748b;margin-bottom:6px;",
             str_sub(top$authors_raw, 1, 60), "..."),
      fluidRow(
        column(6, tags$span(class="badge bg-success fs-6", paste("👁", format(top$downloads, big.mark=","), "Accesses"))),
        column(6, tags$span(class="badge bg-secondary",    top$publication_date))
      )
    )
  })

  # ════════════════════════════════════════════════════════
  # MODAL COMPARTIDO PARA CLICKS EN GRÁFICOS
  # ════════════════════════════════════════════════════════

  # Render único de la tabla del modal — siempre activo
  output$tabla_modal <- DT::renderDataTable({
    req(rv$modal_data)
    rv$modal_data |>
      mutate(
        # MEJORA: hipervínculo en la columna Título del modal
        Título = ifelse(
          !is.na(doi) & nchar(doi) > 0,
          paste0('<a href="https://doi.org/', doi, '" target="_blank">',
                 str_sub(title, 1, 70),
                 if_else(nchar(title) > 70, "...", ""), "</a>"),
          str_sub(title, 1, 70)
        )
      ) |>
      select(Título, Autores=authors_raw, Fecha=publication_date,
             Tema=topic_label, Citas=citations, Accesses=downloads) |>
      DT::datatable(
        escape   = FALSE,   # permite HTML en la columna Título
        options  = list(pageLength=10, scrollX=TRUE,
                        language=list(url="//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
        rownames = FALSE
      )
  })

  # Función que abre el modal con datos + mini-stats + descarga CSV
  abrir_modal <- function(titulo, df) {
    rv$modal_data   <- df
    rv$modal_titulo <- titulo
    n_art    <- nrow(df)
    avg_c    <- round(mean(df$citations, na.rm = TRUE), 1)
    avg_acc  <- if (sum(!is.na(df$downloads)) > 0) round(mean(df$downloads, na.rm = TRUE), 0) else "—"
    top_cite <- if (!all(is.na(df$citations))) max(df$citations, na.rm = TRUE) else "—"
    showModal(modalDialog(
      title     = tags$span(icon("table"), " ", titulo),
      # Mini-stats del subconjunto seleccionado
      fluidRow(
        column(3, tags$div(class="card text-center p-2 border-primary",
          tags$div(style="font-size:1.4rem;font-weight:700;color:#8CBF3C;", n_art),
          tags$small("Artículos"))),
        column(3, tags$div(class="card text-center p-2",
          tags$div(style="font-size:1.4rem;font-weight:700;color:#dc2626;", avg_c),
          tags$small("Media citas"))),
        column(3, tags$div(class="card text-center p-2",
          tags$div(style="font-size:1.4rem;font-weight:700;color:#16a34a;", avg_acc),
          tags$small("Media Accesses"))),
        column(3, tags$div(class="card text-center p-2",
          tags$div(style="font-size:1.4rem;font-weight:700;color:#7c3aed;", top_cite),
          tags$small("Máx. citas")))
      ),
      tags$br(),
      DT::dataTableOutput("tabla_modal"),
      footer = tagList(
        downloadButton("descargar_modal_csv", "Descargar CSV",
                       class = "btn-success btn-sm"),
        modalButton("Cerrar")
      ),
      size = "xl", easyClose = TRUE
    ))
  }

  # ── Click en "Artículos por categoría temática" ──────────
  observeEvent(input$click_temas, {
    cat_sel <- input$click_temas$categoria
    df      <- datos_actuales() |> filter(topic_label == cat_sel)
    abrir_modal(paste0("Artículos · ", cat_sel, " (", nrow(df), ")"), df)
  })

  # ── Click en "Publicaciones por mes" ────────────────────
  observeEvent(input$click_timeline, {
    mes_sel <- input$click_timeline$mes
    df      <- datos_actuales() |>
      mutate(mes_pub = str_sub(publication_date, 1, 7)) |>
      filter(mes_pub == mes_sel) |>
      select(-mes_pub)
    abrir_modal(paste0("Artículos publicados en ", mes_sel, " (", nrow(df), ")"), df)
  })

  # ── Click en "Top 10 autores" ────────────────────────────
  observeEvent(input$click_autores, {
    autor_sel <- input$click_autores$autor
    df        <- datos_actuales() |>
      filter(str_detect(coalesce(authors_raw, ""), fixed(autor_sel)))
    abrir_modal(paste0("Artículos de ", autor_sel, " (", nrow(df), ")"), df)
  })

  # ── Click en "Distribución de citas" (histograma) ────────
  observeEvent(input$click_citas, {
    x_min <- as.numeric(input$click_citas$x_min)
    x_max <- as.numeric(input$click_citas$x_max)
    # Fallback: si x_max es NA o igual a x_min, estimar bin width
    if (is.na(x_max) || x_max <= x_min) {
      citas_all <- datos_actuales() |> filter(!is.na(citations)) |> pull(citations)
      bin_w     <- max(1, ceiling(diff(range(citas_all)) / 30))
      x_max     <- x_min + bin_w
    }
    df <- datos_actuales() |>
      filter(!is.na(citations), citations >= x_min, citations < x_max)
    abrir_modal(
      paste0("Artículos con ", round(x_min), "–", round(x_max),
             " citas (", nrow(df), ")"),
      df
    )
  })

  # ════════════════════════════════════════════════════════
  # GRÁFICOS HIGHCHARTER CON CLICK INTERACTIVO
  # NUEVO: hc_plotOptions con JS() para enviar clicks a Shiny
  # ════════════════════════════════════════════════════════

  # Gráfico 1: Artículos por categoría — click envía categoría
  # Cada punto lleva propiedad 'name' → this.name en JS es seguro y no crashea
  output$chart_temas <- renderHighchart({
    req(datos_actuales())
    df <- datos_actuales() |> count(topic_label) |> arrange(desc(n))
    data_pts <- lapply(seq_len(nrow(df)), function(i) {
      list(name  = df$topic_label[i],
           y     = df$n[i],
           color = unname(COLORES_TEMA[df$topic_label[i]]))
    })
    highchart() |>
      hc_chart(type = "bar") |>
      hc_title(text = "Artículos por categoría temática") |>
      hc_xAxis(categories = df$topic_label, title = list(text = "Categoría")) |>
      hc_yAxis(title = list(text = "N° artículos")) |>
      hc_add_series(name = "Artículos", data = data_pts, showInLegend = FALSE) |>
      hc_tooltip(pointFormat = "<b>{point.y}</b> artículos · clic para ver lista") |>
      hc_legend(enabled = FALSE) |>
      hc_plotOptions(bar = list(
        colorByPoint = TRUE,
        cursor       = "pointer",
        point        = list(events = list(
          click = JS("function() {
            Shiny.setInputValue('click_temas',
              {categoria: this.name},
              {priority: 'event'});
          }")
        ))
      ))
  })

  output$interp_temas <- renderUI({
    req(datos_actuales())
    df       <- datos_actuales() |> count(topic_label) |> arrange(desc(n))
    if (nrow(df) == 0) return(NULL)
    top_cat   <- df$topic_label[1]
    top_n     <- df$n[1]
    pct_top   <- round(top_n / sum(df$n) * 100, 1)
    n_ml_ia   <- sum(df$n[df$topic_label %in% c("Machine Learning","IA Generativa")])
    pct_ml_ia <- round(n_ml_ia / sum(df$n) * 100, 1)
    color_top <- COLORES_TEMA[top_cat]
    interp_ml <- if (pct_ml_ia < 5) {
      paste0("Los artículos computacionales (ML e IA) representan solo el ",
             pct_ml_ia, "%, consistente con el enfoque ecológico de la revista. Adicionalmente, dado el elevado número
             de artículos en categoria otros, los articulos publicados en las secciones especiales no se pueden apreciar
             de manera adecuada.")
    } else {
      paste0("Los artículos de ML e IA representan el ", pct_ml_ia,
             "%, reflejando la penetración de métodos computacionales en ecología.")
    }
    tags$p(class="text-muted mt-1", style="font-size:0.82rem;",
      icon("circle-info"), " La categoría ",
      tags$strong(style=paste0("color:",color_top), top_cat),
      " domina con ", tags$strong(paste0(pct_top, "%")),
      " (", top_n, " artículos). ", interp_ml)
  })

  # Gráfico 2: Publicaciones por mes — click envía mes
  output$chart_timeline <- renderHighchart({
    req(datos_actuales())
    df <- datos_actuales() |>
      mutate(mes=str_sub(publication_date,1,7)) |>
      count(mes) |> arrange(mes)
    highchart() |>
      hc_chart(type="line") |>
      hc_title(text="Publicaciones por mes") |>
      hc_xAxis(categories=df$mes, title=list(text="Mes"), labels=list(rotation=-45)) |>
      hc_yAxis(title=list(text="N° artículos"), min=0) |>
      hc_add_series(name="Publicaciones", data=df$n, color="#8CBF3C",
                    marker=list(enabled=TRUE),
                    cursor="pointer",
                    point=list(events=list(
                      click = JS("function() {
                        var cats = this.series.xAxis.categories;
                        var mes = cats ? cats[this.x] : this.x;
                        Shiny.setInputValue('click_timeline',
                          {mes: mes},
                          {priority: 'event'});
                      }")
                    ))) |>
      hc_tooltip(pointFormat="<b>{point.y}</b> artículos · clic para ver lista", shared=TRUE) |>
      hc_legend(enabled=FALSE)
  })

  output$interp_timeline <- renderUI({
    req(datos_actuales())
    df <- datos_actuales() |>
      mutate(mes=str_sub(publication_date,1,7)) |>
      count(mes) |> arrange(mes)
    if (nrow(df) == 0) return(NULL)
    mes_pico <- df$mes[which.max(df$n)]
    n_pico   <- max(df$n)
    media    <- round(mean(df$n), 1)
    varianza <- round(var(df$n), 1)
    desv_est <- round(sd(df$n), 1)
    tags$p(class="text-muted mt-1", style="font-size:0.82rem;",
      icon("circle-info"), " Mes con mayor actividad: ",
      tags$strong(mes_pico), " (", tags$strong(n_pico), " artículos). ",
      "Media: ", tags$strong(media),
      " · varianza: ", tags$strong(varianza),
      " · DE: ", tags$strong(desv_est), " artículos/mes.")
  })

  # Gráfico 3: Distribución de citas — click envía rango del bin
  output$chart_citas <- renderHighchart({
    req(datos_actuales())
    citas <- datos_actuales() |> filter(!is.na(citations)) |> pull(citations)
    validate(need(length(citas) > 0, "Sin datos de citas."))
    # MEJORA: manejar caso borde donde todas las citas son iguales
    if (length(unique(citas)) <= 1) {
      return(highchart() |>
        hc_title(text = "Distribución de citas") |>
        hc_subtitle(text = paste("Todos los artículos tienen", citas[1], "citas")))
    }
    hchart(citas, type="histogram", color="#dc2626") |>
      hc_title(text="Distribución de citas") |>
      hc_xAxis(title=list(text="Citas")) |>
      hc_yAxis(title=list(text="Frecuencia")) |>
      hc_tooltip(pointFormat="Rango: {point.x:.0f}–{point.x2:.0f} · Frecuencia: {point.y} · clic para ver") |>
      hc_legend(enabled=FALSE) |>
      hc_plotOptions(histogram = list(
        cursor = "pointer",
        point  = list(events = list(
          click = JS("function() {
            var xmax = (typeof this.x2 !== 'undefined') ? this.x2 :
                       (this.x + this.series.closestPointRange);
            Shiny.setInputValue('click_citas',
              {x_min: this.x, x_max: xmax},
              {priority: 'event'});
          }")
        ))
      ))
  })

  output$interp_citas <- renderUI({
    req(datos_actuales())
    citas <- datos_actuales() |> filter(!is.na(citations)) |> pull(citations)
    if (length(citas) == 0) return(NULL)
    mediana  <- median(citas)
    media    <- round(mean(citas), 1)
    varianza <- round(var(citas), 1)
    desv_est <- round(sd(citas), 1)
    pct_0    <- round(mean(citas == 0) * 100, 1)
    tags$p(class="text-muted mt-1", style="font-size:0.82rem;",
      icon("circle-info"),
      " Distribución sesgada a la derecha: mediana ", tags$strong(mediana),
      " · media ", tags$strong(media),
      " · varianza ", tags$strong(varianza),
      " · DE ", tags$strong(desv_est), " citas. ",
      "El ", tags$strong(paste0(pct_0, "%")), " sin citas aún.")
  })

  # Gráfico 4: Top 10 autores — click envía nombre del autor
  output$chart_top_autores <- renderHighchart({
    req(datos_actuales())
    autores_vec <- datos_actuales() |>
      filter(!is.na(authors_raw)) |> pull(authors_raw) |>
      strsplit("; ") |> unlist()
    validate(need(length(autores_vec) > 0, "Sin datos de autores."))
    df_aut <- autores_vec |> table() |> sort(decreasing=TRUE) |> head(10) |>
      as.data.frame()
    colnames(df_aut) <- c("autor","n")
    df_aut <- df_aut |> mutate(n=as.integer(n))
    # Puntos con name explícito → this.name seguro en JS
    data_aut <- lapply(seq_len(nrow(df_aut)), function(i) {
      list(name = as.character(df_aut$autor[i]), y = df_aut$n[i])
    })
    highchart() |>
      hc_chart(type = "bar") |>
      hc_title(text = "Top 10 autores más frecuentes") |>
      hc_xAxis(categories = as.character(df_aut$autor), title = list(text = "Autor")) |>
      hc_yAxis(title = list(text = "N° artículos"), min = 0) |>
      hc_add_series(name = "Artículos", data = data_aut,
                    color = "#7c3aed", showInLegend = FALSE) |>
      hc_legend(enabled = FALSE) |>
      hc_tooltip(pointFormat = "<b>{point.y}</b> artículos · clic para ver lista") |>
      hc_plotOptions(bar = list(
        cursor = "pointer",
        point  = list(events = list(
          click = JS("function() {
            Shiny.setInputValue('click_autores',
              {autor: this.name},
              {priority: 'event'});
          }")
        ))
      ))
  })

  output$interp_autores <- renderUI({
    req(datos_actuales())
    autores_vec <- datos_actuales() |>
      filter(!is.na(authors_raw)) |> pull(authors_raw) |>
      strsplit("; ") |> unlist()
    if (length(autores_vec) == 0) return(NULL)
    conteos  <- sort(table(autores_vec), decreasing=TRUE)
    top1     <- names(conteos)[1]
    top1_n   <- as.integer(conteos[1])
    n_unicos <- length(unique(autores_vec))
    media_art <- round(mean(as.integer(conteos)), 1)
    varianza  <- round(var(as.integer(conteos)), 1)
    desv_est  <- round(sd(as.integer(conteos)), 1)
    tags$p(class="text-muted mt-1", style="font-size:0.82rem;",
      icon("circle-info"), " Autor más frecuente: ",
      tags$strong(top1), " (", top1_n, " artículos). ",
      format(n_unicos, big.mark=","), " autores únicos. ",
      "Media de artículos por autor: ", tags$strong(media_art),
      " · varianza: ", tags$strong(varianza),
      " · DE: ", tags$strong(desv_est), ".")
  })

  # ════════════════════════════════════════════════════════
  # TABLA (Pestaña 2)
  # ════════════════════════════════════════════════════════

  papers_tabla <- reactive({
    leer_papers(filtros=list(
      fecha_ini = as.character(input$rango_fecha2[1]),
      fecha_fin = as.character(input$rango_fecha2[2]),
      topics    = if(input$tema_tabla=="Todos") NULL else input$tema_tabla,
      titulo    = input$titulo_tabla,
      autor     = input$autor_tabla
    ))
  })

  output$tabla_articulos <- DT::renderDataTable({
    req(papers_tabla())
    papers_tabla() |>
      mutate(
        Título = ifelse(
          !is.na(doi) & nchar(doi) > 0,
          paste0('<a href="https://doi.org/', doi, '" target="_blank">',
                 str_sub(title, 1, 60),
                 if_else(nchar(title) > 60, "...", ""), "</a>"),
          str_sub(title, 1, 60)
        )
      ) |>
      select(Título, Autores=authors_raw, Fecha=publication_date,
             Tema=topic_label, DOI=doi, Citas=citations, Accesses=downloads) |>
      DT::datatable(
        escape  = FALSE,
        options = list(pageLength=10, scrollX=TRUE,
                       language=list(url="//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
        rownames = FALSE, filter = "top"
      )
  })

  output$descargar_csv <- downloadHandler(
    filename = function() paste0("nature_eco_evo_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(papers_tabla(), file, row.names=FALSE)
  )

  # ════════════════════════════════════════════════════════
  # ACTUALIZACIÓN (Pestaña 3)
  # ════════════════════════════════════════════════════════

  output$log_actualizacion <- renderText({
    if (length(rv$log_lines) == 0) return("Sin ejecuciones recientes.")
    paste(rv$log_lines, collapse="\n")
  })

  output$timestamp_actualizacion <- renderText({
    if (is.null(rv$ultima_exec)) return("")
    paste("Última:", format(rv$ultima_exec, "%d/%m/%Y %H:%M:%S"))
  })

  output$contador_resultado <- renderUI({
    if (is.null(rv$nuevos_papers)) return(
      tags$div(class="card text-center p-3 bg-light",
        tags$div(style="font-size:2rem;color:#94a3b8;","—"),
        tags$small(class="text-muted","Presiona el botón para buscar")
      )
    )
    if (nrow(rv$nuevos_papers) == 0) return(
      tags$div(class="card text-center p-3 border-warning",
        tags$div(style="font-size:2rem;color:#f59e0b;","0"),
        tags$small(class="text-warning fw-bold","Artículos nuevos"),
        tags$br(),
        tags$small(class="text-muted","Citas de los últimos 5 actualizadas")
      )
    )
    tags$div(class="card text-center p-3 border-success",
      tags$div(style="font-size:2.5rem;font-weight:700;color:#16a34a;", nrow(rv$nuevos_papers)),
      tags$small(class="text-success fw-bold","Artículos nuevos guardados"),
      tags$br(),
      tags$small(class="text-muted",
                 paste(sum(!is.na(rv$nuevos_papers$downloads)),"con Accesses scrapeados"))
    )
  })

  output$tabla_ultimos_siempre <- DT::renderDataTable({
    rv$papers
    con <- dbConnect(RSQLite::SQLite(), DB_PATH)
    df  <- dbGetQuery(con,
      "SELECT title AS Título, authors_raw AS Autores,
              publication_date AS Fecha, topic_label AS Tema,
              citations AS Citas, downloads AS Accesses
       FROM papers ORDER BY publication_date DESC LIMIT 5")
    dbDisconnect(con)
    DT::datatable(df, options=list(pageLength=5, dom="t", scrollX=TRUE), rownames=FALSE)
  })

  observeEvent(input$btn_actualizar, {
    rv$log_lines   <- character(0)
    rv$ultima_exec <- Sys.time()

    withProgress(message="Consultando OpenAlex...", value=0, {
      incProgress(0.10, detail="Buscando última fecha...")
      con          <- dbConnect(RSQLite::SQLite(), DB_PATH)
      ultima_fecha <- dbGetQuery(con,"SELECT MAX(publication_date) AS uf FROM papers")$uf
      n_actual     <- dbGetQuery(con,"SELECT COUNT(*) AS n FROM papers")$n
      dbDisconnect(con)
      if (is.na(ultima_fecha)) ultima_fecha <- "2025-01-01"
      agregar_log(paste("BD actual:", n_actual, "artículos · Última fecha:", ultima_fecha))

      incProgress(0.20, detail="Consultando OpenAlex...")
      agregar_log("Consultando OpenAlex API...")
      nuevos <- buscar_papers_nuevos(ultima_fecha)
      agregar_log(paste("OpenAlex retornó:", nrow(nuevos), "resultados"))

      if (nrow(nuevos) > 0) {
        con            <- dbConnect(RSQLite::SQLite(), DB_PATH)
        ids_existentes <- dbGetQuery(con,"SELECT paper_id FROM papers")$paper_id
        dbDisconnect(con)
        nuevos_reales <- nuevos |> filter(!paper_id %in% ids_existentes, !is.na(title))
        agregar_log(paste("Artículos genuinamente nuevos:", nrow(nuevos_reales)))

        if (nrow(nuevos_reales) > 0) {
          nuevos_reales <- clasificar_tema(nuevos_reales)
          agregar_log("Clasificación temática completada")
          incProgress(0.40, detail="Scrapeando Accesses...")
          agregar_log(paste("Scrapeando Accesses de", nrow(nuevos_reales), "artículos..."))
          for (i in seq_len(nrow(nuevos_reales))) {
            acc <- scrape_accesses_articulo(nuevos_reales$url[i])
            nuevos_reales$downloads[i] <- acc
            agregar_log(paste0("  [",i,"/",nrow(nuevos_reales),"] ",
                               str_sub(nuevos_reales$title[i],1,45),"... → ",
                               ifelse(is.na(acc),"NA",paste(acc,"Accesses"))))
            Sys.sleep(runif(1,1.5,3.5))
            incProgress(0.40/nrow(nuevos_reales))
          }
          incProgress(0.85, detail="Guardando en SQLite...")
          con <- dbConnect(RSQLite::SQLite(), DB_PATH)
          dbWriteTable(con,"papers",nuevos_reales,append=TRUE,row.names=FALSE)
          dbDisconnect(con)
          rv$nuevos_papers <- nuevos_reales
          rv$papers        <- leer_papers()
          rv$indicadores   <- leer_indicadores()
          n_con_acc <- sum(!is.na(nuevos_reales$downloads))
          agregar_log(paste("Guardados:",nrow(nuevos_reales),"artículos |",n_con_acc,"con Accesses"))
          showNotification(paste(nrow(nuevos_reales),"artículos guardados."),type="message",duration=8)
        } else {
          incProgress(0.60, detail="Actualizando citas...")
          agregar_log("Sin artículos nuevos. Actualizando citas de los últimos 5...")
          con     <- dbConnect(RSQLite::SQLite(), DB_PATH)
          ultimos <- dbGetQuery(con,"SELECT paper_id FROM papers ORDER BY publication_date DESC LIMIT 5")
          dbDisconnect(con)
          for (pid in ultimos$paper_id) {
            pid_c     <- str_remove(pid,"https://openalex.org/")
            url_check <- paste0("https://api.openalex.org/works/",pid_c,
                                "?select=id,cited_by_count&mailto=setabaress@unal.edu.co")
            resp_c <- hacer_peticion_segura(url_check)
            if (!is.null(resp_c)) {
              d <- tryCatch(resp_c |> resp_body_json(simplifyVector=FALSE), error=function(e) NULL)
              if (!is.null(d) && !is.null(d$cited_by_count)) {
                con <- dbConnect(RSQLite::SQLite(), DB_PATH)
                dbExecute(con,"UPDATE papers SET citations = ? WHERE paper_id = ?",
                          params=list(as.integer(d$cited_by_count),pid))
                dbDisconnect(con)
                agregar_log(paste("  →",str_sub(pid_c,1,20),":",d$cited_by_count,"citas"))
              }
            }
            Sys.sleep(0.1)
          }
          rv$nuevos_papers <- tibble()
          rv$papers        <- leer_papers()
          rv$indicadores   <- leer_indicadores()
          agregar_log("Citas de los últimos 5 actualizadas")
          showNotification("Sin artículos nuevos. Citas actualizadas.",type="warning",duration=6)
        }
      } else {
        rv$nuevos_papers <- tibble()
        agregar_log("Sin respuesta de la API de OpenAlex")
        showNotification("Error al conectar con OpenAlex.",type="error")
      }
      incProgress(1.0, detail="Completado.")
    })
  })

  output$nuevos_papers_ui <- renderUI({
    req(input$btn_actualizar)
    if (is.null(rv$nuevos_papers) || nrow(rv$nuevos_papers)==0) return(NULL)
    tagList(
      tags$h5(paste(nrow(rv$nuevos_papers),"artículos nuevos encontrados:"),
              class="text-success fw-bold"),
      DT::dataTableOutput("tabla_nuevos"))
  })
  output$tabla_nuevos <- DT::renderDataTable({
    req(rv$nuevos_papers)
    if (nrow(rv$nuevos_papers)==0) return(NULL)
    rv$nuevos_papers |>
      select(Título=title,Autores=authors_raw,Fecha=publication_date,
             Tema=topic_label,DOI=doi,Citas=citations,Accesses=downloads) |>
      DT::datatable(options=list(pageLength=5,scrollX=TRUE),rownames=FALSE)
  })

  # Descarga CSV del modal
  output$descargar_modal_csv <- downloadHandler(
    filename = function() paste0("seleccion_modal_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(rv$modal_data)
      write.csv(rv$modal_data, file, row.names = FALSE)
    }
  )

  # Tabla metadata (Metodología)
  output$tabla_metadata <- DT::renderDataTable({
    tibble(
      Columna=c("paper_id","journal_name","title","publication_date","year","doi",
                "url","abstract","authors_raw","n_authors","citations",
                "downloads","n_references","topic_label"),
      Tipo=c("TEXT PK","TEXT","TEXT","TEXT","INTEGER","TEXT","TEXT","TEXT",
             "TEXT","INTEGER","INTEGER","INTEGER","INTEGER","TEXT"),
      Fuente=c("OpenAlex","Fijo","OpenAlex","OpenAlex","OpenAlex","OpenAlex",
               "DOI → URL","OpenAlex (índice invertido)","OpenAlex",
               "Conteo","OpenAlex","Scraping nature.com","OpenAlex","Keywords")
    ) |>
      DT::datatable(options=list(pageLength=14,dom="t"),rownames=FALSE)
  })
}

shinyApp(ui, server)
