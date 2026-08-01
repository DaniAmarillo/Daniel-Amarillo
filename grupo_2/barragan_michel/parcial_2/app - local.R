library(shiny)
library(bslib)
library(DBI)
library(bigrquery)
library(DT)
library(highcharter)
library(dplyr)
library(future)
library(promises)
library(shinyjs)
library(lubridate)

plan(multisession)


# =============================================================================
# Conexión a BigQuery
# =============================================================================
# bq_auth(path = "keys/BigQuery.json")
# 
# con <- dbConnect(
#   bigrquery::bigquery(),
#   project = "jss-dashboard-498723",
#   dataset = "JSS",
#   billing = "jss-dashboard-498723"
# )

# ============================================================================
# CONEXIÓN EN LOCAL
# ============================================================================

con <- dbConnect(RSQLite::SQLite(), 'data/JSS no updated.sqlite')

# =============================================================================
# Helper: calcular estadísticas de boxplot para un vector numérico
# =============================================================================
boxplot_stats <- function(x) {
  x   <- x[!is.na(x)]
  q   <- quantile(x, probs = c(0.25, 0.5, 0.75))
  iqr <- q[3] - q[1]
  list(
    low    = max(min(x), q[1] - 1.5 * iqr),
    q1     = unname(q[1]),
    median = unname(q[2]),
    q3     = unname(q[3]),
    high   = min(max(x), q[3] + 1.5 * iqr)
  )
}

# =============================================================================
# Helper: construir datos de boxplot agrupados
# =============================================================================
build_boxplot_data <- function(df, grupo_col, valor_col) {
  grupos <- sort(unique(df[[grupo_col]]))
  list(
    categorias = as.character(grupos),
    datos      = lapply(grupos, function(g) {
      vals <- df[[valor_col]][df[[grupo_col]] == g]
      s    <- boxplot_stats(vals)
      list(s$low, s$q1, s$median, s$q3, s$high)
    })
  )
}

# =============================================================================
# Helper: renderizar highchart de boxplot
# =============================================================================
render_boxplot_hc <- function(bp, titulo, ylab, color = "#2a6496") {
  highchart() |>
    hc_chart(type = "boxplot") |>
    hc_title(text = titulo) |>
    hc_xAxis(categories = bp$categorias) |>
    hc_yAxis(title = list(text = ylab)) |>
    hc_add_series(
      name      = ylab,
      data      = bp$datos,
      color     = color,
      fillColor = paste0(color, "33")
    ) |>
    hc_legend(enabled = FALSE) |>
    hc_credits(enabled = FALSE)
}

# =============================================================================
# CARGA INICIAL (fuera de UI y server)
# =============================================================================

# Artículos
art_raw <- dbGetQuery(con, "
  SELECT
    DOI,
    title,
    date,
    abstract,
    no_authors,
    no_citations,
    no_references,
    importance_ratio,
    topic,
    vol_no
  FROM articles
  ORDER BY date
")

art_raw$date <- lubridate::mdy(art_raw$date)
art_raw$year <- as.integer(format(art_raw$date, "%Y"))

# Volúmenes con métricas base
vol_raw <- dbGetQuery(con, "
  SELECT
    v.year                                                        AS year,
    v.no                                                          AS volumen,
    v.url                                                         AS url,
    MIN(a.date)                                                   AS fecha,
    COUNT(a.DOI)                                                  AS articulos,
    ROUND(AVG(a.no_citations), 1)                                 AS citas_promedio,
    SUM(a.no_citations)                                           AS citas_total,
    ROUND(AVG(a.no_references), 1)                                AS referencias_promedio,
    SUM(CASE WHEN a.topic = 'Machine Learning' THEN 1 ELSE 0 END) AS machine_learning,
    SUM(CASE WHEN a.topic = 'Generative AI' THEN 1 ELSE 0 END)    AS generative_ai,
    SUM(CASE WHEN a.topic = 'Statistics' THEN 1 ELSE 0 END)       AS statistics,
    SUM(CASE WHEN a.topic = 'Other' THEN 1 ELSE 0 END)            AS other
  FROM volumes v
  LEFT JOIN articles a ON v.no = a.vol_no
  GROUP BY v.no, v.year, v.url
  ORDER BY v.year, v.no
")

vol_raw$fecha <- lubridate::mdy(vol_raw$fecha)

# Referencias y autores (no se filtran, se cargan una vez)
referencias_raw <- dbGetQuery(con, "
  SELECT
    ar.DOI AS DOI_origen,
    r.*
  FROM article_references ar
  LEFT JOIN refs r
    ON r.ref_url = ar.ref_url
")

autores_raw <- dbGetQuery(con, "
  SELECT
    aa.DOI,
    a.*
  FROM article_authors aa
  LEFT JOIN authors a
    ON aa.authorId = a.authorId
  ORDER BY no_papers
")

# Filtros:
fechas <- dbGetQuery(con, "
  SELECT date
  FROM articles
  WHERE date IS NOT NULL
")

fechas$date <- lubridate::mdy(fechas$date)

fecha_limits <- data.frame(
  fecha_min = min(fechas$date, na.rm = TRUE),
  fecha_max = max(fechas$date, na.rm = TRUE)
)

tematicas_unicas <- dbGetQuery(con, "
  SELECT DISTINCT topic
  FROM articles
  WHERE topic IS NOT NULL
  ORDER BY topic
")$topic


DOIs <- unique(art_raw$DOI)

# =============================================================================
# UI
# =============================================================================
ui <- page_sidebar(
  
  title = div(
    style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
    div(
      style = "display: flex; align-items: center;",
      img(
        src   = "https://www.zeileis.org/images/logo-jstatsoft.jpg",
        height = "40px",
        style  = "margin-right: 10px;"
      ),
      div(
        style = "display: flex; flex-direction: column; line-height: 1.2;",
        span("Journal of Statistical Software",
             style = "font-size: 1rem; font-weight: 600;"),
        span("Historial de publicaciones", style = "font-size: 0.75rem; opacity: 0.85;")
      )
    ),
    div(
      style = "display: flex; align-items: center; gap: 12px;",
      tags$a(
        href  = "mailto:mbarraganz@unal.edu.co",
        style = "color: white; text-decoration: none; font-weight: 500;",
        "Mendivenson Barragán"
      ),
      tags$a(
        href   = "https://github.com/Mendivenson/data-mining",
        target = "_blank",
        style  = "color: white; line-height: 1;",
        tags$i(class = "bi bi-github", style = "font-size: 1.3rem;")
      ),
      tags$a(
        href   = "https://www.linkedin.com/in/mendivenson/",
        target = "_blank",
        style  = "color: white; line-height: 1;",
        tags$i(class = "bi bi-linkedin", style = "font-size: 1.3rem;")
      )
    )
  ),
  
  # ------------------------------------------------------------------
  # Sidebar: navegación + filtros
  # ------------------------------------------------------------------
  sidebar = sidebar(
    
    navset_pill_list(
      id = "pestana",
      nav_panel(value = "volumenes",
                title = tagList(tags$i(class = "bi bi-collection"), " Volumenes")),
      nav_panel(value = "articulos",
                title = tagList(tags$i(class = "bi bi-journal-text"), " Artículos")),
      nav_panel(value = "referencias",
                title = tagList(tags$i(class = "bi bi-bookmarks"), " Referencias")),
      nav_panel(value = "autores",
                title = tagList(tags$i(class = "bi bi-people"), " Autores"))
    ),
    
    hr(style = "border-top: 1px solid #ccc; margin: 8px 0 4px 0;"),
    
    div(
      style = "font-size: 0.75rem; color: #666; margin-bottom: 0px; font-weight: 600;",
      tags$i(class = "bi bi-funnel"), " Filtros"
    ),
    
    div(
      style = "font-size: 0.72rem; color: #555; margin-bottom: 1px; font-weight: 600; display: flex; align-items: center; gap: 4px;",
      "Rango de fechas",
      tags$i(
        class = "bi bi-question-circle",
        title = "Aplica a: Volúmenes, Artículos, Referencias, Autores",
        style = "font-size: 0.75rem; color: #888; cursor: help;"
      )
    ),
    dateRangeInput(
      inputId   = "fecha_range",
      label     = NULL,
      start     = fecha_limits$fecha_min,
      end       = fecha_limits$fecha_max,
      min       = fecha_limits$fecha_min,
      max       = fecha_limits$fecha_max,
      format    = "yyyy-mm-dd",
      separator = "–",
      language  = "es"
    ),
    
    div(
      style = "font-size: 0.72rem; color: #555; margin-bottom: 1px; margin-top: 2px; font-weight: 600; display: flex; align-items: center; gap: 4px;",
      "Temáticas",
      tags$i(
        class = "bi bi-question-circle",
        title = "Aplica a: Artículos, Referencias, Autores",
        style = "font-size: 0.75rem; color: #888; cursor: help;"
      )
    ),
    checkboxGroupInput(
      inputId  = "tematicas",
      label    = NULL,
      choices  = tematicas_unicas,
      selected = tematicas_unicas
    ),
    
    div(
      style = "font-size: 0.72rem; color: #555; margin-bottom: 1px; margin-top: 2px; font-weight: 600; display: flex; align-items: center; gap: 4px;",
      "DOIs",
      tags$i(
        class = "bi bi-question-circle",
        title = "Aplica a: Artículos, Referencias, Autores",
        style = "font-size: 0.75rem; color: #888; cursor: help;"
      )
    ),
    selectizeInput(
      inputId  = "filtro_doi",
      label    = NULL,
      choices  = DOIs,
      selected = NULL,
      multiple = TRUE,
      options  = list(
        placeholder = "Buscar DOI...",
        plugins     = list("remove_button")
      )
    )),
  
  # ------------------------------------------------------------------
  # Subheader + contenido principal
  # ------------------------------------------------------------------
  uiOutput("subheader"),
  uiOutput("contenido"),
  
  # ------------------------------------------------------------------
  # Tema
  # ------------------------------------------------------------------
  theme = bs_theme(
    bg        = "#ffffff",
    fg        = "#333333",
    primary   = "#2a6496",
    navbar_bg = "#2a6496",
    base_font = font_google("Source Sans Pro")
  ),
  
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.0/font/bootstrap-icons.css"
    )
  ),
  tags$style(HTML("

  /* ============================================================
     NAVEGACIÓN: pills del sidebar
     ============================================================ */
  .nav-pills .nav-link.active {
    background-color: #2a6496 !important;
    color: white !important;
  }
  .nav-pills .nav-link:hover {
    background-color: #d6e4f0 !important;
    color: #2a6496 !important;
  }
  .nav-pills .nav-link {
    display: flex !important;
    align-items: center !important;
    gap: 6px !important;
    white-space: nowrap !important;
    margin-right: 8px !important;
  }

  /* ============================================================
     SIDEBAR: dimensiones y espaciado
     ============================================================ */
  .well {
    background: transparent !important;
    border: none !important;
    box-shadow: none !important;
  }
  .bslib-sidebar-layout > .sidebar {
    width: 190px !important;
    min-width: 180px !important;
  }

  /* ============================================================
     SUBHEADER: barra fija debajo del header principal
     ============================================================ */
  .subheader-bar {
    position: fixed;
    top: 45px;
    left: 0;
    right: 0;
    z-index: 1020;
    background-color: #2a6496;
    color: white;
    padding: 4px 16px;
    display: flex;
    gap: 8px;
    align-items: stretch;
    box-shadow: 0 2px 4px rgba(0,0,0,0.2);
    justify-content: center;
  }
  .subheader-bar .fc-card {
    background-color: rgba(255,255,255,0.12);
    border-radius: 5px;
    padding: 3px 10px;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    flex: 1;
    min-width: 90px;
  }
  .subheader-bar .fc-label {
    font-size: 0.6rem;
    opacity: 0.8;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    white-space: nowrap;
  }
  .subheader-bar .fc-value {
    font-size: 0.82rem;
    font-weight: 700;
    white-space: nowrap;
  }
  /* Empuja el contenido para que no quede tapado por el subheader */
  .bslib-sidebar-layout {
    margin-top: 40px !important;
  }

  /* ============================================================
     TABLAS: ocultar buscador nativo de DataTables
     ============================================================ */
  .dataTables_filter { display: none !important; }
  .filters input, .filters select {
    width: 100%;
    font-size: 0.8rem;
    padding: 2px 4px;
    border: 1px solid #ccc;
    border-radius: 4px;
  }

  /* ============================================================
     GRÁFICOS: títulos verticales
     ============================================================ */
  .chart-title-vertical {
    writing-mode: vertical-rl;
    transform: rotate(180deg);
    font-size: 0.85rem;
    font-weight: 600;
    color: #2a6496;
    white-space: nowrap;
    padding-right: 6px;
    align-self: center;
  }
  .chart-wrapper {
    display: flex;
    align-items: stretch;
  }

  /* ============================================================
     FILTROS: dateRangeInput compacto
     ============================================================ */
  .shiny-input-container .input-daterange {
    font-size: 0.72rem !important;
  }
  .shiny-input-container .input-daterange input {
    font-size: 0.72rem !important;
    padding: 2px 4px !important;
  }
  .shiny-input-container:has(.input-daterange) {
    margin-bottom: 2px !important;
  }

  /* ============================================================
     FILTROS: checkboxGroupInput compacto
     ============================================================ */
  .shiny-input-container .checkbox {
    margin-top: 1px !important;
    margin-bottom: 1px !important;
  }
  .shiny-input-container .checkbox label {
    font-size: 0.75rem;
  }
  .shiny-input-container:has(.shiny-options-group) {
    margin-bottom: 0 !important;
  }
  
  /* =============================================================
     BOTÓN ACTUALIZACIÓN
     ============================================================= */
    /* Tarjeta contenedora */
  .fc-card-update {
  display: flex;
  align-items: center;
  justify-content: center;

  flex: 0 0 auto;
  min-width: unset;
  width: auto;
  
  /* Botón */
  .btn-update {
    background-color: #c0392b !important;
    border: none !important;
    color: white !important;
    font-weight: 600 !important;
    font-size: 0.8rem !important;
  
    height: 50% !important;
    min-height: 26px;
  
    border-radius: 5px !important;
    padding: 0 14px !important;
  }
  
  .btn-update:hover {
    background-color: #a93226 !important;
    color: white !important;
  }
  
"))
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
  
  observe({
    req(input$fecha_range, input$tematicas)
    
    dois_disponibles <- art_raw |>
      filter(
        date >= as.Date(input$fecha_range[1]),
        date <= as.Date(input$fecha_range[2]),
        topic %in% input$tematicas
      ) |>
      pull(DOI) |>
      unique()
    
    updateSelectizeInput(
      session,
      inputId  = "filtro_doi",
      choices  = dois_disponibles,
      selected = intersect(input$filtro_doi, dois_disponibles)
    )
  })
  
  
  # ------------------------------------------------------------------
  # Datos reactivos de artículos (filtrados por fecha Y temática)
  # Usados en: pestaña Artículos + footer
  # ------------------------------------------------------------------
  art_data <- reactive({
    req(input$fecha_range, input$tematicas)
    
    df <- art_raw
    df <- df[df$date >= as.Date(input$fecha_range[1]) & df$date <= as.Date(input$fecha_range[2]), ]
    df <- df[df$topic %in% input$tematicas, ]
    
    # Filtro DOI solo si hay algo seleccionado
    if (length(input$filtro_doi) > 0) {
      df <- df[df$DOI %in% input$filtro_doi, ]
    }
    
    df
  })
  
  # ------------------------------------------------------------------
  # Datos reactivos de volúmenes (filtrados solo por fecha)
  # ------------------------------------------------------------------
  vol_data <- reactive({
    req(input$fecha_range)
    df <- vol_raw
    df <- df[df$fecha >= as.Date(input$fecha_range[1]) & as.Date(df$fecha) <= as.Date(input$fecha_range[2]), ]
    df
  })
  
  
  
  # ------------------------------------------------------------------
  # Header reactivo — métricas desde art_data() (ambos filtros aplicados)
  # ------------------------------------------------------------------
  output$subheader <- renderUI({
    df <- art_data()
    
    fecha_inicio <- format(input$fecha_range[1], "%d %b %Y")
    fecha_fin    <- format(input$fecha_range[2], "%d %b %Y")
    
    temas_label <- if (length(input$tematicas) == length(tematicas_unicas)) {
      "Todas"
    } else {
      paste(input$tematicas, collapse = ", ")
    }
    
    total_art    <- nrow(df)
    prom_autores <- if (total_art > 0) round(mean(df$no_authors,    na.rm = TRUE), 1) else "—"
    prom_citas   <- if (total_art > 0) round(mean(df$no_citations,  na.rm = TRUE), 1) else "—"
    prom_refs    <- if (total_art > 0) round(mean(df$no_references, na.rm = TRUE), 1) else "—"
    
    fc <- function(label, value) {
      div(class = "fc-card",
          div(class = "fc-label", label),
          div(class = "fc-value", value))
    }
    
    tags$div(
      class = "subheader-bar",
      
      fc("Rango de fechas",        paste(fecha_inicio, "–", fecha_fin)),
      fc("Temáticas",              temas_label),
      fc("Total artículos",        format(total_art, big.mark = ",")),
      fc("Prom. autores/art.",     as.character(prom_autores)),
      fc("Prom. citas/art.",       as.character(prom_citas)),
      fc("Prom. referencias/art.", as.character(prom_refs)),
      
      div(
        class = "fc-card-update",
        uiOutput("boton_actualizar")
      )
    )
  })
  
  # ------------------------------------------------------------------
  # UI dinámica: renderiza el contenido según la pestaña activa
  # ------------------------------------------------------------------
  output$contenido <- renderUI({
    switch(req(input$pestana),
           
           "volumenes" = div(
             h4(tagList(tags$i(class = "bi bi-collection"), " Volúmenes"),
                style = "color: #2a6496; margin-bottom: 15px;"),
             fluidRow(
               column(6,
                      card(
                        div(class = "chart-wrapper",
                            div("Composición temática", class = "chart-title-vertical"),
                            highchartOutput("hc_donut", height = "280px", width = "100%")
                        ),
                        style = "margin-bottom: 12px;"
                      ),
                      card(
                        div(class = "chart-wrapper",
                            div("Artículos por volumen", class = "chart-title-vertical"),
                            highchartOutput("hc_stacked_articulos", height = "280px", width = "100%")
                        ),
                        style = "margin-bottom: 12px;"
                      ),
                      card(
                        div(class = "chart-wrapper",
                            div("Citas totales", class = "chart-title-vertical"),
                            highchartOutput("hc_citas_total", height = "280px", width = "100%")
                        ),
                        style = "margin-bottom: 12px;"
                      ),
                      card(
                        div(class = "chart-wrapper",
                            div("Prom. referencias", class = "chart-title-vertical"),
                            highchartOutput("hc_referencias_promedio", height = "280px", width = "100%")
                        )
                      )
               ),
               column(6,
                      DTOutput("tabla_volumenes"), 
                      style = "height: 1400px;"
               )
             )
           ),
           
           "articulos" = div(
             h4(tagList(tags$i(class = "bi bi-journal-text"), " Artículos"),
                style = "color: #2a6496; margin-bottom: 15px;"),
             fluidRow(
               column(5,
                      card(
                        div(class = "chart-wrapper",
                            div("Métricas por temática", class = "chart-title-vertical"),
                            div(style = "flex: 1;",
                                highchartOutput("hc_box_topic_citas",       height = "260px"),
                                highchartOutput("hc_box_topic_importance",  height = "260px")
                            )
                        ),
                        style = "margin-bottom: 12px;"
                      ),
                      card(
                        div(class = "chart-wrapper",
                            div("Métricas por año", class = "chart-title-vertical"),
                            div(style = "flex: 1;",
                                highchartOutput("hc_box_year_citas",       height = "260px"),
                                highchartOutput("hc_box_year_importance",  height = "260px")
                            )
                        )
                      )
               ),
               column(7,
                      card(
                        DTOutput("tabla_articulos"), 
                        style = "height: 1120px;")
               )
             )
           ),
           "referencias" = div(
             h4(
               tagList(tags$i(class = "bi bi-bookmarks"), " Referencias"),
               style = "color: #2a6496; margin-bottom: 15px;"
             ),
             
             fluidRow(
               column(
                 5,
                 
                 card(
                   highchartOutput("hc_ref_scatter", height = "320px"),
                   style = "margin-bottom: 12px;"
                 ),
                 
                 fluidRow(
                   column(
                     6,
                     card(
                       h5("Prom. Citas", 
                          style = "text-align:center; height: 15px"),
                       uiOutput("avg_cited_by"),
                     )
                   ),
                   column(
                     6,
                     card(
                       h5("Prom. Citas en JSS", style = "text-align:center; height: 15px"),
                       uiOutput("avg_jss_cites")
                     )
                   )
                 )
               ),
               
               column(
                 7,
                 card(
                   DTOutput("tabla_referencias"),
                   style = "height: 500px;"
                 )
               )
             )
           ), 
           
           "autores" = div(
             h4(
               tagList(tags$i(class = "bi bi-people"), " Autores"),
               style = "color: #2a6496; margin-bottom: 15px;"
             ),
             fluidRow(
               
               column(
                 6,
                 
                 fluidRow(
                   column(
                     6,
                     card(
                       highchartOutput(
                         "hc_authors_papers",
                         height = "250px"
                       )
                     )
                   ),
                   
                   column(
                     6,
                     card(
                       highchartOutput(
                         "hc_authors_citations",
                         height = "250px"
                       )
                     )
                   )
                 ),
                 
                 card(
                   highchartOutput(
                     "hc_authors_scatter",
                     height = "300px"
                   )
                 )
                 
               ),
               
               column(
                 6,
                 
                 card(
                   DTOutput("tabla_autores"),
                   style = "height: 640px;"
                 )
                 
               )
               
             )
           )
    )
  })
  
  # ================================================================
  # PESTAÑA: VOLÚMENES
  # ================================================================
  
  etiquetas_anio <- function(df) {
    ifelse(duplicated(df$year), "", as.character(df$year))
  }
  
  output$hc_donut <- renderHighchart({
    df <- vol_data()
    
    totales <- data.frame(
      name  = c("Machine Learning", "Generative AI", "Statistics", "Other"),
      y     = c(sum(df$machine_learning), sum(df$generative_ai),
                sum(df$statistics),       sum(df$other)),
      color = c("#2a6496", "#e74c3c", "#27ae60", "#95a5a6")
    )
    
    highchart() |>
      hc_chart(type = "pie") |>
      hc_plotOptions(
        pie = list(
          innerSize  = "55%",
          dataLabels = list(enabled = TRUE,
                            format  = "<b>{point.name}</b>: {point.percentage:.1f}%")
        )
      ) |>
      hc_add_series(name = "Artículos", data = list_parse(totales)) |>
      hc_tooltip(pointFormat = "<b>{point.percentage:.1f}%</b> ({point.y} artículos)") |>
      hc_credits(enabled = FALSE)
  })
  
  output$hc_stacked_articulos <- renderHighchart({
    df  <- vol_data()
    etq <- etiquetas_anio(df)
    
    puntos <- function(df, col) {
      lapply(seq_len(nrow(df)), function(i) {
        total <- df$articulos[i]
        list(
          y       = df[[col]][i],
          vol     = df$volumen[i],
          anio    = df$year[i],
          total   = total,
          pct_ml  = if (total > 0) round(df$machine_learning[i] / total * 100, 1) else 0,
          pct_gen = if (total > 0) round(df$generative_ai[i]    / total * 100, 1) else 0,
          pct_sta = if (total > 0) round(df$statistics[i]        / total * 100, 1) else 0
        )
      })
    }
    
    highchart() |>
      hc_chart(type = "area") |>
      hc_xAxis(categories = etq, title = list(text = "Año"),
               labels = list(rotation = -90)) |>
      hc_yAxis(title = list(text = "Artículos")) |>
      hc_plotOptions(area = list(stacking = "normal", marker = list(enabled = FALSE))) |>
      hc_add_series(name = "Machine Learning", data = puntos(df, "machine_learning"), color = "#2a6496") |>
      hc_add_series(name = "Generative AI",    data = puntos(df, "generative_ai"),    color = "#e74c3c") |>
      hc_add_series(name = "Statistics",       data = puntos(df, "statistics"),       color = "#27ae60") |>
      hc_add_series(name = "Other",            data = puntos(df, "other"),            color = "#95a5a6") |>
      hc_tooltip(
        shared = FALSE, useHTML = TRUE, headerFormat = "",
        pointFormat = "
          <b>Vol. {point.vol} ({point.anio})</b><br/>
          Artículos: <b>{point.total}</b><br/>
          % ML: <b>{point.pct_ml}%</b><br/>
          % Generative AI: <b>{point.pct_gen}%</b><br/>
          % Statistics: <b>{point.pct_sta}%</b>
        "
      ) |>
      hc_credits(enabled = FALSE)
  })
  
  output$hc_citas_total <- renderHighchart({
    df  <- vol_data()
    etq <- etiquetas_anio(df)
    
    highchart() |>
      hc_chart(type = "line") |>
      hc_xAxis(categories = etq, title = list(text = "Año"),
               labels = list(rotation = -90)) |>
      hc_yAxis(title = list(text = "Citas totales")) |>
      hc_add_series(
        name = "Citas totales", color = "#8e44ad",
        marker = list(enabled = TRUE, radius = 3),
        data = lapply(seq_len(nrow(df)), function(i) {
          list(y = df$citas_total[i], vol = df$volumen[i], anio = df$year[i])
        })
      ) |>
      hc_tooltip(useHTML = TRUE, headerFormat = "",
                 pointFormat = "<b>Vol. {point.vol} ({point.anio})</b><br/>Citas: <b>{point.y}</b>") |>
      hc_legend(enabled = FALSE) |>
      hc_credits(enabled = FALSE)
  })
  
  output$hc_referencias_promedio <- renderHighchart({
    df  <- vol_data()
    etq <- etiquetas_anio(df)
    
    highchart() |>
      hc_chart(type = "line") |>
      hc_xAxis(categories = etq, title = list(text = "Año"),
               labels = list(rotation = -90)) |>
      hc_yAxis(title = list(text = "Promedio de referencias")) |>
      hc_add_series(
        name = "Promedio de referencias", color = "#e67e22",
        marker = list(enabled = TRUE, radius = 3),
        data = lapply(seq_len(nrow(df)), function(i) {
          list(y = df$referencias_promedio[i], vol = df$volumen[i], anio = df$year[i])
        })
      ) |>
      hc_tooltip(useHTML = TRUE, headerFormat = "",
                 pointFormat = "<b>Vol. {point.vol} ({point.anio})</b><br/>Prom. referencias: <b>{point.y}</b>") |>
      hc_legend(enabled = FALSE) |>
      hc_credits(enabled = FALSE)
  })
  
  output$tabla_volumenes <- renderDT({
    volumenes <- vol_data()
    
    volumenes$url <- paste0(
      '<a href="', volumenes$url, '" target="_blank">',
      '<i class="bi bi-box-arrow-up-right"></i></a>'
    )
    
    volumenes <- volumenes[, c("volumen", "year", "articulos", "citas_promedio",
                               "citas_total", "referencias_promedio", "url")]
    
    datatable(
      volumenes, escape = FALSE, filter = "none",
      options = list(
        pageLength = 25, scrollX = FALSE,dom = "lrtip",
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Spanish.json")
      ),
      rownames = FALSE,
      colnames = c("Vol.", "Año", "Artículos", "Citas promedio",
                   "Citas totales", "Prom. referencias", "")
    )
  })
  
  # ================================================================
  # PESTAÑA: ARTÍCULOS
  # ================================================================
  
  output$hc_box_topic_citas <- renderHighchart({
    bp <- build_boxplot_data(art_data(), "topic", "no_citations")
    render_boxplot_hc(bp, "Citas por temática", "Citas", "#2a6496")
  })
  
  output$hc_box_topic_importance <- renderHighchart({
    df      <- art_data()
    df$imp  <- df$importance_ratio * 100
    bp      <- build_boxplot_data(df, "topic", "imp")
    render_boxplot_hc(bp, "Importancia por temática", "Importancia (%)", "#27ae60")
  })
  
  output$hc_box_year_citas <- renderHighchart({
    bp <- build_boxplot_data(art_data(), "year", "no_citations")
    render_boxplot_hc(bp, "Citas por año", "Citas", "#2a6496")
  })
  
  output$hc_box_year_importance <- renderHighchart({
    df      <- art_data()
    df$imp  <- df$importance_ratio * 100
    bp      <- build_boxplot_data(df, "year", "imp")
    render_boxplot_hc(bp, "Importancia por año", "Importancia (%)", "#27ae60")
  })
  
  output$tabla_articulos <- renderDT({
    df <- art_data()
    
    df$DOI <- paste0(
      '<a href="https://doi.org/', df$DOI, '" target="_blank">',
      df$DOI, '</a>'
    )
    
    df$importance_ratio <- ifelse(
      is.na(df$importance_ratio), NA,
      paste0(round(df$importance_ratio * 100, 1), "%")
    )
    
    df$abstract <- ifelse(
      is.na(df$abstract), "—",
      paste0(
        '<span title="', stringr::str_replace_all(df$abstract, '"', "'"), '" ',
        'style="cursor:help; white-space:nowrap; overflow:hidden; ',
        'display:inline-block; max-width:200px; text-overflow:ellipsis;">',
        df$abstract, '</span>'
      )
    )
    
    df <- df[, c("DOI", "title", "date", "abstract", "no_authors",
                 "no_citations", "no_references", "importance_ratio", "topic")]
    
    datatable(
      df, escape = FALSE, filter = "top",
      options = list(
        pageLength = 7, scrollX = FALSE, dom = "lrtip",
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Spanish.json")
      ),
      rownames = FALSE,
      colnames = c("DOI", "Título", "Fecha", "Abstract", "Autores",
                   "Citas", "Referencias", "Importancia", "Tema")
    )
  })
  
  # ================================================================
  # PESTAÑA: REFERENCIAS
  # ================================================================
  
  referencias_plot_data <- reactive({
    
    req(input$fecha_range, input$tematicas)
    
    articulos_filtrados <- art_raw |>
      filter(
        date >= as.Date(input$fecha_range[1]),
        date <= as.Date(input$fecha_range[2]),
        topic %in% input$tematicas
      )
    
    referencias_raw |>
      filter(!is.na(ref_url) | !is.na(doi)) |>
      filter(DOI_origen %in% articulos_filtrados$DOI) |>
      group_by(ref_url, title, cited_by, doi) |>
      summarise(
        jss_cites = n(),
        .groups = "drop"
      )
    
  })
  
  
  output$hc_ref_scatter <- renderHighchart({
    
    df <- referencias_plot_data()
    
    highchart() |>
      hc_chart(type = "scatter", zoomType = "xy") |>
      hc_xAxis(
        title = list(text = "Log Citas totales"),
        type = "logarithmic"
      ) |>
      hc_yAxis(
        title = list(text = "Log Citas en JSS"),
        type = "logarithmic"
      ) |>
      hc_add_series(
        data = purrr::map(
          seq_len(nrow(df)),
          ~list(
            x = df$cited_by[.x],
            y = df$jss_cites[.x],
            title = df$title[.x]
          )
        ),
        name = "Referencias"
      ) |>
      hc_tooltip(
        useHTML = TRUE,
        pointFormat =
          "<b>{point.title}</b><br/>
         Citas: {point.x}<br/>
         Citas en JSS: {point.y}"
      ) |>
      hc_legend(enabled = FALSE) |>
      hc_credits(enabled = FALSE)
    
  })
  
  output$avg_cited_by <- renderUI({
    
    df <- referencias_plot_data()
    
    div(
      style = "
      text-align:center;
      font-size:34px;
      font-weight:600;
      color:#2a6496;
      padding:1px;
    ",
      scales::comma(round(mean(df$cited_by, na.rm = TRUE)))
    )
    
  })
  
  output$avg_jss_cites <- renderUI({
    
    df <- referencias_plot_data()
    
    div(
      style = "
      text-align:center;
      font-size:34px;
      font-weight:600;
      color:#2a6496;
      padding:1px;
    ",
      round(mean(df$jss_cites, na.rm = TRUE), 1)
    )
    
  })
  
  output$tabla_referencias <- renderDT({
    
    df <- referencias_raw |>
      filter(!is.na(ref_url) | !is.na(doi)) |> 
      filter(DOI_origen %in% art_data()$DOI) |>
      group_by(ref_url, title, cited_by, doi) |>
      summarise(jss_cites = n(), .groups = "drop") |>
      mutate(
        enlace = case_when(
          !is.na(doi) & doi != "" ~
            paste0(
              '<a href="https://doi.org/', doi,
              '" target="_blank"><i class="bi bi-box-arrow-up-right"></i></a>'
            ),
          !is.na(ref_url) & ref_url != "" ~
            paste0(
              '<a href="', ref_url,
              '" target="_blank"><i class="bi bi-box-arrow-up-right"></i></a>'
            ),
          TRUE ~ "—"
        )
      ) |>
      select(title, cited_by, jss_cites, enlace) |> 
      arrange(desc(jss_cites))
    
    datatable(
      df, escape = FALSE, filter = "none",
      options = list(
        pageLength = 20, scrollX = FALSE, dom = "lrtip",
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Spanish.json")
      ),
      rownames = FALSE,
      colnames = c("Título", "Citas", "Citas en JSS", "")
    )
  })
  
  # ================================================================
  # PESTAÑA: AUTORES (sin filtros)
  # ================================================================
  
  autores_data <- reactive({
    
    autores_raw |>
      filter(DOI %in% art_data()$DOI) |>
      group_by(authorId, name, ORCID, GoogleScholar, no_citations, no_papers) |>
      summarise(
        jss_papers = n(),
        .groups = "drop"
      )
    
  })
  
  output$hc_authors_papers <- renderHighchart({
    
    df <- autores_data()
    
    highchart() |>
      hc_add_series(
        data = list(boxplot.stats(df$no_papers)$stats),
        type = "boxplot"
      ) |>
      hc_title(text = "Papers totales") |>
      hc_xAxis(
        categories = c(""),
        labels = list(enabled = FALSE)
      ) |>
      hc_legend(enabled = FALSE)
    
  })
  
  output$hc_authors_citations <- renderHighchart({
    
    df <- autores_data()
    
    highchart() |>
      hc_add_series(
        data = list(boxplot.stats(df$no_citations)$stats),
        type = "boxplot"
      ) |>
      hc_title(text = "Citas totales") |>
      hc_xAxis(
        categories = c(""),
        labels = list(enabled = FALSE)
      ) |>
      hc_legend(enabled = FALSE)
    
  })
  
  output$hc_authors_scatter <- renderHighchart({
    
    df <- autores_data()
    highchart() |>
      hc_add_series(
        data = purrr::map(
          seq_len(nrow(df)),
          \(i) list(
            x = df$no_papers[i],
            y = df$jss_papers[i],
            name = df$name[i]
          )
        ),
        type = "scatter"
      ) |>
      hc_xAxis(
        title = list(text = "Papers totales")
      ) |>
      hc_yAxis(
        title = list(text = "Papers JSS")
      ) |>
      hc_tooltip(
        pointFormat =
          "<b>{point.name}</b><br>
         Papers: {point.x}<br>
         JSS papers: {point.y}"
      ) |> 
      hc_legend(enabled = FALSE)
  })
  
  output$tabla_autores <- renderDT({
    
    df <- autores_data() |>
      mutate(
        ORCID = ifelse(
          is.na(ORCID) | ORCID == "",
          "—",
          paste0(
            '<a href="https://orcid.org/',
            ORCID,
            '" target="_blank">',
            '<i class="bi bi-box-arrow-up-right"></i></a>'
          )
        ),
        GoogleScholar = ifelse(
          is.na(GoogleScholar) | GoogleScholar == "",
          "—",
          paste0(
            '<a href="',
            GoogleScholar,
            '" target="_blank">',
            '<i class="bi bi-box-arrow-up-right"></i></a>'
          )
        )
      ) |>
      arrange(desc(jss_papers)) |>
      select(
        name,
        no_papers,
        jss_papers,
        no_citations,
        ORCID,
        GoogleScholar
      )
    
    datatable(
      df,
      escape = FALSE,
      filter = "none",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = "lrtip",
        language = list(
          url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Spanish.json"
        )
      ),
      rownames = FALSE,
      colnames = c(
        "Autor",
        "Papers",
        "Papers JSS",
        "Citas",
        "ORCID",
        "Scholar"
      )
    )
    
  })
  
  # ================================================================
  # BOTÓN DE ACTUALIZACIÓN
  # ================================================================
  
  actualizando <- reactiveVal(FALSE)
  actualizado  <- reactiveVal(FALSE)
  
  output$boton_actualizar <- renderUI({
    if (actualizando()) {
      tags$button(
        class    = "btn-update",
        disabled = TRUE,
        style    = "background-color: 
#f39c12 !important; color: white !important;",
        "Procesando..."
      )
    } else if (actualizado()) {
      tags$button(
        class    = "btn-update",
        disabled = TRUE,
        style    = "background-color: 
#27ae60 !important; color: white !important;",
        "Actualizado"
      )
    } else {
      actionButton(
        "btn_actualizar",
        "Actualizar",
        class = "btn-update"
      )
    }
  })
  
  observeEvent(input$btn_actualizar, {
    
    req(!actualizando())
    
    actualizando(TRUE)
    
    future({
      
      env <- new.env(parent = globalenv())
      
      source(
        "BigQuery build/local actualization.R",
        local = env
      )
      
      env$update_BD(unique(vol_raw$volumen), unique(art_raw$DOI), con)
      
    }) %...>% {
      
      resultado <- .
      
      actualizando(FALSE)
      actualizado(TRUE)
      
      showModal(
        modalDialog(
          
          title = "Actualización completada",
          
          tags$h4("Resumen"),
          
          tags$p(
            paste(
              "Artículos nuevos:",
              resultado$new_articles
            )
          ),
          
          tags$p(
            paste(
              "Referencias nuevas:",
              resultado$new_references
            )
          ),
          
          tags$p(
            paste(
              "Autores nuevos:",
              resultado$new_authors
            )
          ),
          
          tags$p(
            paste(
              "Duración:",
              resultado$duration,
              "segundos"
            )
          ),
          
          easyClose = TRUE,
          footer = modalButton("Cerrar")
        )
      )
      
    } %...!% {
      
      actualizando(FALSE)
      
      showModal(
        modalDialog(
          title = "Error",
          
          tags$pre(
            conditionMessage(.)
          ),
          
          easyClose = TRUE
        )
      )
      
    }
    
  })
}


shinyApp(ui, server)