

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = tags$span(
      tags$img(
        src    = "logo.png",
        height = "200px",
        style  = "border:2px solid red;"
      ),
      tags$span(
        "Parcial 2-Annual Reviews",
        style = "font-size:15px; vertical-align:middle;"
      )
    ),
    titleWidth = 320
  ),

  dashboardSidebar(disable = TRUE),

  dashboardBody(
    use_theme(tema_claro),
    tags$head(tags$style(HTML("
      .skin-blue .main-header .logo  { background:#2563EB!important; }
      .skin-blue .main-header .navbar { background:#2563EB!important; }
      .content-wrapper {
        background:#F8FAFC!important;
        margin-right:280px!important;
        margin-left:0!important;
      }
      .box { border-top:3px solid #3B82F6;
             box-shadow:0 1px 6px rgba(0,0,0,.08)!important; }
      .shiny-notification {
        background:#DBEAFE; color:#1E40AF;
        border:1px solid #93C5FD;
      }

      /* ── Right sidebar ── */
      #rsb {
        position:fixed; top:50px; right:0;
        width:272px; height:calc(100vh - 50px);
        background:#F1F5F9; border-left:1px solid #E2E8F0;
        overflow-y:auto; z-index:900;
        padding:14px 12px;
        box-shadow:-2px 0 8px rgba(0,0,0,.06);
      }
      .rsb-logo { text-align:center; margin-bottom:12px; }
      .rsb-logo p { font-size:10px; color:#64748B; margin:3px 0 0; }
      .rsb-section {
        font-size:10px; font-weight:700; color:#94A3B8;
        text-transform:uppercase; letter-spacing:.06em;
        margin:12px 0 5px;
      }
      .nav-btn {
        display:flex; align-items:center; gap:8px;
        width:100%; padding:8px 10px; margin-bottom:3px;
        border:none; border-radius:6px; background:transparent;
        color:#334155; font-size:13px; cursor:pointer; text-align:left;
        transition:background .15s, color .15s;
      }
      .nav-btn:hover  { background:#DBEAFE; color:#1E40AF; }
      .nav-btn.active { background:#DBEAFE; color:#1E40AF; font-weight:600; }
      .rsb-label {
        font-size:12px; color:#475569;
        margin:6px 0 3px; font-weight:500;
      }
      #scraping_log {
        font-family:monospace; font-size:11px;
        max-height:140px; overflow-y:auto;
        background:#F0F9FF; border:1px solid #BAE6FD;
        border-radius:4px; padding:7px;
        color:#0C4A6E; white-space:pre-wrap;
      }
      .dataTables_wrapper { font-size:13px; }
      .subtitle-header {
        font-size:12px; color:#64748B;
        margin:-10px 0 16px; font-style:italic;
      }
    "))),

    tags$div(id = "rsb",
      tags$div(class = "rsb-logo",
        tags$img(
          src    = paste0("https://upload.wikimedia.org/wikipedia/commons/",
                          "thumb/0/08/Escudo_UNAL.svg/120px-Escudo_UNAL.svg.png"),
          height = "42px"
        ),
        tags$p("Juan Pablo Aldana Henao"),
        tags$p("Minería de Datos · 2016325")
      ),

      tags$div(class = "rsb-section", "Navegación"),
      tags$button(class = "nav-btn active", id = "nb_kpi",
        onclick = "navTo('tab_kpi')",  tags$span("📊"), "Indicadores"),
      tags$button(class = "nav-btn",        id = "nb_viz",
        onclick = "navTo('tab_viz')",  tags$span("📈"), "Visualizaciones"),
      tags$button(class = "nav-btn",        id = "nb_tabla",
        onclick = "navTo('tab_tabla')", tags$span("📋"), "Tabla de Papers"),
      tags$button(class = "nav-btn",        id = "nb_scrap",
        onclick = "navTo('tab_scrap')", tags$span("🔄"), "Actualizar DB"),

      tags$hr(style = "border-color:#CBD5E1; margin:10px 0;"),
      tags$div(class = "rsb-section", "Filtros globales"),

      uiOutput("ui_slider_anio"),

      tags$div(class = "rsb-label", "Categoría:"),
      pickerInput("filtro_cat", label = NULL,
        choices  = c("Todas", "Machine Learning", "IA Generativa",
                     "Estadística", "Otros"),
        selected = "Todas",
        options  = list(style = "btn-light btn-sm")),

      tags$div(class = "rsb-label", "Autor (contiene):"),
      textInput("filtro_autor", label = NULL, placeholder = "ej. Smith"),

      tags$div(class = "rsb-label", "DOI (contiene):"),
      textInput("filtro_doi",   label = NULL, placeholder = "ej. 10.1146"),

      tags$div(class = "rsb-label", "Título / Palabras clave:"),
      textInput("filtro_kw",    label = NULL, placeholder = "ej. neural"),

      actionButton("btn_filtrar", "Aplicar filtros", icon = icon("filter"),
        class = "btn-primary btn-sm btn-block",
        style = "margin-top:8px; background:#2563EB; border-color:#1E40AF;")
    ),

    tags$script(HTML("
      var NAV_IDS = {
        'tab_kpi'  : 'nb_kpi',
        'tab_viz'  : 'nb_viz',
        'tab_tabla': 'nb_tabla',
        'tab_scrap': 'nb_scrap'
      };
      function navTo(tab) {
        Object.values(NAV_IDS).forEach(function(id) {
          document.getElementById(id).classList.remove('active');
        });
        document.getElementById(NAV_IDS[tab]).classList.add('active');
        Shiny.setInputValue('active_tab', tab, {priority: 'event'});
      }
    ")),

    tabsetPanel(id = "main_tabs", type = "hidden",

      tabPanelBody("tab_kpi",
        h3("Indicadores descriptivos",
           style = "color:#1E40AF; margin-bottom:4px;"),
        tags$p("Annual Reviews of Economics", class = "subtitle-header"),

        fluidRow(
          valueBoxOutput("kpi_total",   width = 3),
          valueBoxOutput("kpi_citas",   width = 3),
          valueBoxOutput("kpi_autores", width = 3),
          valueBoxOutput("kpi_refs",    width = 3)
        ),
        fluidRow(
          valueBoxOutput("kpi_cats",    width = 4),
          valueBoxOutput("kpi_top_cit", width = 4),
          valueBoxOutput("kpi_top_dl",  width = 4)
        ),
        fluidRow(
          box(title = "Distribución por categoría",
              width = 6, status = "primary",
              withSpinner(highchartOutput("hc_cat_pie",   height = "300px"),
                          color = "#2563EB")),
          box(title = "Top 10 artículos más citados",
              width = 6, status = "primary",
              withSpinner(highchartOutput("hc_top_citas", height = "300px"),
                          color = "#2563EB"))
        )
      ),

      tabPanelBody("tab_viz",
        h3("Visualizaciones interactivas",
           style = "color:#1E40AF; margin-bottom:4px;"),
        tags$p("Annual Reviews of Economics", class = "subtitle-header"),

        fluidRow(
          box(title = "Evolución temporal de publicaciones",
              width = 12, status = "primary",
              withSpinner(highchartOutput("hc_evolucion", height = "320px"),
                          color = "#2563EB"))
        ),
        fluidRow(
          box(title = "Top 15 autores con más publicaciones",
              width = 6, status = "info",
              withSpinner(highchartOutput("hc_autores",    height = "320px"),
                          color = "#2563EB")),
          box(title = "Distribución de citas (histograma)",
              width = 6, status = "info",
              withSpinner(highchartOutput("hc_hist_citas", height = "320px"),
                          color = "#2563EB"))
        ),
        fluidRow(
          box(title = "Descargas por temática",
              width = 6, status = "warning",
              withSpinner(highchartOutput("hc_dl_cat", height = "280px"),
                          color = "#2563EB")),
          box(title = "Citas vs Descargas (burbuja)",
              width = 6, status = "warning",
              withSpinner(highchartOutput("hc_bubble",  height = "280px"),
                          color = "#2563EB"))
        )
      ),

      tabPanelBody("tab_tabla",
        h3("Tabla de artículos filtrados",
           style = "color:#1E40AF; margin-bottom:16px;"),
        fluidRow(
          box(width = 12, status = "primary",
            tags$div(
              style = "margin-bottom:10px; display:flex; gap:10px; align-items:center;",
              downloadButton("btn_csv", "Descargar CSV",
                class = "btn-sm",
                style = "background:#16A34A; color:#fff; border:none;"),
              tags$span(
                style = "color:#64748B; font-size:13px;",
                textOutput("n_papers_label", inline = TRUE))
            ),
            withSpinner(DTOutput("tabla_papers"), color = "#2563EB"))
        )
      ),

      tabPanelBody("tab_scrap",
        h3("Actualización de datos — Scraping + Crossref",
           style = "color:#1E40AF; margin-bottom:4px;"),
        tags$p("Annual Reviews Economics 2025-2026. Citas obtenidas via API de Crossref.",
               class = "subtitle-header"),

        fluidRow(
          box(title = "Control de scraping", width = 6, status = "primary",
            pickerInput("scrap_anio", "Año(s) a scrapear:",
              choices  = names(URLS_SCRAPING),
              selected = names(URLS_SCRAPING),
              multiple = TRUE,
              options  = list(`actions-box` = TRUE,
                              `selected-text-format` = "count > 2",
                              style = "btn-light btn-sm")),

            checkboxInput("scrap_crossref",
              "Consultar Crossref para citas reales", value = TRUE),

            tags$div(
              style = "display:flex; gap:8px; flex-wrap:wrap; margin-top:10px;",
              actionButton("btn_scrape", "Buscar artículos",
                class = "btn-primary",
                style = "background:#2563EB; border-color:#1E40AF;"),
              actionButton("btn_verificar", "Verificar últimos 5",
                class = "btn-default")
            ),

            tags$hr(style = "border-color:#E2E8F0; margin:12px 0;"),
            tags$p(tags$b("Log:"),
                   style = "color:#475569; margin-bottom:4px; font-size:13px;"),
            verbatimTextOutput("scraping_log")
          ),

          box(title = "Resultados", width = 6, status = "success",
            uiOutput("scraping_resultado"),
            withSpinner(DTOutput("tabla_nuevos"), color = "#16A34A"))
        )
      )
    ) 
  )  
) 
