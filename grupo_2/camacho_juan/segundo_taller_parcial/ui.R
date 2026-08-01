# ==================================================
# UI.R
# Taller 2 - Minería de Datos
# Interfaz del dashboard
# ==================================================

custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&family=Montserrat:wght@500;600;700;800&display=swap');

:root {
  --bg: #fafafa;
  --panel: #ffffff;
  --accent: #ff6b00;
  --accent2: #2563eb;
  --text: #1f2937;
  --muted: #6b7280;
  --border: #e5e7eb;
  --danger: #ef4444;
}

body {
  background: var(--bg) !important;
  color: var(--text) !important;
  font-family: 'Inter', sans-serif !important;
}

.sidebar-panel, .well {
  background: var(--panel) !important;
  border: 1px solid var(--border) !important;
  border-radius: 16px !important;
  padding: 22px !important;
  box-shadow: 0 8px 24px rgba(15, 23, 42, 0.06);
}

.sidebar-heading {
  font-family: 'Montserrat', sans-serif;
  font-size: 12px;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--accent);
  margin-bottom: 16px;
  border-bottom: 1px solid var(--border);
  padding-bottom: 8px;
  font-weight: 700;
}

label, .control-label {
  color: var(--muted) !important;
  font-size: 11px !important;
  letter-spacing: 0.05em !important;
  text-transform: uppercase !important;
  font-family: 'Montserrat', sans-serif !important;
  font-weight: 700 !important;
}

.form-control, .selectize-input {
  background: #ffffff !important;
  border: 1px solid var(--border) !important;
  color: var(--text) !important;
  border-radius: 8px !important;
  font-size: 13px !important;
}

.form-control:focus, .selectize-input.focus {
  border-color: var(--accent) !important;
  box-shadow: 0 0 0 3px rgba(255, 107, 0, 0.12) !important;
}

.btn-scrape {
  background: linear-gradient(135deg, var(--accent), #ff8a3d) !important;
  color: #ffffff !important;
  border: none !important;
  border-radius: 10px !important;
  font-family: 'Montserrat', sans-serif !important;
  font-size: 13px !important;
  font-weight: 700 !important;
  padding: 11px 18px !important;
  width: 100% !important;
  box-shadow: 0 8px 18px rgba(255, 107, 0, 0.22);
}

.btn-reset {
  background: #ffffff !important;
  color: var(--muted) !important;
  border: 1px solid var(--border) !important;
  border-radius: 8px !important;
  font-size: 11px !important;
  font-family: 'Montserrat', sans-serif !important;
  font-weight: 700 !important;
}

.app-header {
  background: linear-gradient(135deg, #ffffff, #fff7ed);
  border: 1px solid var(--border);
  border-left: 6px solid var(--accent);
  border-radius: 18px;
  padding: 24px 30px;
  box-shadow: 0 10px 30px rgba(15, 23, 42, 0.06);
}

.app-title {
  font-family: 'Montserrat', sans-serif;
  font-size: 30px;
  font-weight: 800;
  color: var(--text);
}

.app-title span {
  color: var(--accent);
}

.app-subtitle {
  color: var(--muted);
  font-size: 13px;
  margin-top: 6px;
  font-family: 'Inter', sans-serif;
}

.kpi-grid {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 14px;
}

.kpi-card {
  background: var(--panel);
  border: 1px solid var(--border);
  border-radius: 16px;
  padding: 18px;
  box-shadow: 0 8px 24px rgba(15, 23, 42, 0.06);
}

.kpi-card:hover {
  transform: translateY(-2px);
  transition: 0.2s ease;
  border-color: rgba(255, 107, 0, 0.45);
}

.kpi-label {
  color: var(--muted);
  font-size: 11px;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  font-family: 'Montserrat', sans-serif;
  font-weight: 700;
}

.kpi-value {
  color: var(--text);
  font-size: 27px;
  font-weight: 800;
  margin-top: 8px;
  font-family: 'Montserrat', sans-serif;
}

.kpi-sub {
  color: var(--muted);
  font-size: 12px;
  margin-top: 6px;
}

.chart-card {
  background: #ffffff;
  border: 1px solid var(--border);
  border-radius: 16px;
  padding: 12px;
  box-shadow: 0 8px 24px rgba(15, 23, 42, 0.06);
}

.scrape-box {
  background: #ecfdf5;
  color: #166534;
  border: 1px solid #bbf7d0;
  border-radius: 12px;
  padding: 12px;
  margin-top: 14px;
  white-space: pre-line;
  font-size: 13px;
}

.scrape-box.warn {
  background: #fff7ed;
  color: #9a3412;
  border-color: #fed7aa;
}

.dataTables_wrapper {
  color: var(--text) !important;
  font-family: 'Inter', sans-serif !important;
}

table.dataTable {
  background: var(--panel) !important;
  color: var(--text) !important;
  border: 1px solid var(--border) !important;
}

table.dataTable thead th {
  background: #fff7ed !important;
  color: var(--text) !important;
  border-bottom: 1px solid var(--border) !important;
}

table.dataTable tbody td {
  border-color: var(--border) !important;
}

.nav-tabs {
  border-bottom: 1px solid var(--border) !important;
}

.nav-tabs > li > a {
  color: var(--muted) !important;
  font-family: 'Montserrat', sans-serif !important;
  font-weight: 700 !important;
  border-radius: 10px 10px 0 0 !important;
}

.nav-tabs > li.active > a,
.nav-tabs > li.active > a:focus,
.nav-tabs > li.active > a:hover {
  color: var(--accent) !important;
  background: #ffffff !important;
  border: 1px solid var(--border) !important;
  border-bottom-color: transparent !important;
}
"

ui <- fluidPage(
  tags$head(
    tags$style(HTML(custom_css)),
    tags$title("Frontiers Big Data · Taller 2")
  ),
  
  div(
    class = "app-header",
    div(class = "app-title", "Análisis bibliométrico · ", tags$span("Frontiers in Big Data")),
    div(
      class = "app-subtitle",
      "Exploración, visualización y actualización de artículos científicos almacenados en SQLite"
    )
  ),
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      div(
        style = "display:flex; align-items:center; justify-content:space-between;",
        div(class = "sidebar-heading", style = "margin-bottom:0; border-bottom:none;", "Filtros"),
        actionButton("btn_reset", "limpiar", class = "btn-reset")
      ),
      
      div(style = "border-bottom: 1px solid var(--border); margin-bottom: 16px; margin-top: 8px;"),
      
      dateRangeInput(
        "fecha_rango",
        label = "Rango de fechas",
        start = "2025-01-01",
        end = Sys.Date(),
        format = "yyyy-mm-dd",
        language = "es"
      ),
      
      selectInput(
        "topic_sel",
        label = "Categoría",
        choices = get_topics(),
        selected = "Todos"
      ),
      
      textInput(
        "autor_busq",
        label = "Autor",
        placeholder = "Ej: Zhang"
      ),
      
      textInput(
        "doi_busq",
        label = "DOI",
        placeholder = "Ej: 10.3389/..."
      ),
      
      textInput(
        "keyword_busq",
        label = "Búsqueda por título o palabra",
        placeholder = "Ej: machine learning"
      ),
      
      br(),
      
      actionButton(
        "btn_filtrar",
        "Aplicar filtros",
        class = "btn-scrape",
        style = "background: linear-gradient(135deg, var(--accent2), #60a5fa) !important;"
      ),
      
      hr(style = "border-color: var(--border); margin: 20px 0;"),
      
      div(class = "sidebar-heading", "Actualización"),
      
      actionButton("btn_scrape", "Actualizar base de datos", class = "btn-scrape"),
      
      uiOutput("scrape_msg")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "tabs",
        
        tabPanel(
          "Resumen analítico",
          br(),
          uiOutput("kpi_cards"),
          br(),
          
          fluidRow(
            column(
              6,
              div(class = "chart-card", highchartOutput("chart_temporal", height = "310px"))
            ),
            column(
              6,
              div(class = "chart-card", highchartOutput("chart_topics", height = "310px"))
            )
          ),
          
          br(),
          
          fluidRow(
            column(
              6,
              div(class = "chart-card", highchartOutput("chart_descargas_categoria", height = "310px"))
            ),
            column(
              6,
              div(class = "chart-card", highchartOutput("chart_top_descargados", height = "310px"))
            )
          ),
          
          br(),
          
          fluidRow(
            column(
              6,
              div(class = "chart-card", highchartOutput("chart_relevancia", height = "370px"))
            ),
            column(
              6,
              div(class = "chart-card", highchartOutput("chart_top_citados", height = "370px"))
            )
          )
        ),
        
        tabPanel(
          "Explorador de artículos",
          br(),
          uiOutput("filtro_kpi_msg"),
          DTOutput("tabla_papers")
        ),
        
        tabPanel(
          "Actualización automática",
          br(),
          uiOutput("nuevos_header"),
          DTOutput("tabla_nuevos")
        )
      )
    )
  )
)