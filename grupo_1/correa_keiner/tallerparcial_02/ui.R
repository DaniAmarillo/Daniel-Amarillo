source("global.R")

#Paleta y estilos
custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&family=Outfit:wght@500;600;700;800&display=swap');

:root {
  /* Nueva paleta: Elegante, limpia y profesional (Slate Dark Mode) */
  --bg:        #0f172a; /* Azul pizarra muy oscuro */
  --panel:     #1e293b; /* Pizarra oscuro para las tarjetas */
  --border:    #334155; /* Gris pizarra para bordes */
  --accent:    #3b82f6; /* Azul profesional */
  --accent2:   #6366f1; /* Índigo */
  --text:      #f8fafc; /* Blanco hueso para mejor lectura */
  --muted:     #94a3b8; /* Gris claro para texto secundario */
  --danger:    #ef4444; /* Rojo suave para errores */
}

* { box-sizing: border-box; }

body {
  background: var(--bg) !important;
  color: var(--text) !important;
  font-family: 'Inter', sans-serif !important;
  margin: 0;
}

/* ── Sidebar ── */
.sidebar-panel, .well {
  background: var(--panel) !important;
  border: 1px solid var(--border) !important;
  border-radius: 12px !important;
  padding: 20px !important;
  box-shadow: none !important;
}

.sidebar-heading {
  font-family: 'Outfit', sans-serif;
  font-size: 12px;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--accent);
  margin-bottom: 16px;
  border-bottom: 1px solid var(--border);
  padding-bottom: 8px;
  font-weight: 600;
}

/* ── Labels y controles ── */
label, .control-label {
  color: var(--muted) !important;
  font-size: 11px !important;
  letter-spacing: 0.05em !important;
  text-transform: uppercase !important;
  font-family: 'Outfit', sans-serif !important;
  font-weight: 500 !important;
}

.form-control, .selectize-input {
  background: var(--bg) !important;
  border: 1px solid var(--border) !important;
  color: var(--text) !important;
  border-radius: 6px !important;
  font-size: 13px !important;
  font-family: 'Inter', sans-serif !important;
}

.selectize-dropdown {
  background: var(--panel) !important;
  border: 1px solid var(--border) !important;
  color: var(--text) !important;
}

.selectize-dropdown-content .option:hover,
.selectize-dropdown-content .option.active {
  background: var(--accent2) !important;
  color: white !important;
}

/* ── Botones ── */
.btn-scrape {
  background: linear-gradient(135deg, var(--accent), #2563eb) !important;
  color: #ffffff !important;
  border: none !important;
  border-radius: 8px !important;
  font-family: 'Outfit', sans-serif !important;
  font-size: 13px !important;
  font-weight: 600 !important;
  letter-spacing: 0.02em !important;
  padding: 10px 18px !important;
  width: 100% !important;
  transition: opacity 0.2s !important;
}
.btn-scrape:hover { opacity: 0.85 !important; }

/* ── Tabs ── */
.nav-tabs { border-bottom: 1px solid var(--border) !important; }
.nav-tabs > li > a {
  color: var(--muted) !important;
  background: transparent !important;
  border: none !important;
  font-family: 'Outfit', sans-serif !important;
  font-size: 13px !important;
  letter-spacing: 0.05em !important;
  padding: 10px 18px !important;
  text-transform: uppercase !important;
  font-weight: 600 !important;
  border-bottom: 2px solid transparent !important;
  transition: all 0.2s !important;
}
.nav-tabs > li.active > a,
.nav-tabs > li > a:hover {
  color: var(--accent) !important;
  border-bottom: 2px solid var(--accent) !important;
  background: transparent !important;
}
.tab-content { padding-top: 20px !important; }

/* ── KPI Cards ── */
.kpi-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
  gap: 12px;
  margin-bottom: 24px;
}
.kpi-card {
  background: var(--panel);
  border: 1px solid var(--border);
  border-radius: 10px;
  padding: 18px 16px;
  position: relative;
  overflow: hidden;
}
.kpi-card::before {
  content: '';
  position: absolute;
  top: 0; left: 0; right: 0;
  height: 3px;
}
.kpi-card.c1::before { background: var(--accent); }
.kpi-card.c2::before { background: var(--accent2); }
.kpi-card.c3::before { background: #f59e0b; }
.kpi-card.c4::before { background: #ef4444; }
.kpi-card.c5::before { background: #06b6d4; }
.kpi-card.c6::before { background: #ec4899; }

.kpi-label {
  font-family: 'Outfit', sans-serif;
  font-size: 11px;
  letter-spacing: 0.05em;
  text-transform: uppercase;
  color: var(--muted);
  margin-bottom: 8px;
  font-weight: 500;
}
.kpi-value {
  font-family: 'Outfit', sans-serif;
  font-size: 28px;
  font-weight: 700;
  color: var(--text);
  line-height: 1;
}
.kpi-sub {
  font-size: 12px;
  color: var(--muted);
  margin-top: 4px;
  font-family: 'Inter', sans-serif;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

/* ── Título header ── */
.app-header {
  background: var(--panel);
  border-bottom: 1px solid var(--border);
  padding: 20px 28px;
  margin-bottom: 0;
  display: flex;
  align-items: center;
  gap: 16px;
}
.app-title {
  font-family: 'Outfit', sans-serif;
  font-size: 20px;
  font-weight: 700;
  color: var(--text);
  letter-spacing: -0.01em;
}
.app-title span { color: var(--accent); }
.app-badge {
  font-family: 'Outfit', sans-serif;
  font-size: 10px;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  font-weight: 600;
  color: #ffffff;
  background: var(--accent);
  padding: 4px 8px;
  border-radius: 4px;
}

/* ── Notificación scraping ── */
.scrape-box {
  background: var(--panel);
  border: 1px solid var(--accent);
  border-radius: 8px;
  padding: 14px 16px;
  margin-top: 12px;
  font-family: 'Inter', sans-serif;
  font-size: 13px;
  color: var(--accent);
}
.scrape-box.warn { border-color: var(--danger); color: var(--danger); }

/* ── Tabla DT ── */
.dataTables_wrapper {
  color: var(--text) !important;
  font-size: 13px !important;
  font-family: 'Inter', sans-serif !important;
}
table.dataTable thead th {
  background: var(--bg) !important;
  color: var(--muted) !important;
  border-bottom: 1px solid var(--border) !important;
  font-family: 'Outfit', sans-serif !important;
  font-size: 12px !important;
  letter-spacing: 0.05em !important;
  text-transform: uppercase !important;
  font-weight: 600 !important;
}
table.dataTable tbody tr {
  background: var(--panel) !important;
  border-bottom: 1px solid var(--border) !important;
}
table.dataTable tbody tr:hover td {
  background: #263348 !important; /* Ligeramente más claro que el panel */
}
.dataTables_filter input,
.dataTables_length select {
  background: var(--bg) !important;
  color: var(--text) !important;
  border: 1px solid var(--border) !important;
  border-radius: 4px !important;
}
.dataTables_info, .dataTables_paginate { color: var(--muted) !important; }
.paginate_button { color: var(--muted) !important; }
.paginate_button.current { background: var(--accent2) !important; color: white !important; border-radius: 4px !important; border: none !important;}

/* ── Highcharts override ── */
.highcharts-background { fill: var(--panel) !important; }


/* ── Ajuste para el Calendario (Datepicker) en Modo Oscuro ── */

/* Contenedor principal del desplegable */
.datepicker.datepicker-dropdown.dropdown-menu {
  background-color: var(--panel) !important;
  border: 1px solid var(--border) !important;
  color: var(--text) !important;
  padding: 12px !important;
  border-radius: 8px !important;
}

/* Encabezado: Nombre del mes y flechas (« Enero 2025 ») */
.datepicker-dropdown th.datepicker-switch,
.datepicker-dropdown th.prev,
.datepicker-dropdown th.next {
  color: var(--text) !important;
  font-family: 'Outfit', sans-serif !important;
  font-weight: 600 !important;
}

/* Efecto hover en las flechas y nombre del mes */
.datepicker-dropdown th.prev:hover,
.datepicker-dropdown th.next:hover,
.datepicker-dropdown th.datepicker-switch:hover {
  background: rgba(255, 255, 255, 0.08) !important;
}

/* Días de la semana (Do, Lu, Ma, Mi...) */
.datepicker table tr th.dow {
  color: var(--muted) !important;
  font-family: 'Outfit', sans-serif !important;
  font-size: 11px !important;
  text-transform: uppercase !important;
  font-weight: 500 !important;
  border-bottom: 1px solid var(--border) !important;
  padding-bottom: 6px !important;
}

/* Todos los días del mes */
.datepicker table tr td.day {
  color: var(--text) !important;
  background: transparent !important;
  border-radius: 6px !important;
  font-family: 'Inter', sans-serif !important;
  font-size: 13px !important;
}

/* Días que pertenecen al mes anterior o siguiente (atenuados) */
.datepicker table tr td.old,
.datepicker table tr td.new {
  color: var(--muted) !important;
  opacity: 0.4 !important;
}

/* Efecto hover al pasar el cursor sobre cualquier día disponible */
.datepicker table tr td.day:hover,
.datepicker table tr td.focused {
  background: rgba(255, 255, 255, 0.08) !important;
  color: var(--text) !important;
}

/* El día seleccionado como inicio y fin del rango */
.datepicker table tr td.active, 
.datepicker table tr td.active:hover,
.datepicker table tr td.active.focused {
  background: var(--accent) !important;
  color: #ffffff !important;
  font-weight: 600 !important;
  box-shadow: none !important;
}

/* Los días que quedan dentro del rango seleccionado (entre inicio y fin) */
.datepicker table tr td.range,
.datepicker table tr td.range:hover {
  background: rgba(59, 130, 246, 0.15) !important; /* Azul del acento con transparencia */
  color: var(--text) !important;
  border-radius: 0 !important; /* Mantiene el bloque del rango continuo */
}
"

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$style(HTML(custom_css)),
    tags$title("PLOS ONE ·  Buscador de arítculos")
  ),
  
  # Header
  div(class = "app-header",
      div(class = "app-title", "PLOS", tags$span(" ONE"), " · Buscador de arítculos")
  ),
  
  br(),
  
  sidebarLayout(
    # ── Sidebar ──────────────────────────────────────────────────────────────
    sidebarPanel(
      width = 3,
      
      div(style = "display:flex; align-items:center; justify-content:space-between;",
          div(class = "sidebar-heading", style = "margin-bottom:0; border-bottom:none;", "⚙ Filtros"),
          actionButton("btn_reset", "🗑️ limpiar",class = "btn-reset")
                      
          
      ),
      div(style = "border-bottom: 1px solid var(--border); margin-bottom: 16px; margin-top: 8px;"),
      
      # Rango de fechas
      dateRangeInput("fecha_rango",
                     label  = "Rango de fechas",
                     start  = "2025-01-01",
                     end    = Sys.Date(),
                     format = "yyyy-mm-dd",
                     language = "es"
      ),
      
      # Tema
      selectInput("topic_sel",
                  label   = "Temática",
                  choices = c("Todos", "Machine Learning", "IA Generativa", "Estadística", "Otros"),
                  selected = "Todos"
      ),
      
      # Autor
      textInput("autor_busq",
                label       = "Autor",
                placeholder = "Ej: Smith, J."
      ),
      
      # DOI
      textInput("doi_busq",
                label       = "DOI",
                placeholder = "Ej: 10.1371/journal..."
      ),
      
      # Palabra clave
      textInput("keyword_busq",
                label       = "Palabra clave / título",
                placeholder = "Ej: neural network"
      ),
      
      # Botón aplicar filtros (Ajustado el gradiente a los nuevos colores)
      br(),
      actionButton("btn_filtrar", "Aplicar filtros",
                   class = "btn-scrape",
                   style = "background: linear-gradient(135deg, var(--accent2), #4f46e5) !important;"
      ),
      
      hr(style = "border-color: var(--border); margin: 20px 0;"),
      
      div(class = "sidebar-heading", "⟳ Actualización"),
      
      actionButton("btn_scrape", "🔍 Buscar artículos nuevos", class = "btn-scrape"),
      
      uiOutput("scrape_msg")
    ),
    
    # ── Main panel ───────────────────────────────────────────────────────────
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "tabs",
        
        # ── Tab 1: Indicadores ──────────────────────────────────────────────
        tabPanel("📊 Indicadores",
                 br(),
                 uiOutput("kpi_cards"),
                 br(),
                 fluidRow(
                   column(6, highchartOutput("chart_temporal", height = "300px")),
                   column(6, highchartOutput("chart_topics",   height = "300px"))
                 ),
                 br(),
                 fluidRow(
                   column(6, highchartOutput("chart_top_autores", height = "300px")),
                   column(6, highchartOutput("chart_citas_dist",  height = "300px"))
                 )
        ),
        
        # ── Tab 2: Tabla de artículos ───────────────────────────────────────
        tabPanel("📋 Artículos",
                 br(),
                 uiOutput("filtro_kpi_msg"),   
                 DTOutput("tabla_papers")
        ),
      
        
        # ── Tab 3: Artículos nuevos (scraping) ─────────────────────────────
        tabPanel("🆕 Nuevos artículos",
                 br(),
                 uiOutput("nuevos_header"),
                 DTOutput("tabla_nuevos")
        )
      )
    )
  )
)