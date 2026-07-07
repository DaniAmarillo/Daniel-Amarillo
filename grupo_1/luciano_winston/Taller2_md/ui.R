ui <- fluidPage(
  useShinyjs(),
  title = "Springer Visual Miner",
  
  # ============================================================
  # HEAD: FONTS + ICONS + GLOBAL CSS
  # ============================================================
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    

    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@300;400;500;600;700&family=IBM+Plex+Mono:wght@400;500&family=Syne:wght@600;700;800&display=swap"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css"),
    
    tags$style(HTML("
/* ============================================================
   DESIGN TOKENS — NEXUS DARK PALETTE
   ============================================================ */
:root {
  --bg-void:        #030712;
  --bg-deep:        #080d1a;
  --bg-base:        #0a101f;
  --bg-surface:     #0f172a;
  --bg-card:        rgba(15, 23, 42, 0.70);
  --bg-input:       rgba(8, 13, 26, 0.90);

  --border-subtle:  rgba(148, 163, 184, 0.07);
  --border-default: rgba(148, 163, 184, 0.12);
  --border-accent:  rgba(99, 102, 241, 0.30);

  --text-primary:   #f1f5f9;
  --text-secondary: #94a3b8;
  --text-muted:     #475569;

  --accent-primary:   #6366f1;   
  --accent-secondary: #22d3ee;   

  --glow-primary:   rgba(99, 102, 241, 0.35);

  --font-sans: 'IBM Plex Sans', 'Inter', sans-serif;
  --font-display: 'Syne', sans-serif;
  --font-mono: 'IBM Plex Mono', monospace;

  --radius-sm: 6px;
  --radius-md: 10px;
  --radius-lg: 16px;
  --radius-xl: 22px;

  --sidebar-w: 260px;
}

/* ============================================================
   RESET & BASE
   ============================================================ */
*, *::before, *::after { box-sizing: border-box; }

html, body {
  margin: 0; padding: 0;
  background: var(--bg-void);
  color: var(--text-primary);
  font-family: var(--font-sans);
  font-size: 14px;
  line-height: 1.6;
  min-height: 100vh;
  overflow-x: hidden;
}

/* Scrollbar premium */
::-webkit-scrollbar { width: 6px; height: 6px; }
::-webkit-scrollbar-track { background: transparent; }
::-webkit-scrollbar-thumb {
  background: rgba(99, 102, 241, 0.35);
  border-radius: 3px;
}
::-webkit-scrollbar-thumb:hover { background: rgba(99,102,241,0.6); }

/* ============================================================
   LAYOUT SHELL
   ============================================================ */
#app-shell { display: flex; min-height: 100vh; position: relative; }

/* ============================================================
   SIDEBAR (Con animación de ocultamiento)
   ============================================================ */
#sidebar {
  width: var(--sidebar-w);
  height: 100vh;
  background: var(--bg-deep);
  border-right: 1px solid var(--border-subtle);
  display: flex;
  flex-direction: column;
  position: fixed;
  top: 0; left: 0;
  z-index: 100;
  overflow-y: auto;
  overflow-x: hidden;
  transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
}

#sidebar::before {
  content: ''; position: absolute; top: 0; left: 0; right: 0; height: 2px;
  background: linear-gradient(90deg, var(--accent-primary), var(--accent-secondary), transparent);
}

.sidebar-logo {
  padding: 20px 20px 16px; border-bottom: 1px solid var(--border-subtle); display: flex; align-items: center; gap: 12px;
}

.sidebar-logo-icon {
  width: 36px; height: 36px;
  background: linear-gradient(135deg, var(--accent-primary), var(--accent-secondary));
  border-radius: var(--radius-md); display: flex; align-items: center; justify-content: center;
  font-size: 16px; color: white; box-shadow: 0 0 18px var(--glow-primary); flex-shrink: 0;
}

.sidebar-logo-text { flex: 1; }
.sidebar-logo-text h2 { font-family: var(--font-display); font-size: 14px; font-weight: 800; color: var(--text-primary); margin: 0; }
.sidebar-logo-text span { font-size: 10px; color: var(--text-muted); letter-spacing: 0.08em; text-transform: uppercase; font-weight: 500; }

.sidebar-status { margin: 12px 16px; padding: 8px 12px; background: rgba(255, 255, 255, 0.03); border: 1px solid var(--border-subtle); border-radius: var(--radius-md); font-size: 11px; }

.sidebar-accordion { margin: 0 10px 10px; background: rgba(255, 255, 255, 0.01); border: 1px solid var(--border-subtle); border-radius: var(--radius-sm); }
.accordion-trigger { padding: 12px 15px; color: var(--text-secondary); font-size: 11px; font-weight: 600; text-transform: uppercase; letter-spacing: 0.08em; cursor: pointer; display: flex; justify-content: space-between; align-items: center; list-style: none; }
.accordion-trigger::-webkit-details-marker { display: none; }
.accordion-trigger:hover { background: rgba(255, 255, 255, 0.04); color: var(--text-primary); }
.arrow-icon { font-size: 10px; transition: transform 0.2s ease; }
.sidebar-accordion[open] .arrow-icon { transform: rotate(180deg); }
.accordion-content { padding: 5px 10px 15px; }

.nav-item-btn {
  display: flex; align-items: center; gap: 12px; padding: 9px 12px; margin-bottom: 2px;
  border-radius: var(--radius-md); border: 1px solid transparent; background: transparent;
  color: var(--text-secondary); font-size: 13px; font-weight: 500; cursor: pointer; transition: all 0.18s ease;
  width: 100%; text-align: left;
}
.nav-item-btn:hover { background: rgba(99, 102, 241, 0.08); color: var(--text-primary); border-color: var(--border-accent); }
.nav-item-btn.active { background: rgba(99, 102, 241, 0.14); color: #a5b4fc; border-color: rgba(99, 102, 241, 0.30); }

.nav-item-btn .nav-icon {
  width: 30px; height: 30px; display: flex; align-items: center; justify-content: center;
  border-radius: var(--radius-sm); background: rgba(255,255,255,0.04); font-size: 12px; flex-shrink: 0;
}
.nav-item-btn.active .nav-icon { background: rgba(99,102,241,0.25); color: #a5b4fc; }

.sidebar-bottom { margin-top: auto; padding: 16px; border-top: 1px solid var(--border-subtle); }
.filter-group { margin-bottom: 14px; }
.filter-label { font-size: 11px; font-weight: 600; color: var(--text-muted); letter-spacing: 0.06em; text-transform: uppercase; display: block; margin-bottom: 5px; }

/* NUEVO: CLASES DE COLAPSADO DE SIDEBAR */
.sidebar-collapsed { transform: translateX(-100%); width: 0 !important; border: none !important; opacity: 0; }
.main-expanded { margin-left: 0 !important; width: 100% !important; }

#restore-sidebar-btn { position: fixed; bottom: 20px; left: 20px; z-index: 9999; background: var(--accent-primary); color: white; border: none; border-radius: 50%; width: 45px; height: 45px; box-shadow: 0 4px 15px var(--glow-primary); cursor: pointer; display: none; align-items: center; justify-content: center; transition: all 0.2s ease; }
#restore-sidebar-btn:hover { transform: scale(1.1); background: #4f46e5; }

/* ============================================================
   MAIN CONTENT AREA
   ============================================================ */
#main-content {
  margin-left: var(--sidebar-w); flex: 1; min-height: 100vh; display: flex; flex-direction: column;
  background: radial-gradient(ellipse 80% 40% at 60% -10%, rgba(99,102,241,0.10) 0%, transparent 60%),
              radial-gradient(ellipse 50% 30% at 95% 80%, rgba(34,211,238,0.06) 0%, transparent 50%), var(--bg-void);
  transition: margin-left 0.3s cubic-bezier(0.4, 0, 0.2, 1);
}

/* ============================================================
   TOPBAR
   ============================================================ */
#topbar {
  height: 58px; background: rgba(8, 13, 26, 0.75); backdrop-filter: blur(20px); -webkit-backdrop-filter: blur(20px);
  border-bottom: 1px solid var(--border-subtle); display: flex; align-items: center; padding: 0 28px; gap: 16px;
  position: sticky; top: 0; z-index: 90;
}
.topbar-breadcrumb { display: flex; align-items: center; gap: 8px; flex: 1; }
.topbar-breadcrumb .section-title { font-family: var(--font-display); font-size: 15px; font-weight: 700; color: var(--text-primary); }
.topbar-breadcrumb .section-sub { font-size: 12px; color: var(--text-muted); }
.topbar-actions { display: flex; align-items: center; gap: 10px; }
.topbar-badge { display: inline-flex; align-items: center; gap: 5px; padding: 4px 10px; background: rgba(16,185,129,0.08); border: 1px solid rgba(16,185,129,0.22); border-radius: 20px; font-size: 11px; color: #34d399; font-weight: 500; }
.topbar-badge .dot { width: 5px; height: 5px; border-radius: 50%; background: #10b981; box-shadow: 0 0 6px #10b981; animation: pulse-dot 2s infinite; }
@keyframes pulse-dot { 0%, 100% { opacity: 1; transform: scale(1); } 50% { opacity: 0.6; transform: scale(0.8); } }

/* ============================================================
   PAGE CONTENT & CARDS
   ============================================================ */
.page-content { padding: 24px 28px 40px; flex: 1; }
.page-header { margin-bottom: 24px; }
.page-header h1 { font-family: var(--font-display); font-size: 24px; font-weight: 800; color: var(--text-primary); margin: 0 0 4px; }
.page-header p { font-size: 13px; color: var(--text-muted); margin: 0; }

.kpi-grid { display: grid; grid-template-columns: repeat(4, 1fr); gap: 16px; margin-bottom: 24px; }
.kpi-card { background: var(--bg-card); border: 1px solid var(--border-default); border-radius: var(--radius-xl); padding: 20px 22px; position: relative; overflow: hidden; transition: transform 0.2s ease, box-shadow 0.2s ease; cursor: default; }
.kpi-card:hover { transform: translateY(-2px); box-shadow: 0 20px 40px -12px rgba(0,0,0,0.5); }
.kpi-card::before { content: ''; position: absolute; top: 0; left: 0; right: 0; height: 1px; background: linear-gradient(90deg, transparent, var(--card-accent, var(--accent-primary)), transparent); opacity: 0.6; }
.kpi-card.kpi-blue   { --card-accent: #6366f1; } .kpi-card.kpi-cyan   { --card-accent: #22d3ee; }
.kpi-card.kpi-purple { --card-accent: #a78bfa; } .kpi-card.kpi-emerald{ --card-accent: #10b981; }

.kpi-header { display: flex; align-items: center; justify-content: space-between; margin-bottom: 14px; }
.kpi-icon { width: 34px; height: 34px; border-radius: var(--radius-md); display: flex; align-items: center; justify-content: center; background: rgba(255,255,255,0.05); color: var(--card-accent, var(--accent-primary)); }
.kpi-value { font-family: var(--font-display); font-size: 30px; font-weight: 800; line-height: 1; margin-bottom: 4px; }
.kpi-label { font-size: 12px; color: var(--text-secondary); }

.glass-card { background: var(--bg-card); border: 1px solid var(--border-default); border-radius: var(--radius-xl); overflow: hidden; transition: box-shadow 0.2s ease; }
.glass-card:hover { box-shadow: 0 16px 40px -10px rgba(0,0,0,0.4); }
.card-header { padding: 16px 20px 14px; border-bottom: 1px solid var(--border-subtle); display: flex; align-items: center; gap: 10px; }
.card-header-icon { width: 28px; height: 28px; border-radius: var(--radius-sm); background: rgba(99,102,241,0.15); display: flex; align-items: center; justify-content: center; font-size: 11px; color: var(--accent-primary); }
.card-header h3 { font-size: 13px; font-weight: 600; margin: 0; }
.card-header .card-sub { font-size: 11px; color: var(--text-muted); margin-left: auto; }
.card-body { padding: 18px 20px; }

.chart-row { display: grid; gap: 16px; margin-bottom: 16px; }
.chart-row-7-5 { grid-template-columns: 7fr 5fr; } .chart-row-6-6 { grid-template-columns: 1fr 1fr; } .chart-row-4-4-4 { grid-template-columns: 1fr 1fr 1fr; }

/* DT Table overrides */
table.dataTable { color: var(--text-secondary) !important; font-size: 13px !important; }
table.dataTable thead th { background: rgba(8,13,26,0.8) !important; color: var(--text-muted) !important; font-size: 11px !important; border-bottom: 1px solid var(--border-subtle) !important; }
table.dataTable tbody tr:hover td { background: rgba(99,102,241,0.06) !important; }
table.dataTable tbody td { border-top: 1px solid var(--border-subtle) !important; }
.dataTables_wrapper .dataTables_filter input { background: var(--bg-input) !important; color: var(--text-primary) !important; border: 1px solid var(--border-default) !important; border-radius: var(--radius-md) !important; padding: 4px 8px !important; }

/* Article Details & Insights */
.detail-header { border-bottom: 1px solid var(--border-subtle); padding-bottom: 15px; }
.kpi-mini-card { background: rgba(255,255,255,0.03); border: 1px solid var(--border-subtle); border-radius: var(--radius-lg); padding: 14px 16px; text-align: center; }
.kpi-mini-val { font-family: var(--font-display); font-size: 22px; font-weight: 800; color: var(--text-primary); }
.kpi-mini-lbl { font-size: 11px; color: var(--text-muted); text-transform: uppercase; }
.abstract-box { font-size: 13px; color: var(--text-secondary); padding: 16px; background: rgba(255,255,255,0.02); border-left: 2px solid var(--accent-primary); border-radius: 0 var(--radius-md) var(--radius-md) 0; }
.meta-pill { display: inline-flex; align-items: center; gap: 5px; padding: 4px 10px; border-radius: 20px; font-size: 11px; background: rgba(148,163,184,0.08); border: 1px solid var(--border-default); color: var(--text-secondary); }

.insights-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 14px; margin-bottom: 20px; }
.insight-card { background: var(--bg-card); border: 1px solid var(--border-default); border-radius: var(--radius-xl); padding: 18px 20px; }
.ic-badge { display: inline-flex; align-items: center; padding: 3px 8px; border-radius: 12px; font-size: 10px; font-weight: 700; text-transform: uppercase; margin-bottom: 10px; }
.ic-badge.rising { background: rgba(16,185,129,0.12); color: #34d399; } .ic-badge.dominant { background: rgba(99,102,241,0.12); color: #a5b4fc; } .ic-badge.trending { background: rgba(245,158,11,0.12); color: #fbbf24; }
.insight-card h4 { font-size: 14px; font-weight: 600; margin: 0 0 6px; } .insight-card p { font-size: 12px; color: var(--text-muted); margin: 0; }

.ranking-list { list-style: none; padding: 0; margin: 0; }
.ranking-item { display: flex; align-items: center; gap: 12px; padding: 10px 0; border-bottom: 1px solid var(--border-subtle); }
.ranking-pos { font-family: var(--font-mono); font-size: 11px; font-weight: 600; color: var(--text-muted); width: 20px; text-align: right; }
.ranking-text { flex: 1; } .ranking-text .r-name { font-size: 12px; font-weight: 500; display: block; } .ranking-val { font-family: var(--font-mono); font-size: 12px; font-weight: 600; }

.btn-primary-nexus, .btn-danger-nexus { display: inline-flex; align-items: center; gap: 7px; padding: 8px 16px; border-radius: var(--radius-md); font-size: 12px; font-weight: 600; cursor: pointer; width: 100%; justify-content: center; border: 1px solid transparent; }
.btn-primary-nexus { background: linear-gradient(135deg, var(--accent-primary), #4f46e5); color: white; }
.btn-danger-nexus { background: rgba(244,63,94,0.12); color: #fb7185; border-color: rgba(244,63,94,0.3); }

.form-control, .selectize-input, input, select { background: var(--bg-input) !important; color: var(--text-primary) !important; border: 1px solid var(--border-default) !important; border-radius: var(--radius-md) !important; font-size: 12px !important; padding: 7px 10px !important; }
label, .control-label { color: var(--text-muted) !important; font-size: 11px !important; text-transform: uppercase !important; margin-bottom: 5px !important; }

.admin-danger-zone { background: rgba(244,63,94,0.04); border: 1px solid rgba(244,63,94,0.14); border-radius: var(--radius-lg); padding: 16px; }
.system-tag { display: inline-flex; align-items: center; gap: 4px; font-family: var(--font-mono); font-size: 10px; color: var(--text-muted); background: rgba(255,255,255,0.03); border: 1px solid var(--border-subtle); border-radius: var(--radius-sm); padding: 2px 6px; }

.page-section { animation: pageFadeIn 0.25s ease; }
@keyframes pageFadeIn { from { opacity: 0; transform: translateY(6px); } to { opacity: 1; transform: translateY(0); } }
    "))
  ),
  
  # ============================================================
  # APP SHELL
  # ============================================================
  div(id = "app-shell",
      

      tags$nav(id = "sidebar",
               div(class = "sidebar-logo",
        
                   tags$img(src = "UNAL.png", style = "width: 45px; height: 45px; object-fit: contain; border-radius: 6px;"),
                   div(class = "sidebar-logo-text", tags$h2("Visual Miner"), tags$span("MINERIA DE DATOS"))
               ),
               div(class = "sidebar-status", uiOutput("status_scraping")),
               
               # SECCIÓN 1: NAVIGATION (Desplegable)
               tags$details(open = TRUE, class = "sidebar-accordion",
                            tags$summary(class = "accordion-trigger", 
                                         tags$span(tags$i(class = "fa-solid fa-compass", style="margin-right:8px; color: var(--accent-secondary);"), "Navigation"),
                                         tags$i(class = "fa-solid fa-chevron-down arrow-icon")
                            ),
                            div(class = "accordion-content",
                                actionButton("nav_dashboard", class = "nav-item-btn active", onclick = "switchPage('dashboard')", label = tagList(tags$span(class = "nav-icon", tags$i(class = "fa-solid fa-chart-pie")), "Dashboard")),
                                actionButton("nav_analytics", class = "nav-item-btn", onclick = "switchPage('analytics')", label = tagList(tags$span(class = "nav-icon", tags$i(class = "fa-solid fa-chart-line")), "Analytics")),
                                actionButton("nav_literature", class = "nav-item-btn", onclick = "switchPage('literature')", label = tagList(tags$span(class = "nav-icon", tags$i(class = "fa-solid fa-book-open")), "Literature Explorer")),
                                actionButton("nav_authors", class = "nav-item-btn", onclick = "switchPage('authors')", label = tagList(tags$span(class = "nav-icon", tags$i(class = "fa-solid fa-users")), "Authors Intelligence")),
                                actionButton("nav_insights", class = "nav-item-btn", onclick = "switchPage('insights')", label = tagList(tags$span(class = "nav-icon", tags$i(class = "fa-solid fa-brain")), "Research Insights")),
                                actionButton("nav_admin", class = "nav-item-btn", onclick = "switchPage('admin')", label = tagList(tags$span(class = "nav-icon", tags$i(class = "fa-solid fa-sliders")), "Administration"))
                            )
               ),
               
               # SECCIÓN 2: FILTERS (Desplegable)
               tags$details(open = TRUE, class = "sidebar-accordion",
                            tags$summary(class = "accordion-trigger", 
                                         tags$span(tags$i(class = "fa-solid fa-filter", style="margin-right:8px; color: var(--accent-secondary);"), "Filters"),
                                         tags$i(class = "fa-solid fa-chevron-down arrow-icon")
                            ),
                            div(class = "accordion-content",
                                div(class = "filter-group", tags$span(class = "filter-label", "Período"), dateRangeInput("filtro_fecha", NULL, start = "2025-01-01", end = Sys.Date(), language = "es", width = "100%")),
                                div(class = "filter-group", tags$span(class = "filter-label", "Categoría"), selectInput("filtro_tema", NULL, choices = c("Todos" = "Todos"), selected = "Todos", width = "100%")),
                                div(class = "filter-group", tags$span(class = "filter-label", "Investigador"), selectizeInput("filtro_autor", NULL, choices = c("Todos" = "Todos"), selected = "Todos", width = "100%")),
                                div(class = "filter-group", tags$span(class = "filter-label", "Palabra clave"), textInput("busqueda_global", NULL, placeholder = "Buscar texto...", width = "100%")),
                                
                    
                                tags$button(id = "toggle-sidebar-btn", class = "btn", 
                                            style = "width: 100%; margin-top: 10px; background: rgba(255,255,255,0.05); color: rgba(255,255,255,0.6); border: 1px solid rgba(255,255,255,0.1); border-radius: var(--radius-sm); padding: 8px; font-size: 11px; cursor: pointer; transition: 0.2s;",
                                            tags$i(class = "fa-solid fa-arrows-left-right", style="margin-right:6px;"), "Ocultar Sidebar")
                            )
               ),
               
               div(class = "sidebar-bottom", div(class = "system-tag", tags$i(class = "fa-solid fa-circle", style = "color:#10b981; font-size:6px"), "Springer Link · AIR Journal"))
      ),
      
      # BOTÓN FLOTANTE PARA RESTAURAR EL SIDEBAR
      tags$button(id = "restore-sidebar-btn", tags$i(class = "fa-solid fa-bars", style = "font-size: 18px;")),
      
      # MAIN CONTENT
      div(id = "main-content",
          div(id = "topbar",
              div(class = "topbar-breadcrumb", uiOutput("topbar_title")),
              div(class = "topbar-actions", uiOutput("status_badge"), tags$span(class = "system-tag", uiOutput("topbar_count")))
          ),
          
          # PAGE 1: DASHBOARD
          div(id = "page-dashboard", class = "page-content page-section",
              div(class = "page-header", tags$h1("Dashboard"), tags$p("Vista consolidada del corpus analizado")),
              div(class = "kpi-grid", uiOutput("kpi_articulos"), uiOutput("kpi_autores"), uiOutput("kpi_citas"), uiOutput("kpi_descargas")),
              div(class = "chart-row chart-row-7-5",
                  div(class = "glass-card", div(class = "card-header", div(class="card-header-icon", tags$i(class="fa-solid fa-wave-square")), tags$h3("Evolución de Publicaciones")), div(class = "card-body", highchartOutput("plot_tendencia", height = "300px"))),
                  div(class = "glass-card", div(class = "card-header", div(class="card-header-icon", tags$i(class="fa-solid fa-chart-pie")), tags$h3("Distribución Temática")), div(class = "card-body", highchartOutput("plot_categorias", height = "300px")))
              ),
              div(class = "chart-row chart-row-6-6",
                  div(class = "glass-card", div(class = "card-header", div(class="card-header-icon", tags$i(class="fa-solid fa-circle-nodes")), tags$h3("Citas vs Descargas")), div(class = "card-body", highchartOutput("plot_bubble", height = "300px"))),
                  div(class = "glass-card", div(class = "card-header", div(class="card-header-icon", tags$i(class="fa-solid fa-bars-progress")), tags$h3("Impacto por Autor")), div(class = "card-body", highchartOutput("plot_pareto", height = "300px")))
              )
          ),
          
          # PAGE 2: ANALYTICS
          div(id = "page-analytics", class = "page-content page-section", style = "display:none",
              div(class = "page-header", tags$h1("Analytics"), tags$p("Métricas avanzadas y rankings")),
              div(class = "chart-row chart-row-6-6",
                  div(class = "glass-card", div(class = "card-header", tags$h3("Evolución de Citas")), div(class = "card-body", highchartOutput("plot_citas_anio", height = "300px"))),
                  div(class = "glass-card", div(class = "card-header", tags$h3("Evolución de Descargas")), div(class = "card-body", highchartOutput("plot_downloads_tiempo", height = "300px")))
              ),
              div(class = "chart-row chart-row-6-6",
                  div(class = "glass-card", div(class = "card-header", tags$h3("Heatmap")), div(class = "card-body", highchartOutput("plot_heatmap", height = "300px"))),
                  div(class = "glass-card", div(class = "card-header", tags$h3("Pie Chart")), div(class = "card-body", highchartOutput("plot_treemap", height = "300px")))
              ),
              div(class = "chart-row chart-row-6-6",
                  div(class = "glass-card", div(class = "card-header", tags$h3("Top 10 Papers (Citas)")), div(class = "card-body", highchartOutput("plot_top_papers_citas", height = "340px"))),
                  div(class = "glass-card", div(class = "card-header", tags$h3("Top 10 Papers (Descargas)")), div(class = "card-body", highchartOutput("plot_top_papers_downloads", height = "340px")))
              )
          ),
          
          
          # PAGE 3: LITERATURE EXPLORER
          div(id = "page-literature", class = "page-content page-section", style = "display:none",
              div(class = "page-header", tags$h1("Literature Explorer"), tags$p("Corpus indexado detallado")),
              
              div(class = "glass-card",
                  div(class = "card-header", 
                      div(class="card-header-icon", tags$i(class="fa-solid fa-database")), 
                      tags$h3("Corpus Indexado"), 
                      tags$span(class="card-sub", uiOutput("lit_count_label"))
                  ),
                  
                  div(class = "card-body",
                      DTOutput("tabla_articulos")
                  )
              ),
              uiOutput("article_detail")
          ),
          
          # PAGE 4: AUTHORS
          div(id = "page-authors", class = "page-content page-section", style = "display:none",
              div(class = "page-header", tags$h1("Authors Intelligence"), tags$p("Autores y productividad")),
              div(class = "chart-row chart-row-4-4-4", uiOutput("author_kpi_total"), uiOutput("author_kpi_top"), uiOutput("author_kpi_prolific")),
              div(class = "chart-row chart-row-6-6",
                  div(class = "glass-card", div(class = "card-header", tags$h3("Top 15 Autores")), div(class = "card-body", highchartOutput("plot_top_autores", height = "380px"))),
                  div(class = "glass-card", div(class = "card-header", tags$h3("Ranking Detallado")), div(class = "card-body", uiOutput("ranking_autores_html")))
              )
          ),
          
          # PAGE 5: INSIGHTS
          div(id = "page-insights", class = "page-content page-section", style = "display:none",
              div(class = "page-header", tags$h1("Research Insights"), tags$p("Descubrimiento automático de patrones")),
              uiOutput("insights_cards"),
              div(class = "chart-row chart-row-6-6",
                  div(class = "glass-card", div(class = "card-header", tags$h3("Papers Mayor Impacto")), div(class = "card-body", uiOutput("top_impact_papers_html"))),
                  div(class = "glass-card", div(class = "card-header", tags$h3("Distribución Citas/Descargas")), div(class = "card-body", highchartOutput("plot_comparativa_temas", height = "340px")))
              )
          ),
          
          # PAGE 6: ADMINISTRATION
          div(id = "page-admin", class = "page-content page-section", style = "display:none",
              div(class = "page-header", tags$h1("Administration"), tags$p("Control operacional")),
              div(class = "chart-row chart-row-6-6",
                  
                  # 1. Panel Operaciones
                  div(class = "glass-card",
                      div(class = "card-header", tags$h3("Panel de Operaciones")),
                      div(class = "card-body",
                          actionButton("btn_actualizar", label = tagList(tags$i(class="fa-solid fa-rotate"), " Sincronización"), class = "btn-primary-nexus", style="margin-bottom:24px;"),
                          div(id = "admin-status-box", style = "background: rgba(255,255,255,0.02); border: 1px solid var(--border-subtle); border-radius: var(--radius-lg); padding: 14px;", uiOutput("admin_status_detail"))
                      )
                  ), 
                  
                  # 2. Zona de Seguridad (Modificada con Advertencia y Bloqueo)
                  div(class = "glass-card",
                      div(class = "card-header", tags$h3(style = "color: #fb7185;", "Zona de Seguridad")),
                      div(class = "card-body",
                          
                          # Cuadro de Advertencia
                          div(style = "background: rgba(251,113,133,0.06); border: 1px solid rgba(251,113,133,0.25); border-radius: var(--radius-sm); padding: 12px; margin-bottom: 20px;",
                              div(style = "color: #fb7185; font-weight: 700; font-size: 12px; margin-bottom: 4px;",
                                  tags$i(class = "fa-solid fa-triangle-exclamation"), " ADVERTENCIA CRÍTICA"
                              ),
                              div(style = "color: var(--text-secondary); font-size: 11px;",
                                  "Esta acción purgará la base de datos de forma ", tags$b("IRREVERSIBLE", style="color:#fb7185;"), " e iniciará un scraping intensivo."
                              )
                          ),
                          
                          # Input y Botón
                          div(class = "admin-danger-zone",
                              passwordInput("superuser_key", "PIN Superusuario", placeholder = "Ingrese contraseña", width = "100%"),
                              
                              # Botón envuelto en shinyjs::disabled
                              shinyjs::disabled(
                                actionButton("btn_emergencia", label = tagList(tags$i(class="fa-solid fa-radiation"), " Reconstruir DB"), 
                                             class = "btn-danger-nexus",
                                             style = "width: 100%; opacity: 0.5; cursor: not-allowed;")
                              )
                          )
                      )
                  )
              ), 
              
              # 3. Estadísticas
              div(class = "glass-card", style = "margin-top: 0;",
                  div(class = "card-header", tags$h3("Estadísticas Base de Datos")),
                  div(class = "card-body", div(class = "kpi-grid", uiOutput("db_stat_papers"), uiOutput("db_stat_authors"), uiOutput("db_stat_refs"), uiOutput("db_stat_topics")))
              )
          )
      )
  ),
  
  # ============================================================
  # JAVASCRIPT — FIX DE RESIZE Y DATATABLES
  # ============================================================
  tags$script(HTML("
    var pages = ['dashboard', 'analytics', 'literature', 'authors', 'insights', 'admin'];
    function switchPage(page) {
      pages.forEach(function(p) {
        var el = document.getElementById('page-' + p);
        if (el) el.style.display = 'none';
      });
      
      var target = document.getElementById('page-' + page);
      if (target) { target.style.display = 'block'; target.className = 'page-content page-section'; }
      
      var navBtns = document.querySelectorAll('.nav-item-btn');
      navBtns.forEach(function(btn) { btn.classList.remove('active'); if (btn.id === 'nav_' + page) btn.classList.add('active'); });
      

      setTimeout(function() { 
        window.dispatchEvent(new Event('resize')); 
        if ($.fn.dataTable) {
          $($.fn.dataTable.tables(true)).DataTable().columns.adjust();
        }
      }, 200);
    }

    /* --- NUEVO: Sistema de Ocultamiento de la Barra Lateral --- */
    $(document).on('click', '#toggle-sidebar-btn', function() {
      $('#sidebar').addClass('sidebar-collapsed');
      $('#main-content').addClass('main-expanded');
      $('#restore-sidebar-btn').css('display', 'flex');
      
      setTimeout(function() { 
        window.dispatchEvent(new Event('resize')); 
        if ($.fn.dataTable) { $($.fn.dataTable.tables(true)).DataTable().columns.adjust(); }
      }, 350); 
    });

    $(document).on('click', '#restore-sidebar-btn', function() {
      $('#sidebar').removeClass('sidebar-collapsed');
      $('#main-content').removeClass('main-expanded');
      $('#restore-sidebar-btn').css('display', 'none');
      
      setTimeout(function() { 
        window.dispatchEvent(new Event('resize')); 
        if ($.fn.dataTable) { $($.fn.dataTable.tables(true)).DataTable().columns.adjust(); }
      }, 350);
    });

/* ---- Click en fila DT (DESACTIVADO PARA EVITAR CONFLICTO) ---- 
    $(document).on('click', '#tabla_articulos tbody tr', function() {
      var table = $('#tabla_articulos').DataTable();
      var idx   = table.row(this).index();
      Shiny.setInputValue('selected_row', idx + 1, {priority: 'event'});
    });
    ----------------------------------------------------------------- */
  "))
)