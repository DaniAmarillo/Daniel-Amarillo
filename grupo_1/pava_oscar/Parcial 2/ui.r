# ui.R - Interfaz Gráfica (Frontend Avanzado)

fluidPage(
  title = "Frontiers Plant Science Hub",
  
  # Estilos CSS inyectados para un acabado moderno de Dashboard de Minería
  tags$head(tags$style(HTML("
    body { background-color: #f4f7f6; font-family: 'Segoe UI', sans-serif; }
    .main-header { background: linear-gradient(135deg, #1b4332 0%, #2d6a4f 100%); color: white; padding: 18px 25px; border-radius: 0 0 12px 12px; margin-bottom: 25px; box-shadow: 0 4px 10px rgba(0,0,0,0.08); }
    .main-header h1 { margin: 0; font-size: 24px; font-weight: 700; }
    .sidebar-panel { background: white; padding: 20px; border-radius: 12px; box-shadow: 0 4px 12px rgba(0,0,0,0.05); border: 1px solid #e2ebd9; }
    .kpi-card { background: white; border-radius: 8px; padding: 15px; text-align: center; box-shadow: 0 2px 6px rgba(0,0,0,0.04); border-top: 4px solid #40916c; }
    .kpi-num { font-size: 24px; font-weight: 700; color: #1b4332; }
    .kpi-title { font-size: 11px; text-transform: uppercase; color: #666; font-weight: 600; margin-top: 4px; }
    .nav-tabs { font-weight: 600; margin-top: 15px; }
    .nav-tabs > li.active > a { color: #1b4332 !important; border-bottom: 3px solid #2d6a4f !important; }
    .section-box { background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 5px rgba(0,0,0,0.02); margin-bottom: 20px; }
  "))),
  
  # Encabezado
  div(class = "main-header",
      fluidRow(
        column(8, h1("\U0001f331 Frontiers in Plant Science — KDD Dashboard")),
        column(4, align = "right", span(class="label label-success", style="padding:6px 10px; font-size:11px;", "SQLite Local: Conectado"))
      )
  ),
  
  fluidRow(
    # Sidebar de Control de Consultas Dinámicas
    column(3,
           div(class = "sidebar-panel",
               h4(style="font-weight:700; color:#1b4332; margin-top:0;", "Filtros e Indexación"),
               hr(style="margin-top:5px; margin-bottom:15px;"),
               
               dateRangeInput("fechas", "Rango de Publicación:",
                              start = "2025-01-01", end = "2026-12-31",
                              format = "yyyy-mm-dd", language = "es"),
               
               selectInput("tema", "Filtrar por Tema (NLP):",
                           choices = c("Todos", "Machine Learning", "IA Generativa", "Estadística", "Otros")),
               
               br(),
               h5(style="font-weight:600; color:#2d6a4f; margin-bottom:5px;", "Búsqueda por Patrones de Texto"),
               textInput("titulo", "Keywords en Título:", placeholder = "Ej. CRISPR, RNA, Yield"),
               textInput("autor", "Nombre de Autor:", placeholder = "Ej. Zhang"),
               textInput("doi_fil", "Código DOI:", placeholder = "Ej. 10.3389"),
               
               hr(),
               h4(style="font-weight:700; color:#c1121f;", "\U0001f552 Extractor Incrustado"),
               p(style="font-size:11px; color:#666;", "Lanza el web scraper automatizado para recopilar papers nuevos del año 2026."),
               actionButton("btn_scrape", "\U0001f50d Buscar e Inyectar Nuevos Datos", 
                            class = "btn btn-block btn-success", 
                            style = "background-color:#2d6a4f; border:none; font-weight:600; padding:10px;"),
               br(),
               uiOutput("scrape_msg")
           )
    ),
    
    # Panel Principal de Visualizaciones Complejas
    column(9,
           # Grid de KPIs
           fluidRow(
             column(2, div(class="kpi-card", div(class="kpi-num", textOutput("kpi_total")), div(class="kpi-title", "Artículos"))),
             column(2, div(class="kpi-card", div(class="kpi-num", textOutput("kpi_vistas")), div(class="kpi-title", "Vistas Totales"))),
             column(2, div(class="kpi-card", div(class="kpi-num", textOutput("kpi_citas")), div(class="kpi-title", "Prom. Citas"))),
             column(2, div(class="kpi-card", div(class="kpi-num", textOutput("kpi_autores")), div(class="kpi-title", "Autores / Art"))),
             column(2, div(class="kpi-card", div(class="kpi-num", textOutput("kpi_refs")), div(class="kpi-title", "Referencias Med"))),
             column(2, div(class="kpi-card", div(class="kpi-num", textOutput("kpi_temas")), div(class="kpi-title", "Categorías")))
           ),
           br(),
           
           # Navegación por pestañas
           tabsetPanel(
             id = "tabs",
             
             tabPanel("\U0001f4ca Frecuencias y Volúmenes",
                      br(),
                      fluidRow(
                        column(7, div(class="section-box", highchartOutput("chart_mes", height="380px"))),
                        column(5, div(class="section-box", highchartOutput("chart_tema", height="380px")))
                      )
             ),
             
             tabPanel("\U0001f4c8 Impacto: Citas vs Descargas",
                      br(),
                      fluidRow(
                        column(7, div(class="section-box", highchartOutput("chart_scatter", height="400px"))),
                        column(5, div(class="section-box", highchartOutput("chart_top_autores", height="400px")))
                      )
             ),
             
             tabPanel("\U0001f4dd Explorador Estructurado SQL",
                      br(),
                      div(class="section-box",
                          h4(style="font-weight:600; color:#1b4332; margin-top:0;", "Registros Extraídos de la Base de Datos"),
                          DTOutput("tabla")
                      )
             )
           )
    )
  )
)