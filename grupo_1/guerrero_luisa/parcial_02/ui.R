# ---------------------------------------------------------
# UI
# ---------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  div(
    class = "app-wrapper",
    
    # ---------------------------------------------------
    # HERO
    # ---------------------------------------------------
    
    div(
      class = "editorial-hero",
      div(
        class = "hero-content",
        div(class = "eyebrow", "MINERÍA DE DATOS · TALLER 2 · 2016325"),
        h1(
          class = "hero-title",
          tags$em("BDCC"), " Research", tags$br(), "Dashboard"
        ),
        div(class = "hero-divider"),
        p(
          class = "hero-subtitle",
          "Exploración interactiva de artículos científicos de la revista
          Big Data and Cognitive Computing. Consulta, visualiza y actualiza
          la base de datos SQLite construida mediante web scraping."
        ),
        div(
          class = "topline",
          span(class = "pill", "SQLite"),
          span(class = "pill", "Shiny R"),
          span(class = "pill", "Web Scraping"),
          span(class = "pill", "CrossRef API"),
          span(class = "pill", "KDD"),
          span(class = "pill", "Highcharter")
        )
      )
    ),
    
    # ---------------------------------------------------
    # INTRO STRIP
    # ---------------------------------------------------
    
    div(
      class = "intro-strip",
      div(
        class = "intro-strip-left",
        div(class = "intro-strip-label", "Sobre el proyecto"),
        div(
          class = "intro-strip-text",
          "Dashboard analítico para explorar publicaciones
          académicas de ML, IA Generativa y Estadística."
        )
      ),
      div(
        class = "intro-strip-right",
        div(class = "intro-strip-label", "Base de datos"),
        div(
          class = "intro-strip-text",
          textOutput("inicio_resumen_base")
        )
      )
    ),
    
    # ---------------------------------------------------
    # FILTROS GLOBALES
    # ---------------------------------------------------
    
    h2(class = "section-title", "Filtros globales"),
    p(
      class = "section-note",
      "Seleccione los criterios de búsqueda. Los indicadores, gráficas y tabla
      se actualizan al presionar 'Aplicar filtros'."
    ),
    
    div(
      class = "filter-card",
      
      div(
        class = "filter-header",
        div(
          h3(class = "filter-title", "Parámetros de consulta"),
          p(
            class = "filter-subtitle",
            "Rango de fechas, tema, autor, DOI o palabras clave."
          )
        ),
        div(
          class = "filter-actions-top",
          actionButton(
            "aplicar_filtros",
            "Aplicar filtros",
            class = "btn-editorial btn-filter-main"
          ),
          actionButton(
            "limpiar",
            "Limpiar",
            class = "btn-editorial btn-filter-secondary"
          )
        )
      ),
      
      fluidRow(
        column(
          3,
          dateRangeInput(
            "fecha",
            "Rango de fechas",
            start = min(papers_base$publication_date, na.rm = TRUE),
            end = max(papers_base$publication_date, na.rm = TRUE),
            min = min(papers_base$publication_date, na.rm = TRUE),
            max = max(papers_base$publication_date, na.rm = TRUE),
            language = "es",
            separator = " → "
          )
        ),
        
        column(
          3,
          selectInput(
            "tema",
            "Tema o categoría",
            choices = c("Todos", sort(unique(papers_base$topic_label))),
            selected = "Todos"
          )
        ),
        
        column(
          3,
          tags$div(
            tags$label("Autor", `for` = "autor"),
            selectizeInput(
              "autor",
              label = NULL,
              choices = c(
                "Todos" = "Todos",
                setNames(autores_disponibles, autores_disponibles)
              ),
              selected = "Todos",
              multiple = FALSE,
              options = list(
                placeholder = "Buscar autor…",
                maxOptions = length(autores_disponibles) + 1,
                openOnFocus = TRUE,
                closeAfterSelect = TRUE,
                dropdownParent = "body"
              )
            )
          )
        ),
        
        column(
          3,
          textInput(
            "doi",
            "DOI",
            placeholder = "Buscar por DOI…"
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          textInput(
            "palabra",
            "Título o palabra clave",
            placeholder = "Ej: machine learning, neural network…"
          )
        )
      )
    ),
    
    # ---------------------------------------------------
    # SECCIONES
    # ---------------------------------------------------
    
    tabsetPanel(
      id = "secciones",
      type = "tabs",
      
      # -----------------------------------------------
      # INICIO
      # -----------------------------------------------
      
      tabPanel(
        "Inicio",
        
        h2(class = "section-title", "Resumen del proyecto"),
        
        fluidRow(
          column(
            8,
            div(
              class = "panel-card",
              h3("Dashboard orientado al proceso KDD"),
              p(
                "El objetivo de esta aplicación es consultar, visualizar y actualizar
                una base de datos SQLite construida en el Taller 1 mediante web scraping
                sobre la revista Big Data and Cognitive Computing (BDCC)."
              ),
              p(
                "La aplicación integra filtros dinámicos, indicadores descriptivos,
                visualizaciones interactivas, tabla de consulta y actualización
                automática de nuevos artículos vía CrossRef API."
              )
            )
          ),
          
          column(
            4,
            div(
              class = "article-highlight",
              div(class = "metric-label", "Base de datos activa"),
              div(class = "article-highlight-title", "SQLite · BDCC"),
              div(
                class = "article-highlight-meta",
                textOutput("inicio_tabla_activa")
              )
            )
          )
        ),
        
        div(
          class = "image-banner-card",
          tags$img(
            src = "imagen_principal.png",
            alt = "Imagen principal del proyecto"
          )
        ),
        
        fluidRow(
          column(
            3,
            div(
              class = "metric-card",
              div(class = "metric-label", "Total artículos"),
              div(class = "metric-value", textOutput("inicio_total")),
              div(class = "metric-foot", "Base completa")
            )
          ),
          
          column(
            3,
            div(
              class = "metric-card",
              div(class = "metric-label", "Categorías"),
              div(class = "metric-value", textOutput("inicio_categorias")),
              div(class = "metric-foot", "Clasificación temática")
            )
          ),
          
          column(
            3,
            div(
              class = "metric-card",
              div(class = "metric-label", "Año inicial"),
              div(class = "metric-value", textOutput("inicio_anio_inicial")),
              div(class = "metric-foot", "Primer registro")
            )
          ),
          
          column(
            3,
            div(
              class = "metric-card",
              div(class = "metric-label", "Año final"),
              div(class = "metric-value", textOutput("inicio_anio_final")),
              div(class = "metric-foot", "Último registro")
            )
          )
        )
      ),
      
      # -----------------------------------------------
      # ESTADO DE LA BASE
      # -----------------------------------------------
      
      tabPanel(
        "Estado de la base",
        
        h2(class = "section-title", "Estado de la base"),
        p(
          class = "section-note",
          "Resumen técnico de la base SQLite después del proceso de scraping y actualización automática."
        ),
        
        fluidRow(
          column(
            3,
            div(
              class = "metric-card",
              div(class = "metric-label", "Total artículos"),
              div(class = "metric-value", textOutput("estado_total")),
              div(class = "metric-foot", "Registros en SQLite")
            )
          ),
          
          column(
            3,
            div(
              class = "metric-card",
              div(class = "metric-label", "DOI únicos"),
              div(class = "metric-value", textOutput("estado_doi_unicos")),
              div(class = "metric-foot", "Control de duplicados")
            )
          ),
          
          column(
            3,
            div(
              class = "metric-card",
              div(class = "metric-label", "DOI duplicados"),
              div(class = "metric-value", textOutput("estado_doi_duplicados")),
              div(class = "metric-foot", "Validación de calidad")
            )
          ),
          
          column(
            3,
            div(
              class = "metric-card",
              div(class = "metric-label", "Última fecha"),
              div(class = "metric-value", textOutput("estado_fecha_max")),
              div(class = "metric-foot", "Artículo más reciente")
            )
          )
        ),
        
        fluidRow(
          column(
            6,
            div(
              class = "panel-card",
              highchartOutput("grafico_estado_anios", height = "380px")
            )
          ),
          
          column(
            6,
            div(
              class = "panel-card",
              highchartOutput("grafico_estado_temas", height = "380px")
            )
          )
        ),
        
        div(
          class = "panel-card",
          h3("Resumen de calidad"),
          DTOutput("tabla_estado_base")
        )
      ),
      
      # -----------------------------------------------
      # INDICADORES
      # -----------------------------------------------
      
      tabPanel(
        "Indicadores",
        
        h2(class = "section-title", "Indicadores descriptivos"),
        p(
          class = "section-note",
          "Los valores se recalculan al aplicar los filtros seleccionados."
        ),
        
        fluidRow(
          column(
            3,
            div(
              class = "metric-card",
              div(class = "metric-label", "Total artículos"),
              div(class = "metric-value", textOutput("total_articulos")),
              div(class = "metric-foot", "Registros filtrados")
            )
          ),
          
          column(
            3,
            div(
              class = "metric-card",
              div(class = "metric-label", "Promedio autores"),
              div(class = "metric-value", textOutput("prom_autores")),
              div(class = "metric-foot", "Por artículo")
            )
          ),
          
          column(
            3,
            div(
              class = "metric-card",
              div(class = "metric-label", "Promedio citas"),
              div(class = "metric-value", textOutput("prom_citas")),
              div(class = "metric-foot", "Impacto promedio")
            )
          ),
          
          column(
            3,
            div(
              class = "metric-card",
              div(class = "metric-label", "Promedio referencias"),
              div(class = "metric-value", textOutput("prom_referencias")),
              div(class = "metric-foot", "Referencias por paper")
            )
          )
        ),
        
        fluidRow(
          column(
            6,
            div(
              class = "article-highlight",
              div(class = "metric-label", "Artículo más citado"),
              div(class = "article-highlight-title", textOutput("articulo_mas_citado")),
              div(class = "article-highlight-meta", textOutput("meta_mas_citado"))
            )
          ),
          
          column(
            6,
            div(
              class = "article-highlight",
              div(class = "metric-label", "Categoría con más artículos"),
              div(class = "article-highlight-title", textOutput("categoria_mayor")),
              div(class = "article-highlight-meta", textOutput("meta_categoria_mayor"))
            )
          )
        )
      ),
      
      # -----------------------------------------------
      # VISUALIZACIONES
      # -----------------------------------------------
      
      tabPanel(
        "Visualizaciones",
        
        h2(class = "section-title", "Visualizaciones interactivas"),
        p(
          class = "section-note",
          "Comparaciones, distribuciones y relaciones entre categorías, fechas, autores, citas y referencias de los artículos filtrados."
        ),
        
        fluidRow(
          column(
            6,
            div(
              class = "panel-card",
              highchartOutput("grafico_temas", height = "380px")
            )
          ),
          
          column(
            6,
            div(
              class = "panel-card",
              highchartOutput("grafico_tiempo", height = "380px")
            )
          )
        ),
        
        fluidRow(
          column(
            6,
            div(
              class = "panel-card",
              highchartOutput("grafico_top_autores", height = "430px")
            )
          ),
          
          column(
            6,
            div(
              class = "panel-card",
              highchartOutput("grafico_citas_tema", height = "430px")
            )
          )
        ),
        
        fluidRow(
          column(
            6,
            div(
              class = "panel-card",
              highchartOutput("grafico_referencias_tema", height = "410px")
            )
          ),
          
          column(
            6,
            div(
              class = "panel-card",
              highchartOutput("grafico_autores_tema", height = "410px")
            )
          )
        ),
        
        fluidRow(
          column(
            6,
            div(
              class = "panel-card",
              highchartOutput("grafico_scatter_citas_refs", height = "410px")
            )
          ),
          
          column(
            6,
            div(
              class = "panel-card",
              highchartOutput("grafico_boxplot_citas", height = "410px")
            )
          )
        ),
        
        fluidRow(
          column(
            12,
            div(
              class = "panel-card",
              highchartOutput("grafico_heatmap_mes_tema", height = "430px")
            )
          )
        ),
        
        fluidRow(
          column(
            12,
            div(
              class = "panel-card",
              highchartOutput("grafico_citas", height = "360px")
            )
          )
        )
      ),
      
      # -----------------------------------------------
      # TABLA
      # -----------------------------------------------
      
      tabPanel(
        "Tabla de artículos",
        
        h2(class = "section-title", "Archivo dinámico de artículos"),
        p(
          class = "section-note",
          "Tabla filtrada con título, autores, fecha, categoría, DOI, citas y referencias."
        ),
        
        downloadButton(
          outputId = "descargar_filtrados",
          label = "Descargar datos filtrados",
          class = "btn-editorial btn-filter-secondary"
        ),
        
        tags$br(),
        tags$br(),
        
        div(
          class = "panel-card",
          DTOutput("tabla_articulos")
        )
      ),
      
      # -----------------------------------------------
      # METODOLOGÍA
      # -----------------------------------------------
      
      tabPanel(
        "Metodología",
        
        h2(class = "section-title", "Metodología"),
        p(
          class = "section-note",
          "Esta sección resume el proceso desarrollado para construir, consultar, visualizar y actualizar la base de datos de artículos científicos de la revista Big Data and Cognitive Computing."
        ),
        
        div(
          class = "methodology-grid",
          
          div(
            class = "method-card",
            div(class = "method-number", "01"),
            h3("Fuente de datos"),
            p(
              "Se trabajó con artículos científicos de la revista Big Data and Cognitive Computing, tomando como referencia los registros recolectados inicialmente en el Taller 1."
            )
          ),
          
          div(
            class = "method-card",
            div(class = "method-number", "02"),
            h3("Extracción de información"),
            p(
              "La base inicial fue construida mediante web scraping. En esta versión de la aplicación, la actualización se realiza consultando CrossRef API para recuperar artículos recientes."
            )
          ),
          
          div(
            class = "method-card",
            div(class = "method-number", "03"),
            h3("Almacenamiento en SQLite"),
            p(
              "Los artículos se almacenan en una base de datos SQLite. Cada registro incluye información como título, DOI, autores, fecha de publicación, citas, referencias y categoría temática."
            )
          ),
          
          div(
            class = "method-card",
            div(class = "method-number", "04"),
            h3("Limpieza y preparación"),
            p(
              "Los datos se transforman para asegurar tipos adecuados, fechas válidas, valores numéricos y campos textuales consistentes antes de ser usados en filtros, indicadores y gráficas."
            )
          ),
          
          div(
            class = "method-card",
            div(class = "method-number", "05"),
            h3("Clasificación temática"),
            p(
              "Los artículos se organizan en categorías como Machine Learning, IA Generativa, Estadística u Otros, lo que permite analizar la distribución temática de la producción científica."
            )
          ),
          
          div(
            class = "method-card",
            div(class = "method-number", "06"),
            h3("Dashboard interactivo"),
            p(
              "La aplicación Shiny permite filtrar artículos por fecha, tema, autor, DOI y palabra clave. Los indicadores, visualizaciones y tablas se actualizan según los filtros aplicados."
            )
          ),
          
          div(
            class = "method-card",
            div(class = "method-number", "07"),
            h3("Actualización automática"),
            p(
              "El usuario puede buscar nuevos artículos desde la aplicación. Los DOI encontrados se comparan contra los existentes para evitar registros duplicados antes de insertar nuevos datos."
            )
          ),
          
          div(
            class = "method-card",
            div(class = "method-number", "08"),
            h3("Análisis y exportación"),
            p(
              "Los resultados se presentan mediante indicadores, visualizaciones interactivas, tablas dinámicas y un botón para descargar los artículos filtrados en formato CSV."
            )
          )
        ),
        
        div(
          class = "methodology-summary",
          h3("Relación con el proceso KDD"),
          p(
            "El proyecto sigue una lógica cercana al proceso KDD: selección de la fuente, recolección de datos, limpieza y transformación, almacenamiento estructurado, exploración visual, análisis descriptivo y actualización de la información para mantener la base vigente."
          )
        )
      ),
      
      # -----------------------------------------------
      # ACTUALIZACIÓN
      # -----------------------------------------------
      
      tabPanel(
        "Actualización SQLite",
        
        h2(class = "section-title", "Actualización mediante scraping"),
        p(
          class = "section-note",
          "Consulta CrossRef API para buscar artículos nuevos, los compara con los DOI ya almacenados y los inserta automáticamente en SQLite si corresponde."
        ),
        
        fluidRow(
          column(
            4,
            div(
              class = "filter-card",
              h3(class = "filter-title", "Control"),
              p(
                class = "filter-subtitle",
                "Seleccione el año y ejecute el proceso de actualización."
              ),
              
              selectInput(
                "anio_scraping",
                "Año a consultar",
                choices = c(2026, 2025),
                selected = 2026
              ),
              
              actionButton(
                "actualizar",
                "Buscar nuevos artículos",
                class = "btn-editorial btn-filter-main"
              ),
              
              tags$br(),
              tags$br(),
              
              actionButton(
                "verificar_ultimos",
                "Verificar últimos 5",
                class = "btn-editorial btn-filter-secondary"
              )
            )
          ),
          
          column(
            8,
            div(
              class = "panel-card",
              h3("Resultado del proceso"),
              verbatimTextOutput("estado_actualizacion"),
              tags$br(),
              DTOutput("tabla_actualizacion")
            )
          )
        )
      )
    ),
    
    div(
      class = "footer-note",
      "BDCC Research Dashboard · Taller 2 · Minería de Datos · Universidad Nacional"
    )
  )
)