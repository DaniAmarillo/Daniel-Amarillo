# ---------------------------------------------------------
# UI
# ---------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(
      HTML(
        "
        .metric-value-search {
          font-size: clamp(24px, 2.2vw, 34px) !important;
          line-height: 1.08 !important;
          letter-spacing: -0.8px;
          overflow-wrap: break-word;
          word-break: normal;
          hyphens: none;
        }

        .metric-value-search .shiny-text-output {
          white-space: normal;
          overflow: visible;
          text-overflow: clip;
        }
        "
      )
    )
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
        div(class = "eyebrow", "MINERÍA DE DATOS · TALLER 4 · 2016325"),
        h1(
          class = "hero-title",
          tags$em("BDCC"), " Research", tags$br(), "Dashboard"
        ),
        div(class = "hero-divider"),
        p(
          class = "hero-subtitle",
          "Exploración interactiva de artículos científicos de la revista
          Big Data and Cognitive Computing. Consulta, visualiza y recupera
          información mediante TF-IDF, LSA y similitud coseno."
        ),
        div(
          class = "topline",
          span(class = "pill", "SQLite"),
          span(class = "pill", "Shiny R"),
          span(class = "pill", "Web Scraping"),
          span(class = "pill", "KDD"),
          span(class = "pill", "Highcharter"),
          span(class = "pill", "TF-IDF"),
          span(class = "pill", "LSA")
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
          "Dashboard analítico para explorar y recuperar publicaciones
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
                de nuevos artículos mediante web scraping."
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
      # Buscador inteligente
      
      tabPanel(
        "Buscador inteligente",
        
        h2(
          class = "section-title",
          "Recuperación inteligente de artículos"
        ),
        
        p(
          class = "section-note",
          "Escriba una consulta temática y seleccione TF-IDF, LSA o la comparación de ambas estrategias. Los resultados se ordenan según su similitud con la consulta."
        ),
        
        fluidRow(
          column(
            4,
            div(
              class = "filter-card",
              
              h3(
                class = "filter-title",
                "Parámetros de búsqueda"
              ),
              
              p(
                class = "filter-subtitle",
                "La consulta puede escribirse en lenguaje natural."
              ),
              
              textAreaInput(
                inputId = "consulta_buscador",
                label = "Consulta",
                placeholder = "Ejemplo: machine learning for disease diagnosis",
                rows = 4,
                width = "100%"
              ),
              
              selectInput(
                inputId = "estrategia_buscador",
                label = "Estrategia de recuperación",
                choices = c(
                  "TF-IDF y similitud coseno" = "TF-IDF",
                  "LSA con 200 componentes" = "LSA",
                  "Comparar ambas estrategias" = "Comparar"
                ),
                selected = "Comparar"
              ),
              
              selectInput(
                inputId = "n_resultados_buscador",
                label = "Cantidad de resultados",
                choices = c(5, 10, 20),
                selected = 5
              ),
              
              actionButton(
                inputId = "ejecutar_busqueda",
                label = "Buscar artículos",
                class = "btn-editorial btn-filter-main",
                width = "100%"
              ),
              
              tags$br(),
              tags$br(),
              
              actionButton(
                inputId = "limpiar_busqueda",
                label = "Limpiar búsqueda",
                class = "btn-editorial btn-filter-secondary",
                width = "100%"
              )
            )
          ),
          
          column(
            8,
            
            fluidRow(
              column(
                4,
                div(
                  class = "metric-card",
                  div(class = "metric-label", "Consulta activa"),
                  div(
                    class = "metric-value metric-value-search",
                    textOutput("buscador_consulta_activa")
                  ),
                  div(class = "metric-foot", "Texto procesado")
                )
              ),
              
              column(
                4,
                div(
                  class = "metric-card",
                  div(class = "metric-label", "Estrategia"),
                  div(
                    class = "metric-value",
                    textOutput("buscador_estrategia_activa")
                  ),
                  div(class = "metric-foot", "Método seleccionado")
                )
              ),
              
              column(
                4,
                div(
                  class = "metric-card",
                  div(class = "metric-label", "Resultados"),
                  div(
                    class = "metric-value",
                    textOutput("buscador_total_resultados")
                  ),
                  div(class = "metric-foot", "Artículos recuperados")
                )
              )
            ),
            
            div(
              class = "panel-card",
              h3("Estado de la búsqueda"),
              uiOutput("mensaje_buscador")
            )
          )
        ),
        
        tags$br(),
        
        div(
          class = "panel-card",
          h3("Resultados ordenados por similitud"),
          DTOutput("tabla_resultados_buscador")
        ),
        
        tags$br(),
        
        conditionalPanel(
          condition = "input.estrategia_buscador == 'Comparar'",
          
          div(
            class = "panel-card",
            h3("Comparación de estrategias"),
            p(
              class = "section-note",
              "La tabla resume los artículos recuperados por TF-IDF y LSA. Un mismo artículo puede aparecer en posiciones diferentes según la representación utilizada."
            ),
            DTOutput("tabla_comparacion_buscador")
          )
        )
      ),
      
      # Metodología
      
      tabPanel(
        "Metodología",
        
        h2(
          class = "section-title",
          "Metodología del proyecto"
        ),
        
        p(
          class = "section-note",
          "Esta sección resume la construcción de la base de datos, la preparación del corpus y las estrategias de recuperación de información implementadas en la aplicación."
        ),
        
        div(
          class = "methodology-grid",
          
          div(
            class = "method-card",
            div(class = "method-number", "01"),
            h3("Fuente de información"),
            p(
              "Se trabajó con artículos científicos de la revista Big Data and Cognitive Computing. Los registros incluyen título, resumen, autores, fecha de publicación, DOI, citas, referencias y categoría temática."
            )
          ),
          
          div(
            class = "method-card",
            div(class = "method-number", "02"),
            h3("Recolección mediante scraping"),
            p(
              "La información fue recolectada y actualizada mediante un proceso de web scraping. Los DOI encontrados se comparan con los registros existentes para evitar duplicados antes de guardar nuevos artículos en SQLite."
            )
          ),
          
          div(
            class = "method-card",
            div(class = "method-number", "03"),
            h3("Almacenamiento en SQLite"),
            p(
              "Los artículos se almacenan en una base de datos SQLite, lo que permite conservar una estructura reproducible, realizar consultas y actualizar la información sin modificar manualmente los registros existentes."
            )
          ),
          
          div(
            class = "method-card",
            div(class = "method-number", "04"),
            h3("Construcción del corpus"),
            p(
              "El corpus textual se construyó combinando principalmente el título y el resumen de cada artículo. También se conservaron campos descriptivos para mostrar los resultados y facilitar su interpretación."
            )
          ),
          
          div(
            class = "method-card",
            div(class = "method-number", "05"),
            h3("Preparación del texto"),
            p(
              "Los textos fueron normalizados mediante conversión a minúsculas, limpieza de caracteres, tokenización y eliminación de términos poco informativos. La misma configuración se aplica a los documentos y a las consultas."
            )
          ),
          
          div(
            class = "method-card",
            div(class = "method-number", "06"),
            h3("Representación TF-IDF"),
            p(
              "Cada artículo se representó mediante TF-IDF, asignando mayor peso a los términos importantes dentro de un documento y menor peso a los términos frecuentes en todo el corpus."
            )
          ),
          
          div(
            class = "method-card",
            div(class = "method-number", "07"),
            h3("Similitud coseno"),
            p(
              "La consulta se transforma con el mismo vocabulario del corpus y se compara con cada artículo mediante similitud coseno. Los documentos con mayor similitud se presentan en las primeras posiciones."
            )
          ),
          
          div(
            class = "method-card",
            div(class = "method-number", "08"),
            h3("Representación LSA"),
            p(
              "Se aplicó Latent Semantic Analysis mediante descomposición truncada para reducir la representación TF-IDF a 200 componentes. Esto permite identificar relaciones semánticas que no dependen únicamente de coincidencias literales."
            )
          ),
          
          div(
            class = "method-card",
            div(class = "method-number", "09"),
            h3("Comparación de estrategias"),
            p(
              "La aplicación permite ejecutar TF-IDF, LSA o ambas estrategias. Para cada consulta se comparan posiciones, puntajes y coincidencias entre los artículos recuperados."
            )
          ),
          
          div(
            class = "method-card",
            div(class = "method-number", "10"),
            h3("Evaluación de relevancia"),
            p(
              "Se evaluaron manualmente los cinco primeros resultados de cinco consultas para cada estrategia. TF-IDF y LSA obtuvieron una Precision@5 promedio de 0.92 y un MRR de 0.90."
            )
          ),
          
          div(
            class = "method-card",
            div(class = "method-number", "11"),
            h3("Aplicación Shiny"),
            p(
              "Los modelos y matrices se cargan desde objetos previamente guardados para evitar reconstruirlos en cada búsqueda. La interfaz permite consultar, comparar y abrir el DOI de los artículos recuperados."
            )
          ),
          
          div(
            class = "method-card",
            div(class = "method-number", "12"),
            h3("Reproducibilidad"),
            p(
              "El proyecto conserva la base SQLite, los scripts de preparación, los modelos, las funciones de búsqueda y los archivos de evaluación. Esto permite reconstruir el proceso completo o ejecutar directamente la aplicación."
            )
          )
        ),
        
        div(
          class = "methodology-summary",
          h3("Flujo general"),
          p(
            "Recolección y almacenamiento → construcción del corpus → limpieza y tokenización → TF-IDF → reducción LSA → similitud coseno → ranking de resultados → evaluación manual → visualización en Shiny."
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
          "Ejecuta un proceso de scraping para buscar artículos nuevos, compararlos con los DOI almacenados e insertarlos en SQLite cuando corresponde."
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
      "BDCC Research Dashboard · Taller 4 · Minería de Datos · Universidad Nacional"
    )
  )
)