#------------------------------------------------------------
# Buscador académico de Frontiers in Bioinformatics
# Exploración, actualización y recuperación por relevancia
#------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman") # Instalar pacman si no está instalado
if (!requireNamespace("tm", quietly = TRUE)) install.packages("tm") # Instalar tm sin adjuntarlo al search path

pacman::p_load( # Carga de paquetes necesarios para la implementación de la aplicación
  shiny,          # Construir la aplicación web con Shiny
  shinydashboard, # Organizar la app como dashboard
  DBI,            # Conectar R con SQLite
  RSQLite,        # Usar la base SQLite local
  dplyr,          # Manipular tablas
  stringr,        # Limpiar y buscar texto
  tibble,         # Trabajar con tibbles
  DT,             # Mostrar tablas interactivas
  highcharter,    # Crear gráficos interactivos
  lubridate,      # Agrupar y manejar fechas
  rvest,          # Extraer información HTML
  xml2,           # Leer documentos HTML/XML
  httr,           # Hacer solicitudes web
  purrr,          # Iterar sobre URLs/artículos
  tidyr,          # Apoyar limpieza de datos
  janitor,         # Limpiar nombres de columnas cuando haga falta
  tidytext,        # Procesar consultas y documentos
  proxy            # Calcular similitud coseno
)

options(width = 140) # Ampliar consola para revisar salidas

ruta_sqlite <- "revista_q1_2025.sqlite" # Base SQLite del Taller 1
tabla_principal <- "papers" # Tabla principal de la aplicación
carpeta_salida <- "salidas_taller4" # Carpeta de controles y respaldos
archivo_scraping <- file.path("R", "scraping_frontiers.R") # Actualización de artículos
archivo_preparacion_busqueda <- file.path(
  "R",
  "preparar_indices_busqueda.R"
) # Preparación de la colección de búsqueda

if (!dir.exists(carpeta_salida)) { # Crear carpeta de salidas si hace falta
  dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)
}

content <- httr::content # Evitar que tm o NLP sustituyan el lector JSON de httr
source(archivo_scraping) # Cargar funciones de extracción y clasificación de artículos

if (!file.exists(archivo_preparacion_busqueda)) { # Verificar función que reconstruye los índices
  stop("No están disponibles los recursos necesarios para actualizar la colección.")
}

source(archivo_preparacion_busqueda) # Cargar función reutilizable de preparación TF-IDF y LSA

archivo_modelo_busqueda <- file.path(
  "modelos",
  "modelo_busqueda_taller4.rds"
) # Archivo RDS con metadatos, vocabulario, matrices y componentes SVD

archivo_funciones_busqueda <- file.path(
  "modelos",
  "funciones_busqueda_taller4.rds"
) # Archivo RDS con funciones de procesamiento y recuperación

if (
  !file.exists(archivo_modelo_busqueda) ||
  !file.exists(archivo_funciones_busqueda)
) {
  stop("No están disponibles los recursos necesarios para realizar búsquedas.")
}

modelo_busqueda_inicial <- readRDS(archivo_modelo_busqueda) # Cargar índices una sola vez al iniciar Shiny
funciones_busqueda_inicial <- readRDS(archivo_funciones_busqueda) # Cargar funciones sin recalcular el corpus

funciones_requeridas <- c( # Validar que el entorno cargado contiene todo el flujo de búsqueda
  "procesar_consulta",
  "vectorizar_consulta_tfidf",
  "formatear_ranking",
  "buscar_tfidf",
  "buscar_lsa"
)

if (
  !is.environment(funciones_busqueda_inicial) ||
  !all(funciones_requeridas %in% ls(funciones_busqueda_inicial))
) {
  stop("No fue posible cargar las funciones de búsqueda.")
}


#------------------------------------------------------------
# Funciones de lectura, comparación y guardado
# Estas funciones mantienen la conexión entre Shiny, SQLite y el scraping
#------------------------------------------------------------

leer_papers <- function() { # Leer papers desde SQLite y preparar tipos para filtros/salidas
  con <- dbConnect(SQLite(), ruta_sqlite) # Abrir conexión temporal
  
  papers <- dbGetQuery( # Consultar la tabla completa
    con,
    paste0("SELECT * FROM ", dbQuoteIdentifier(con, tabla_principal))
  )
  
  dbDisconnect(con) # Cerrar conexión para no bloquear la base
  
  papers |>
    as_tibble() |> # Convertir a tibble para usar biblioteca tidyverse
    mutate(
      publication_date = as.Date(publication_date), # Fecha lista para dateRangeInput()
      across(
        any_of(c(
          "journal_name",
          "article_type",
          "publication_status",
          "volume_text",
          "section",
          "title",
          "doi",
          "url",
          "abstract",
          "authors_raw",
          "citation_source",
          "metric_used_as_downloads",
          "keywords",
          "topic_label",
          "topic_source",
          "topic_evidence",
          "topic_confidence",
          "manual_topic_label",
          "manual_topic_source",
          "manual_topic_evidence",
          "manual_topic_status"
        )),
        as.character
      ),
      across(
        any_of(c(
          "year",
          "volume_number",
          "volume_year",
          "n_authors",
          "citations",
          "downloads",
          "n_references"
        )),
        as.numeric
      )
    )
}

promedio_seguro <- function(x) { # Calcular promedios considerando entradas nulas o vacías
  if (length(x) == 0 || all(is.na(x))) {
    return(0)
  }
  
  round(mean(x, na.rm = TRUE), 2)
}

detectar_nuevos <- function(scraping, existentes) { # Comparar scraping contra SQLite por DOI o URL
  if (nrow(scraping) == 0) {
    return(scraping)
  }
  
  scraping_llave <- scraping |>
    mutate(
      llave = if_else(
        !is.na(doi) & str_squish(doi) != "",
        str_to_lower(str_squish(doi)),
        str_to_lower(str_squish(url))
      )
    ) |>
    filter(!is.na(llave), llave != "")
  
  existentes_llave <- existentes |>
    mutate(
      llave = if_else(
        !is.na(doi) & str_squish(doi) != "",
        str_to_lower(str_squish(doi)),
        str_to_lower(str_squish(url))
      )
    ) |>
    filter(!is.na(llave), llave != "") |>
    select(llave) |>
    distinct()
  
  scraping_llave |>
    anti_join(existentes_llave, by = "llave") |>
    select(-llave)
}

preparar_para_sqlite <- function(nuevos, columnas_papers, max_paper_id) { # Alinear artículos nuevos con la estructura de papers
  if (nrow(nuevos) == 0) {
    return(nuevos)
  }
  
  faltantes <- setdiff(columnas_papers, names(nuevos))
  
  for (columna in faltantes) {
    nuevos[[columna]] <- NA
  }
  
  columnas_texto <- c(
    "journal_name",
    "article_type",
    "publication_status",
    "volume_text",
    "section",
    "title",
    "publication_date",
    "doi",
    "url",
    "abstract",
    "authors_raw",
    "citation_source",
    "metric_used_as_downloads",
    "keywords",
    "topic_label",
    "topic_source",
    "topic_evidence",
    "topic_confidence",
    "manual_topic_label",
    "manual_topic_source",
    "manual_topic_evidence",
    "manual_topic_status"
  )
  
  columnas_numericas <- c(
    "volume_number",
    "volume_year",
    "year",
    "n_authors",
    "citations",
    "downloads",
    "n_references"
  )
  
  nuevos |>
    select(any_of(columnas_papers)) |>
    mutate(
      paper_id = seq.int(
        from = max_paper_id + 1L,
        length.out = n()
      ),
      across(any_of(columnas_texto), as.character),
      across(any_of(columnas_numericas), as.numeric)
    )
}

guardar_nuevos <- function(nuevos) { # Insertar en SQLite solo artículos auténticamente nuevos
  con <- dbConnect(SQLite(), ruta_sqlite)
  on.exit(dbDisconnect(con), add = TRUE)
  
  papers_existentes <- dbGetQuery(
    con,
    paste0("SELECT * FROM ", dbQuoteIdentifier(con, tabla_principal))
  ) |>
    as_tibble()
  
  if (nrow(nuevos) == 0) {
    return(list(
      n_recibidos = 0,
      n_insertados = 0,
      papers_actualizados = papers_existentes,
      articulos_insertados = tibble(),
      ruta_respaldo = NA_character_
    ))
  }
  
  nuevos_reales <- detectar_nuevos(
    scraping = nuevos,
    existentes = papers_existentes
  )
  
  if (nrow(nuevos_reales) == 0) {
    return(list(
      n_recibidos = nrow(nuevos),
      n_insertados = 0,
      papers_actualizados = papers_existentes,
      articulos_insertados = tibble(),
      ruta_respaldo = NA_character_
    ))
  }
  
  ruta_respaldo <- file.path(
    carpeta_salida,
    paste0(
      "respaldo_revista_q1_2025_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      ".sqlite"
    )
  )
  
  file.copy(
    from = ruta_sqlite,
    to = ruta_respaldo,
    overwrite = TRUE
  )
  
  columnas_papers <- dbListFields(con, tabla_principal)
  
  max_paper_id <- dbGetQuery(
    con,
    paste0("SELECT MAX(paper_id) AS max_id FROM ", dbQuoteIdentifier(con, tabla_principal))
  )$max_id
  
  if (is.na(max_paper_id)) {
    max_paper_id <- 0
  }
  
  nuevos_insertar <- preparar_para_sqlite(
    nuevos = nuevos_reales,
    columnas_papers = columnas_papers,
    max_paper_id = max_paper_id
  )
  
  dbWithTransaction(
    con,
    dbWriteTable(
      con,
      tabla_principal,
      nuevos_insertar,
      append = TRUE
    )
  )
  
  papers_actualizados <- dbGetQuery(
    con,
    paste0("SELECT * FROM ", dbQuoteIdentifier(con, tabla_principal))
  ) |>
    as_tibble()
  
  list(
    n_recibidos = nrow(nuevos),
    n_insertados = nrow(nuevos_insertar),
    papers_actualizados = papers_actualizados,
    articulos_insertados = nuevos_insertar,
    ruta_respaldo = ruta_respaldo
  )
}

revisar_ultimos_cinco <- function(existentes) { # Reconsultar últimos cinco artículos si no hay artículos nuevos
  ultimos <- existentes |>
    mutate(publication_date = as.Date(publication_date)) |>
    arrange(desc(publication_date), desc(paper_id)) |>
    slice_head(n = 5)
  
  if (nrow(ultimos) == 0) {
    return(tibble())
  }
  
  tomar_metrica <- function(datos, columna, defecto = NA) {
    if (!columna %in% names(datos) || nrow(datos) == 0) {
      return(defecto)
    }
    datos[[columna]][1]
  }
  
  map_dfr(seq_len(nrow(ultimos)), function(i) {
    articulo_base <- ultimos[i, ]
    
    listado_base <- tibble(
      doi = articulo_base$doi,
      downloads = articulo_base$downloads
    )
    
    consulta <- try(
      extraer_articulo_reciente(
        url_articulo = articulo_base$url,
        listado_reciente = listado_base
      ),
      silent = TRUE
    )
    
    pagina_accesible <- !inherits(consulta, "try-error") &&
      nrow(consulta) > 0 &&
      !identical(tomar_metrica(consulta, "extraction_status"), "error_html")
    
    citations_anterior <- as.numeric(articulo_base$citations)
    downloads_anterior <- as.numeric(articulo_base$downloads)
    n_references_anterior <- as.numeric(articulo_base$n_references)
    
    citations_actual <- if (pagina_accesible) as.numeric(tomar_metrica(consulta, "citations")) else NA_real_
    downloads_actual <- if (pagina_accesible) as.numeric(tomar_metrica(consulta, "downloads")) else NA_real_
    n_references_actual <- if (pagina_accesible) as.numeric(tomar_metrica(consulta, "n_references")) else NA_real_
    
    cambio_citations <- ifelse(
      !is.na(citations_anterior) & !is.na(citations_actual),
      citations_actual - citations_anterior,
      NA_real_
    )
    
    cambio_downloads <- ifelse(
      !is.na(downloads_anterior) & !is.na(downloads_actual),
      downloads_actual - downloads_anterior,
      NA_real_
    )
    
    cambio_n_references <- ifelse(
      !is.na(n_references_anterior) & !is.na(n_references_actual),
      n_references_actual - n_references_anterior,
      NA_real_
    )
    
    estado_revision <- case_when(
      !pagina_accesible ~ "Página no accesible",
      any(c(cambio_citations, cambio_downloads, cambio_n_references) != 0, na.rm = TRUE) ~ "Métricas con cambios",
      TRUE ~ "Sin cambios detectados"
    )
    
    tibble(
      paper_id = articulo_base$paper_id,
      title = articulo_base$title,
      publication_date = articulo_base$publication_date,
      doi = articulo_base$doi,
      url = articulo_base$url,
      topic_label = articulo_base$topic_label,
      pagina_accesible = pagina_accesible,
      citations_anterior = citations_anterior,
      citations_actual = citations_actual,
      cambio_citations = cambio_citations,
      downloads_anterior = downloads_anterior,
      downloads_actual = downloads_actual,
      cambio_downloads = cambio_downloads,
      n_references_anterior = n_references_anterior,
      n_references_actual = n_references_actual,
      cambio_n_references = cambio_n_references,
      estado_revision = estado_revision
    )
  })
}

crear_tarjeta <- function(titulo, valor, subtitulo = NULL, color = "blue", icono = "bar-chart") { # Construir caja visual de indicador
  div(
    class = paste("small-box bg-", color, sep = ""),
    style = "min-height: 100px; position: relative; overflow: hidden;",
    div(
      class = "inner",
      style = "padding-right: 85px;",
      tags$p(
        titulo,
        style = "font-size: 17px; font-weight: 700; margin-bottom: 10px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"
      ),
      if (!is.null(subtitulo)) tags$p(
        subtitulo,
        style = "font-size: 14px; margin-bottom: 8px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"
      ),
      tags$h3(
        valor,
        style = "font-size: 36px; font-weight: 700; margin-top: 0; margin-bottom: 0;"
      )
    ),
    div(
      class = "icon",
      icon(icono)
    )
  )
}

#------------------------------------------------------------
# Interfaz
#------------------------------------------------------------

ui <- dashboardPage( # Interfaz principal del dashboard
  dashboardHeader( # Encabezado superior
    title = tags$span(
      "Buscador Revista Frontiers in Bioinformatics",
      style = "font-size: 14px; font-weight: 700; line-height: 16px; white-space: normal; display: block; padding-top: 2px;"
    ),
    titleWidth = 300
  ),
  dashboardSidebar( # Barra lateral con filtros y actualización
    width = 300,
    sidebarMenu(
      id = "tabs_principales",
      menuItem(
        "Exploración académica",
        tabName = "base",
        icon = icon("search")
      ),
      menuItem(
        "Búsqueda por relevancia",
        tabName = "relevancia",
        icon = icon("sort-amount-desc")
      )
    ),
    conditionalPanel(
      condition = "input.tabs_principales == 'base'",
      dateRangeInput(
        inputId = "rango_fechas",
        label = "Rango de fechas",
        start = NULL,
        end = NULL
      ),
      selectInput(
        inputId = "tema",
        label = "Tema o categoría",
        choices = "Todos"
      ),
      textInput(
        inputId = "autor",
        label = "Autor",
        value = ""
      ),
      textInput(
        inputId = "doi",
        label = "DOI",
        value = ""
      ),
      textInput(
        inputId = "busqueda",
        label = "Buscar en título, resumen o palabras clave",
        value = ""
      )
    ),
    actionButton(
      inputId = "actualizar",
      label = "Actualizar colección",
      icon = icon("refresh")
    )
  ),
  dashboardBody( # Contenido principal de la app
    tags$head(
      tags$style(HTML("
        body,
        .content-wrapper,
        .right-side {
          background-color: #f6f1e8;
        }
        .main-header .logo {
          background-color: #20303a !important;
          color: #f7efe4 !important;
          width: 300px !important;
          font-size: 14px;
          font-weight: 700;
          letter-spacing: 0.1px;
          line-height: 18px;
          padding-top: 8px;
          text-align: left;
        }
        .main-header .navbar {
          background-color: #2f4858 !important;
          margin-left: 300px !important;
        }
        .main-sidebar {
          position: fixed;
          top: 50px;
          bottom: 0;
          width: 300px;
          overflow-y: auto;
          padding-bottom: 30px;
          background-color: #25333b;
          border-right: 1px solid #d8c8b4;
        }
        .sidebar {
          padding-top: 4px;
        }
        .sidebar-menu {
          margin-top: 0;
        }
        .sidebar-menu > li > a {
          color: #f4eadc;
          font-weight: 600;
        }
        .sidebar-menu > li.active > a,
        .sidebar-menu > li:hover > a {
          background-color: #3b5563 !important;
          color: #ffffff;
          border-left-color: #d79a5b;
        }
        .sidebar label {
          color: #efe5d6;
          font-weight: 600;
        }
        .sidebar .form-control,
        .sidebar .selectize-input {
          border-radius: 7px;
          border: 1px solid #c9b79f;
          background-color: #fffdf8;
        }
        .content-wrapper, .right-side {
          margin-left: 300px;
          overflow-x: hidden !important;
        }
        .content {
          padding: 22px;
          max-width: 100%;
          overflow-x: hidden !important;
        }
        .hero-panel {
          background: linear-gradient(135deg, #fffaf1 0%, #eadcc7 100%);
          border: 1px solid #d6c3a9;
          border-radius: 10px;
          padding: 18px 22px;
          margin-bottom: 18px;
          box-shadow: 0 1px 4px rgba(48, 61, 69, 0.12);
        }
        .hero-panel h2 {
          margin-top: 0;
          color: #243845;
          font-weight: 700;
        }
        .hero-panel p {
          color: #5c554b;
          margin-bottom: 0;
          font-size: 15px;
        }
        #actualizar {
          display: block !important;
          width: calc(100% - 24px) !important;
          max-width: calc(100% - 24px) !important;
          margin: 0 12px 12px 12px !important;
          white-space: normal !important;
          box-sizing: border-box !important;
          border-radius: 8px;
          border: 1px solid #b87946;
          background-color: #b87946;
          color: #ffffff;
          font-weight: 700;
        }
        #actualizar:hover {
          background-color: #9f6538;
          border-color: #9f6538;
        }
        #ejecutar_busqueda {
          border-radius: 8px;
          border: 1px solid #3b5563;
          background-color: #3b5563;
          color: #ffffff;
          font-weight: 700;
          padding: 8px 20px;
        }
        #ejecutar_busqueda:hover {
          background-color: #2f4858;
          border-color: #2f4858;
        }
        .nota-buscador {
          background-color: #fffaf1;
          border-left: 4px solid #c89d58;
          padding: 11px 14px;
          margin-top: 12px;
          margin-bottom: 12px;
          color: #4f4941;
        }
        .mensaje-busqueda-error {
          background-color: #f7e4df;
          border: 1px solid #c78b7c;
          border-radius: 8px;
          color: #723f35;
          padding: 11px 14px;
          margin-bottom: 14px;
        }
        .mensaje-busqueda-correcto {
          background-color: #e8f0f3;
          border: 1px solid #8da6b1;
          border-radius: 8px;
          color: #334b56;
          padding: 11px 14px;
          margin-bottom: 14px;
        }
        .box {
          max-width: 100%;
          overflow: hidden;
          border-radius: 9px;
          border-top: 0;
          box-shadow: 0 1px 4px rgba(48, 61, 69, 0.12);
        }
        .box-body {
          max-width: 100%;
          overflow-x: hidden;
        }
        .box.box-solid.box-primary > .box-header,
        .box.box-solid.box-info > .box-header {
          background-color: #6c7d63;
        }
        .box.box-solid > .box-header .box-title {
          color: #fff7ea;
          font-weight: 600;
          letter-spacing: 0.15px;
        }
        h3 {
          color: #2b3a3f;
          font-weight: 700;
          margin-top: 18px;
          letter-spacing: 0.1px;
        }
        .dataTables_wrapper {
          width: 100% !important;
          max-width: 100% !important;
          overflow-x: hidden !important;
        }
        table.dataTable {
          width: 100% !important;
          table-layout: fixed !important;
        }
        table.dataTable th,
        table.dataTable td {
          white-space: normal !important;
          word-break: break-word;
          vertical-align: top;
        }
        .dataTables_paginate {
          white-space: normal !important;
        }
        .small-box {
          min-height: 105px;
          border-radius: 10px;
          box-shadow: 0 1px 4px rgba(48, 61, 69, 0.14);
        }
        .small-box .inner {
          min-height: 105px;
        }
        .small-box .inner p {
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        .bg-blue { background-color: #4f6f8f !important; }
        .bg-aqua { background-color: #5f8f9c !important; }
        .bg-green { background-color: #698b68 !important; }
        .bg-yellow { background-color: #c89d58 !important; color: #ffffff !important; }
        .bg-purple { background-color: #796a91 !important; }
        .bg-red { background-color: #a66356 !important; }
        .bg-fuchsia { background-color: #9b6f8d !important; }
        @media (max-width: 767px) {
          .main-sidebar {
            position: absolute;
          }
          .content-wrapper, .right-side {
            margin-left: 0;
          }
          .main-header .navbar {
            margin-left: 0 !important;
          }
          .main-header .logo {
            width: 100% !important;
            text-align: center;
          }
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "base",
        div(
          class = "hero-panel",
          h2("Buscador de artículos Frontiers in Bioinformatics"),
        ),
        h3("Indicadores principales"), # Resumen cuantitativo filtrado
        fluidRow(
          column(4, uiOutput("total_articulos")),
          column(4, uiOutput("prom_autores")),
          column(4, uiOutput("prom_citas"))
        ),
        fluidRow(
          column(4, uiOutput("prom_referencias")),
          column(4, uiOutput("n_temas")),
          column(4, uiOutput("articulo_mas_citado"))
        ),
        fluidRow(
          column(4, uiOutput("articulo_mas_descargado"))
        ),
        h3("Gráficos interactivos"), # Visualizaciones alimentadas por papers_filtrados()
        fluidRow(
          box(
            width = 6,
            title = "Comparativo por temática",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("grafico_temas", height = "360px")
          ),
          box(
            width = 6,
            title = "Registro temporal",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("grafico_tiempo", height = "360px")
          )
        ),
        h3("Relaciones y visibilidad"), # Exploración complementaria de autores y métricas
        fluidRow(
          box(
            width = 6,
            title = "Autores con más publicaciones y visibilidad",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("grafico_relacion_tema_autor", height = "360px")
          ),
          box(
            width = 6,
            title = "Artículos más visitados",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("grafico_top_descargas", height = "360px")
          )
        ),
        h3("Tabla interactiva"), # Consulta final de los artículos
        fluidRow(
          box(
            width = 12,
            title = "Artículos filtrados",
            status = "primary",
            solidHeader = TRUE,
            p("Use la barra lateral para filtrar los artículos con mayor precisión."), # Evitar buscador duplicado en DT (DataTables)
            DT::DTOutput("tabla_articulos")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Actualización de la colección",
            status = "info",
            solidHeader = TRUE,
            textOutput("mensaje_actualizacion"),
            DT::DTOutput("tabla_ultimos_revisados")
          )
        )
      ),
      tabItem(
        tabName = "relevancia",
        div(
          class = "hero-panel",
          h2("Búsqueda de artículos por relevancia"),
          p(
            paste(
              "Escriba una consulta temática y explore los artículos más",
              "relacionados mediante dos estrategias de recuperación."
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Consulta en lenguaje natural",
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(
                width = 6,
                textAreaInput(
                  inputId = "consulta_relevancia",
                  label = "Escriba la consulta",
                  value = "",
                  rows = 3,
                  placeholder = "Ejemplo: single-cell RNA sequencing"
                )
              ),
              column(
                width = 3,
                selectInput(
                  inputId = "estrategia_relevancia",
                  label = "Estrategia de recuperación",
                  choices = c(
                    "TF-IDF" = "TF-IDF",
                    "LSA" = "LSA",
                    "Comparar ambas" = "Comparar ambas"
                  ),
                  selected = "TF-IDF"
                )
              ),
              column(
                width = 3,
                selectInput(
                  inputId = "n_resultados_relevancia",
                  label = "Número de resultados",
                  choices = c(5, 10, 20),
                  selected = 10
                )
              )
            ),
            actionButton(
              inputId = "ejecutar_busqueda",
              label = "Buscar por relevancia",
              icon = icon("search")
            ),
            div(
              class = "nota-buscador",
              tags$b("Interpretación: "),
              paste(
                "el puntaje representa similitud dentro de la estrategia",
                "seleccionada y no corresponde a una probabilidad.",
                "Los valores TF-IDF y LSA no deben compararse directamente."
              )
            )
          )
        ),
        uiOutput("mensaje_busqueda_relevancia"),
        fluidRow(
          box(
            width = 12,
            title = "Actualización de la colección",
            status = "info",
            solidHeader = TRUE,
            p(
              paste(
                "Use el botón de la barra lateral para consultar las",
                "publicaciones más recientes de la revista."
              )
            ),
            textOutput("mensaje_actualizacion_relevancia")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Ranking de artículos",
            status = "primary",
            solidHeader = TRUE,
            DT::DTOutput("tabla_resultados_relevancia")
          )
        )
      )
    )
  )
)

#------------------------------------------------------------
# Servidor
# Exploración, búsqueda por relevancia y actualización
#------------------------------------------------------------

server <- function(input, output, session) { # Servidor: reactivos, filtros, salidas y scraping
  papers <- reactiveVal(leer_papers()) # Base activa que alimenta toda la app
  mensaje_actualizacion <- reactiveVal("") # Texto mostrado después del botón
  ultimos_revisados <- reactiveVal(tibble()) # Resultado de revisar últimos cinco artículos
  papers_antes_actualizacion <- reactiveVal(tibble()) # Base previa para comparar proporciones después de actualizar
  modelo_busqueda_activo <- reactiveVal(modelo_busqueda_inicial) # Modelo vigente durante la sesión
  funciones_busqueda_activas <- reactiveVal(funciones_busqueda_inicial) # Funciones vigentes durante la sesión
  
  resultado_busqueda_relevancia <- reactiveVal(NULL) # Guardar el último ranking generado
  
  observeEvent(input$ejecutar_busqueda, { # Responder desde el primer clic del botón
    consulta <- str_squish(input$consulta_relevancia) # Limpiar espacios de la consulta escrita
    estrategia <- input$estrategia_relevancia # Recuperar método seleccionado por el usuario
    n_resultados <- as.integer(input$n_resultados_relevancia) # Convertir 5, 10 o 20 a entero
    modelo <- modelo_busqueda_activo() # Tomar matrices y parámetros actualmente cargados
    funciones <- funciones_busqueda_activas() # Tomar funciones actualmente cargadas
    
    resultado <- if (consulta == "") {
      list(
        datos = tibble(),
        error = "Escriba una consulta antes de ejecutar la búsqueda."
      )
    } else {
      tryCatch({ # Evitar que una consulta inválida cierre la aplicación
        datos <- if (estrategia == "TF-IDF") { # Ranking por coincidencia ponderada
          funciones$buscar_tfidf(
            consulta = consulta,
            n_resultados = n_resultados,
            modelo = modelo
          ) |>
            mutate(estrategia = "TF-IDF", .before = posicion)
        } else if (estrategia == "LSA") { # Ranking en el espacio semántico reducido
          funciones$buscar_lsa(
            consulta = consulta,
            n_resultados = n_resultados,
            modelo = modelo
          ) |>
            mutate(estrategia = "LSA", .before = posicion)
        } else { # Ejecutar ambas estrategias y conservar cada ranking
          bind_rows(
            funciones$buscar_tfidf(
              consulta = consulta,
              n_resultados = n_resultados,
              modelo = modelo
            ) |>
              mutate(estrategia = "TF-IDF", .before = posicion),
            funciones$buscar_lsa(
              consulta = consulta,
              n_resultados = n_resultados,
              modelo = modelo
            ) |>
              mutate(estrategia = "LSA", .before = posicion)
          )
        }
        
        list(datos = datos, error = NULL)
      }, error = function(e) {
        list(
          datos = tibble(),
          error = conditionMessage(e)
        )
      })
    }
    
    resultado_busqueda_relevancia(resultado) # Actualizar mensajes y tabla en el mismo clic
  }, ignoreInit = TRUE)
  
  output$mensaje_busqueda_relevancia <- renderUI({ # Mostrar estado de la consulta en lenguaje expositivo
    resultado <- resultado_busqueda_relevancia()
    
    if (is.null(resultado)) {
      return(
        div(
          class = "mensaje-busqueda-correcto",
          "Escriba una consulta y seleccione una estrategia para generar el ranking."
        )
      )
    }
    
    if (!is.null(resultado$error)) {
      return(
        div(
          class = "mensaje-busqueda-error",
          icon("exclamation-circle"),
          resultado$error
        )
      )
    }
    
    n_seleccionado <- as.integer(input$n_resultados_relevancia)
    
    mensaje <- if (input$estrategia_relevancia == "Comparar ambas") {
      paste(
        "Se muestran",
        n_seleccionado,
        "resultados por estrategia."
      )
    } else {
      paste(
        "Se muestran",
        n_seleccionado,
        "artículos ordenados por relevancia."
      )
    }
    
    div(
      class = "mensaje-busqueda-correcto",
      icon("check-circle"),
      mensaje
    )
  })
  
  output$tabla_resultados_relevancia <- DT::renderDT({ # Presentar el ranking como tabla interactiva
    resultado <- resultado_busqueda_relevancia()
    req(!is.null(resultado))
    
    validate(
      need(is.null(resultado$error), resultado$error),
      need(nrow(resultado$datos) > 0, "No se recuperaron artículos.")
    )
    
    tabla_resultados <- resultado$datos |> # Preparar columnas visibles sin alterar el ranking
      mutate(
        title = str_trunc(coalesce(title, ""), 110),
        authors_raw = str_trunc(coalesce(authors_raw, ""), 85),
        topic_label = coalesce(topic_label, ""),
        doi = coalesce(doi, ""),
        puntaje = round(puntaje, 4),
        fragmento = coalesce(fragmento, ""),
        url = ifelse(
          is.na(url) | url == "",
          "",
          paste0("<a href='", url, "' target='_blank'>Abrir artículo</a>")
        )
      ) |>
      transmute(
        Estrategia = estrategia,
        Posición = posicion,
        Título = title,
        Autores = authors_raw,
        Fecha = publication_date,
        Tema = topic_label,
        DOI = doi,
        Puntaje = puntaje,
        Fragmento = fragmento,
        Enlace = url
      )
    
    DT::datatable(
      tabla_resultados,
      escape = FALSE,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(
        dom = "tip",
        pageLength = min(10, nrow(tabla_resultados)),
        lengthMenu = c(5, 10, 20, 40),
        searching = FALSE,
        ordering = TRUE,
        paging = TRUE,
        autoWidth = FALSE,
        scrollX = FALSE,
        columnDefs = list(
          list(width = "7%", targets = 0),
          list(width = "5%", targets = 1),
          list(width = "20%", targets = 2),
          list(width = "15%", targets = 3),
          list(width = "8%", targets = 4),
          list(width = "9%", targets = 5),
          list(width = "10%", targets = 6),
          list(width = "7%", targets = 7),
          list(width = "15%", targets = 8),
          list(width = "4%", targets = 9)
        )
      )
    )
  })
  
  observeEvent(papers(), { # Inicializar filtros desde la base vigente
    datos <- papers()
    
    fecha_min <- min(datos$publication_date, na.rm = TRUE)
    fecha_max <- max(datos$publication_date, na.rm = TRUE)
    
    updateDateRangeInput(
      session,
      inputId = "rango_fechas",
      start = fecha_min,
      end = fecha_max,
      min = fecha_min,
      max = fecha_max
    )
    
    temas_disponibles <- c("Todos", sort(unique(na.omit(datos$topic_label))))
    
    updateSelectInput(
      session,
      inputId = "tema",
      choices = temas_disponibles,
      selected = "Todos"
    )
  }, ignoreInit = FALSE)
  
  output$mensaje_actualizacion <- renderText({ # Mostrar resultado en exploración académica
    mensaje_actualizacion()
  })
  
  output$mensaje_actualizacion_relevancia <- renderText({ # Mostrar el mismo resultado en relevancia
    mensaje_actualizacion()
  })
  
  ejecutar_actualizacion_coleccion <- function() { # Ejecutar scraping y sincronización como una sola operación
    incProgress(0.10, detail = "Consultando la colección")
    existentes <- leer_papers() # Leer estado actual de SQLite antes del scraping
    columnas_papers <- names(existentes) # Conservar estructura exacta de papers
    
    incProgress(0.25, detail = "Consultando la revista")
    scraping <- scrapear_articulos_recientes( # Extraer publicaciones dentro del rango definido
      fecha_inicio = as.Date("2026-01-01"),
      fecha_fin = Sys.Date(),
      max_paginas = 10,
      limite_articulos = NULL,
      columnas_papers = columnas_papers
    )
    
    incProgress(0.15, detail = "Organizando los artículos")
    scraping <- clasificar_tema(scraping) # Aplicar clasificación temática
    
    incProgress(0.15, detail = "Verificando novedades")
    nuevos <- detectar_nuevos( # Comparar por DOI o URL
      scraping = scraping,
      existentes = existentes
    )
    
    if (nrow(nuevos) == 0) { # No reconstruir índices si SQLite no cambia
      ultimos <- revisar_ultimos_cinco(existentes)
      ultimos_revisados(ultimos)
      papers(existentes)
      
      return(
        "No se encontraron artículos nuevos. Se verificaron los cinco artículos más recientes."
      )
    }
    
    incProgress(0.10, detail = "Incorporando artículos nuevos")
    guardado <- guardar_nuevos(nuevos) # Insertar únicamente novedades reales
    
    if (guardado$n_insertados == 0) { # Confirmar el resultado después de la segunda validación
      ultimos <- revisar_ultimos_cinco(existentes)
      ultimos_revisados(ultimos)
      papers(existentes)
      
      return(
        "No se encontraron artículos nuevos. Se verificaron los cinco artículos más recientes."
      )
    }
    
    incProgress(0.25, detail = "Actualizando la colección de búsqueda")
    
    respaldo_modelo_busqueda <- tempfile(fileext = ".rds")
    respaldo_funciones_busqueda <- tempfile(fileext = ".rds")
    
    file.copy(
      archivo_modelo_busqueda,
      respaldo_modelo_busqueda,
      overwrite = TRUE
    )
    
    file.copy(
      archivo_funciones_busqueda,
      respaldo_funciones_busqueda,
      overwrite = TRUE
    )
    
    actualizacion_busqueda <- tryCatch( # Reconstruir una sola vez tras una inserción real
      preparar_indices_busqueda(
        ruta_sqlite = ruta_sqlite,
        tabla_principal = tabla_principal,
        archivo_modelo = archivo_modelo_busqueda,
        archivo_funciones = archivo_funciones_busqueda
      ),
      error = function(e) e
    )
    
    if (inherits(actualizacion_busqueda, "error")) { # Recuperar el estado anterior si falla la reconstrucción
      if (
        !is.na(guardado$ruta_respaldo) &&
        file.exists(guardado$ruta_respaldo)
      ) {
        file.copy(
          from = guardado$ruta_respaldo,
          to = ruta_sqlite,
          overwrite = TRUE
        )
      }
      
      file.copy(
        respaldo_modelo_busqueda,
        archivo_modelo_busqueda,
        overwrite = TRUE
      )
      
      file.copy(
        respaldo_funciones_busqueda,
        archivo_funciones_busqueda,
        overwrite = TRUE
      )
      
      unlink(c(
        respaldo_modelo_busqueda,
        respaldo_funciones_busqueda
      ))
      
      papers(leer_papers())
      
      stop(
        paste(
          "No fue posible completar la actualización.",
          "La colección anterior se conservó."
        )
      )
    }
    
    unlink(c(
      respaldo_modelo_busqueda,
      respaldo_funciones_busqueda
    ))
    
    modelo_nuevo <- readRDS(archivo_modelo_busqueda) # Leer índices recién construidos
    funciones_nuevas <- readRDS(archivo_funciones_busqueda) # Leer funciones validadas
    
    modelo_busqueda_activo(modelo_nuevo) # Recargar modelo sin reiniciar Shiny
    funciones_busqueda_activas(funciones_nuevas) # Recargar funciones en la sesión
    resultado_busqueda_relevancia(NULL) # Limpiar ranking anterior porque cambió el corpus
    papers(leer_papers()) # Refrescar indicadores, filtros, gráficos y tablas
    ultimos_revisados(tibble())
    
    paste(
      "Se incorporaron",
      guardado$n_insertados,
      "artículos nuevos. La búsqueda y los indicadores ya incluyen la colección actualizada."
    )
  }
  
  observeEvent(input$actualizar, { # Iniciar actualización desde cualquiera de las dos pestañas
    papers_antes_actualizacion(papers()) # Conservar base previa para comparación gráfica
    mensaje_actualizacion("Consultando publicaciones recientes...")
    
    withProgress(message = "Buscando artículos nuevos...", value = 0, {
      resultado <- tryCatch(
        ejecutar_actualizacion_coleccion(),
        error = function(e) {
          paste(
            "Ocurrió un error durante la actualización:",
            conditionMessage(e)
          )
        }
      )
      
      mensaje_actualizacion(resultado) # Guardar siempre el resultado, incluso si no hubo novedades
    })
  }, ignoreInit = TRUE)
  
  papers_filtrados <- reactive({ # Base única filtrada para indicadores, gráficos y tablas
    datos_filtrados <- papers() # Comenzar desde la tabla completa vigente
    
    if (
      is.null(input$rango_fechas) ||
      length(input$rango_fechas) != 2 ||
      any(is.na(input$rango_fechas)) ||
      is.null(input$tema)
    ) {
      return(datos_filtrados)
    }
    
    autor_txt <- str_to_lower(str_squish(input$autor))
    doi_txt <- str_to_lower(str_squish(input$doi))
    busqueda_txt <- str_to_lower(str_squish(input$busqueda))
    
    datos_filtrados <- datos_filtrados |>
      filter(
        publication_date >= input$rango_fechas[1],
        publication_date <= input$rango_fechas[2]
      ) |>
      filter(
        input$tema == "Todos" | topic_label == input$tema
      )
    
    if (autor_txt != "") {
      datos_filtrados <- datos_filtrados |>
        filter(str_detect(str_to_lower(coalesce(authors_raw, "")), fixed(autor_txt)))
    }
    
    if (doi_txt != "") {
      datos_filtrados <- datos_filtrados |>
        filter(str_detect(str_to_lower(coalesce(doi, "")), fixed(doi_txt)))
    }
    
    if (busqueda_txt != "") {
      datos_filtrados <- datos_filtrados |>
        filter(
          str_detect(str_to_lower(coalesce(title, "")), fixed(busqueda_txt)) |
            str_detect(str_to_lower(coalesce(abstract, "")), fixed(busqueda_txt)) |
            str_detect(str_to_lower(coalesce(keywords, "")), fixed(busqueda_txt))
        )
    }
    
    datos_filtrados
  })
  
  output$total_articulos <- renderUI({ # Indicador: total de artículos filtrados
    datos <- papers_filtrados()
    crear_tarjeta("Total de artículos", nrow(datos), color = "blue", icono = "file")
  })
  
  output$prom_autores <- renderUI({ # Indicador: promedio de autores
    datos <- papers_filtrados()
    crear_tarjeta("Promedio de autores", promedio_seguro(datos$n_authors), color = "aqua", icono = "users")
  })
  
  output$prom_citas <- renderUI({ # Indicador: promedio de citas
    datos <- papers_filtrados()
    crear_tarjeta("Promedio de citas", promedio_seguro(datos$citations), color = "green", icono = "quote-right")
  })
  
  output$prom_referencias <- renderUI({ # Indicador: promedio de referencias
    datos <- papers_filtrados()
    crear_tarjeta("Promedio de referencias", promedio_seguro(datos$n_references), color = "yellow", icono = "book")
  })
  
  output$n_temas <- renderUI({ # Indicador: número de temas disponibles
    datos <- papers_filtrados()
    crear_tarjeta("Número de temas", n_distinct(na.omit(datos$topic_label)), color = "purple", icono = "tags")
  })
  
  output$articulo_mas_citado <- renderUI({ # Indicador: artículo con más citas
    datos <- papers_filtrados() |>
      filter(!is.na(citations)) |>
      arrange(desc(citations)) |>
      slice_head(n = 1)
    
    nombre_paper <- if (nrow(datos) == 0) "Sin datos" else str_trunc(datos$title[1], 75)
    valor <- if (nrow(datos) == 0) 0 else datos$citations[1]
    crear_tarjeta("Artículo más citado", valor, subtitulo = nombre_paper, color = "red", icono = "star")
  })
  
  output$articulo_mas_descargado <- renderUI({ # Indicador: artículo con más descargas
    datos <- papers_filtrados() |>
      filter(!is.na(downloads)) |>
      arrange(desc(downloads)) |>
      slice_head(n = 1)
    
    nombre_paper <- if (nrow(datos) == 0) "Sin datos" else str_trunc(datos$title[1], 75)
    valor <- if (nrow(datos) == 0) 0 else datos$downloads[1]
    crear_tarjeta("Artículo más descargado", valor, subtitulo = nombre_paper, color = "fuchsia", icono = "download")
  })
  
  output$grafico_temas <- renderHighchart({ # Gráfico: artículos por temática
    datos_actual <- papers_filtrados() |>
      mutate(topic_label = coalesce(topic_label, "Sin tema")) |>
      count(topic_label, name = "n") |>
      arrange(desc(n))
    
    datos_previo_base <- papers_antes_actualizacion()
    
    validate(need(nrow(datos_actual) > 0, "No hay datos para graficar por temática."))
    
    datos_actual_grafico <- datos_actual |>
      transmute(name = topic_label, y = n)
    
    if (nrow(datos_previo_base) == 0) {
      return(
        highchart() |>
          hc_chart(
            type = "pie",
            spacingTop = 38,
            spacingRight = 15,
            spacingBottom = 56,
            spacingLeft = 15
          ) |>
          hc_title(text = "Artículos por temática") |>
          hc_subtitle(
            text = "Base actual",
            style = list(color = "#2b3a3f", fontSize = "13px", fontWeight = "600")
          ) |>
          hc_tooltip(
            pointFormat = "<b>{series.name}</b><br/>{point.name}: <b>{point.y}</b> ({point.percentage:.1f}%)"
          ) |>
          hc_plotOptions(
            pie = list(
              innerSize = "42%",
              dataLabels = list(enabled = FALSE)
            )
          ) |>
          hc_add_series(
            name = "Base actual",
            data = list_parse2(datos_actual_grafico),
            size = 180,
            center = c("50%", "60%"),
            showInLegend = TRUE
          ) |>
          hc_legend(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            y = 14,
            layout = "horizontal"
          )
      )
    }
    
    datos_previo <- datos_previo_base |>
      mutate(topic_label = coalesce(topic_label, "Sin tema")) |>
      count(topic_label, name = "n") |>
      arrange(desc(n)) |>
      transmute(name = topic_label, y = n)
    
    highchart() |>
      hc_chart(
        type = "pie",
        spacingTop = 48,
        spacingRight = 15,
        spacingBottom = 72,
        spacingLeft = 15,
        events = list(
          load = htmlwidgets::JS(
            "function () {
              var chart = this;
              if (chart.comparativoLabels) {
                chart.comparativoLabels.forEach(function(label) { label.destroy(); });
              }
              chart.comparativoLabels = [];
              var y = chart.plotTop + 8;
              var x1 = chart.plotLeft + chart.plotWidth * 0.28;
              var x2 = chart.plotLeft + chart.plotWidth * 0.72;
              chart.comparativoLabels.push(
                chart.renderer.text('Versión anterior', x1, y)
                  .attr({ align: 'center', zIndex: 5 })
                  .css({ color: '#2b3a3f', fontSize: '13px', fontWeight: '600' })
                  .add()
              );
              chart.comparativoLabels.push(
                chart.renderer.text('Última actualización', x2, y)
                  .attr({ align: 'center', zIndex: 5 })
                  .css({ color: '#2b3a3f', fontSize: '13px', fontWeight: '600' })
                  .add()
              );
            }"
          ),
          redraw = htmlwidgets::JS(
            "function () {
              var chart = this;
              if (chart.comparativoLabels) {
                chart.comparativoLabels.forEach(function(label) { label.destroy(); });
              }
              chart.comparativoLabels = [];
              var y = chart.plotTop + 8;
              var x1 = chart.plotLeft + chart.plotWidth * 0.28;
              var x2 = chart.plotLeft + chart.plotWidth * 0.72;
              chart.comparativoLabels.push(
                chart.renderer.text('Versión anterior', x1, y)
                  .attr({ align: 'center', zIndex: 5 })
                  .css({ color: '#2b3a3f', fontSize: '13px', fontWeight: '600' })
                  .add()
              );
              chart.comparativoLabels.push(
                chart.renderer.text('Última actualización', x2, y)
                  .attr({ align: 'center', zIndex: 5 })
                  .css({ color: '#2b3a3f', fontSize: '13px', fontWeight: '600' })
                  .add()
              );
            }"
          )
        )
      ) |>
      hc_title(text = "Artículos por temática") |>
      hc_tooltip(
        pointFormat = "<b>{series.name}</b><br/>{point.name}: <b>{point.y}</b> ({point.percentage:.1f}%)"
      ) |>
      hc_plotOptions(
        pie = list(
          innerSize = "42%",
          dataLabels = list(enabled = FALSE)
        )
      ) |>
      hc_add_series(
        name = "Versión anterior",
        data = list_parse2(datos_previo),
        size = 150,
        center = c("28%", "64%"),
        showInLegend = FALSE
      ) |>
      hc_add_series(
        name = "Última actualización",
        data = list_parse2(datos_actual_grafico),
        size = 150,
        center = c("72%", "64%"),
        showInLegend = TRUE
      ) |>
      hc_legend(
        enabled = TRUE,
        align = "center",
        verticalAlign = "bottom",
        y = 14,
        layout = "horizontal"
      )
  })
  
  output$grafico_tiempo <- renderHighchart({ # Gráfico: evolución temporal mensual
    datos <- papers_filtrados() |>
      filter(!is.na(publication_date)) |>
      mutate(mes = floor_date(publication_date, "month")) |>
      count(mes, name = "n") |>
      arrange(mes)
    
    validate(need(nrow(datos) > 0, "No hay datos para graficar la evolución temporal."))
    
    hchart(datos, "line", hcaes(x = mes, y = n)) |>
      hc_title(text = "Evolución temporal de publicaciones") |>
      hc_xAxis(title = list(text = "Mes")) |>
      hc_yAxis(title = list(text = "Número de artículos")) |>
      hc_tooltip(pointFormat = "Publicaciones: <b>{point.y}</b>") |>
      hc_plotOptions(line = list(dataLabels = list(enabled = TRUE)))
  })
  
  output$grafico_relacion_tema_autor <- renderHighchart({ # Gráfico: autores frecuentes y visibilidad
    datos_autores <- papers_filtrados() |>
      select(authors_raw, downloads, citations) |>
      mutate(
        authors_raw = coalesce(authors_raw, "Sin autor"),
        downloads = ifelse(is.na(downloads), 0, as.numeric(downloads)),
        citations = ifelse(is.na(citations), 0, as.numeric(citations))
      ) |>
      separate_rows(authors_raw, sep = ";") |>
      mutate(
        authors_raw = str_squish(authors_raw),
        authors_raw = ifelse(authors_raw == "" | is.na(authors_raw), "Sin autor", authors_raw)
      ) |>
      group_by(authors_raw) |>
      summarise(
        n_articulos = n(),
        total_descargas = sum(downloads, na.rm = TRUE),
        total_citas = sum(citations, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(n_articulos), desc(total_descargas), desc(total_citas)) |>
      slice_head(n = 10) |>
      mutate(autor = str_trunc(authors_raw, 34))
    
    validate(need(nrow(datos_autores) > 0, "No hay datos para graficar autores y visibilidad."))
    
    max_descargas <- max(datos_autores$total_descargas, na.rm = TRUE)
    max_citas <- max(datos_autores$total_citas, na.rm = TRUE)
    
    enlaces_descargas <- datos_autores |>
      mutate(
        metrica = "Descargas / visualizaciones",
        peso = ifelse(max_descargas > 0, total_descargas / max_descargas * 100, 0),
        valor_real = total_descargas
      ) |>
      transmute(
        from = autor,
        to = metrica,
        weight = pmax(peso, 1),
        valor_real = valor_real,
        n_articulos = n_articulos
      )
    
    enlaces_citas <- datos_autores |>
      mutate(
        metrica = "Citas",
        peso = ifelse(max_citas > 0, total_citas / max_citas * 100, 0),
        valor_real = total_citas
      ) |>
      transmute(
        from = autor,
        to = metrica,
        weight = pmax(peso, 1),
        valor_real = valor_real,
        n_articulos = n_articulos
      )
    
    enlaces <- bind_rows(enlaces_descargas, enlaces_citas) |>
      filter(!is.na(weight), weight > 0)
    
    highchart() |>
      hc_chart(type = "sankey") |>
      hc_title(text = "Relación temática-autor") |>
      hc_add_series(
        name = "Visibilidad",
        type = "sankey",
        keys = c("from", "to", "weight", "valor_real", "n_articulos"),
        data = list_parse2(enlaces)
      ) |>
      hc_tooltip(
        pointFormat = "<b>{point.from}</b> → <b>{point.to}</b><br/>Valor acumulado: <b>{point.valor_real}</b><br/>Artículos: <b>{point.n_articulos}</b>"
      )
  })
  
  output$grafico_top_descargas <- renderHighchart({ # Gráfico: artículos con mayor visibilidad
    paleta_temas <- c(
      "Otros" = "#8a9a5b",
      "Machine Learning" = "#6f8f72",
      "IA Generativa" = "#7f6f91",
      "Estadistica" = "#6f8fa6",
      "Sin tema" = "#9c8f7a"
    )
    
    datos <- papers_filtrados() |>
      filter(!is.na(downloads), downloads > 0) |>
      mutate(
        topic_label = coalesce(topic_label, "Sin tema"),
        title = str_trunc(coalesce(title, "Sin título"), 70),
        citations = ifelse(is.na(citations), 0, citations),
        color = unname(paleta_temas[topic_label]),
        color = ifelse(is.na(color), "#9c8f7a", color)
      ) |>
      arrange(desc(downloads)) |>
      slice_head(n = 12)
    
    validate(need(nrow(datos) > 0, "No hay datos para graficar artículos con mayor visibilidad."))
    
    datos_grafico <- datos |>
      transmute(
        name = title,
        y = downloads,
        color = color,
        tema = topic_label,
        citas = citations
      )
    
    highchart() |>
      hc_chart(type = "bar") |>
      hc_title(text = "Artículos con mayor visibilidad") |>
      hc_xAxis(type = "category", title = list(text = "Artículo"), reversed = TRUE) |>
      hc_yAxis(title = list(text = "Descargas / visualizaciones")) |>
      hc_add_series(
        name = "Descargas",
        data = list_parse(datos_grafico)
      ) |>
      hc_tooltip(
        useHTML = TRUE,
        pointFormat = "Tema: <b>{point.tema}</b><br/>Descargas: <b>{point.y}</b><br/>Citas: <b>{point.citas}</b>"
      ) |>
      hc_plotOptions(
        bar = list(
          dataLabels = list(enabled = TRUE),
          borderRadius = 3
        )
      ) |>
      hc_legend(enabled = FALSE)
  })
  
  output$tabla_articulos <- DT::renderDT({ # Tabla final alimentada por filtros
    tabla_articulos <- papers_filtrados() |>
      select(title, authors_raw, publication_date, topic_label, doi, citations, downloads, url) |>
      mutate(
        title = str_trunc(coalesce(title, ""), 100),
        authors_raw = str_trunc(coalesce(authors_raw, ""), 75),
        topic_label = coalesce(topic_label, ""),
        doi = coalesce(doi, ""),
        url = ifelse(
          is.na(url) | url == "",
          "",
          paste0("<a href='", url, "' target='_blank'>Abrir artículo</a>")
        )
      ) |>
      rename(
        Título = title,
        Autores = authors_raw,
        Fecha = publication_date,
        Tema = topic_label,
        DOI = doi,
        Citas = citations,
        Descargas = downloads,
        Enlace = url
      )
    
    validate(need(nrow(tabla_articulos) > 0, "No hay artículos que cumplan los filtros seleccionados."))
    
    DT::datatable(
      tabla_articulos,
      escape = FALSE,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(
        dom = "tip",
        pageLength = 10,
        lengthMenu = c(10, 25, 50),
        scrollX = FALSE,
        searching = FALSE,
        ordering = TRUE,
        paging = TRUE,
        autoWidth = FALSE,
        columnDefs = list(
          list(width = "32%", targets = 0),
          list(width = "23%", targets = 1),
          list(width = "9%", targets = 2),
          list(width = "8%", targets = 3),
          list(width = "13%", targets = 4),
          list(width = "5%", targets = 5),
          list(width = "6%", targets = 6),
          list(width = "4%", targets = 7)
        )
      )
    )
  })
  
  output$tabla_ultimos_revisados <- DT::renderDT({ # Tabla de revisión alternativa
    datos <- ultimos_revisados()
    
    req(nrow(datos) > 0)
    
    tabla_revision <- datos |>
      mutate(
        title = str_trunc(coalesce(title, ""), 100),
        doi = coalesce(doi, ""),
        pagina_accesible = ifelse(pagina_accesible, "Sí", "No")
      ) |>
      transmute(
        ID = paper_id,
        Título = title,
        Fecha = publication_date,
        DOI = doi,
        Tema = topic_label,
        `Página accesible` = pagina_accesible,
        `Cambio citas` = cambio_citations,
        `Cambio descargas` = cambio_downloads,
        `Cambio referencias` = cambio_n_references,
        Estado = estado_revision
      )
    
    DT::datatable(
      tabla_revision,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(
        dom = "t",
        pageLength = 5,
        scrollX = FALSE,
        autoWidth = FALSE
      )
    )
  })
}

#------------------------------------------------------------
# Ejecutar aplicación
#------------------------------------------------------------

shinyApp(ui, server) # Ejecutar aplicación

