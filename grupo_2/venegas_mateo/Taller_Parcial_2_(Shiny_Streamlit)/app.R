# ==============================================================================
# Taller Parcial 2 - Tablero Shiny: JAIR Web Scraping Dashboard
# Autor: Mateo Venegas Clavijo
# Minería de Datos - 2026
# ==============================================================================

library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(rvest)
library(httr)
library(jsonlite)
library(stringr)
library(plotly)
library(scales)

# --- Ruta a la base de datos -------------------------------------------------
# Ruta adaptativa: local para desarrollo, bundled para shinyapps.io
DB_PATH <- if (file.exists("revista_q1_2025.sqlite")) {
  "revista_q1_2025.sqlite"
} else {
  file.path("..", "Taller_Parcial_1_(WS_SQL)", "revista_q1_2025.sqlite")
}

# --- Funciones auxiliares de scraping ----------------------------------------

scrape_volume_articles <- function(vol_url) {
  pag <- tryCatch(read_html(vol_url), error = function(e) NULL)
  if (is.null(pag)) return(character(0))
  Sys.sleep(1)
  links <- pag |> html_nodes("a") |> html_attr("href")
  unique(links[grepl("^https://www.jair.org/index.php/jair/article/view/\\d+$", links)])
}

scrape_one_article <- function(article_url) {
  page <- tryCatch(read_html(article_url), error = function(e) NULL)
  if (is.null(page)) return(NULL)

  t       <- page |> html_node("meta[name='citation_title']")   |> html_attr("content")
  d       <- page |> html_node("meta[name='citation_date']")    |> html_attr("content")
  doi_val <- page |> html_node("meta[name='citation_doi']")     |> html_attr("content")
  auths   <- page |> html_nodes("meta[name='citation_author']") |> html_attr("content")
  abstr   <- page |> html_node(".article-abstract")             |> html_text(trim = TRUE)
  abstr   <- gsub("^Abstract\\s*", "", ifelse(is.null(abstr), NA_character_, abstr))

  citas_val <- NA_real_
  refs_val  <- NA_real_
  if (!is.na(doi_val)) {
    oa <- tryCatch(
      GET(paste0("https://api.openalex.org/works/https://doi.org/", doi_val),
          add_headers(`User-Agent` = "JAIR-Shiny-Dashboard/1.0")),
      error = function(e) NULL
    )
    if (!is.null(oa) && status_code(oa) == 200) {
      oa_data   <- content(oa, as = "parsed", type = "application/json")
      citas_val <- as.numeric(oa_data$cited_by_count)
      refs_val  <- as.numeric(length(oa_data$referenced_works))
    }
    Sys.sleep(0.3)
  }

  tibble(
    journal_name     = "Journal of Artificial Intelligence Research",
    title            = ifelse(is.na(t), "Sin titulo", t),
    publication_date = ifelse(is.na(d), NA_character_, d),
    year             = as.numeric(substr(d, 1, 4)),
    doi              = doi_val,
    url              = article_url,
    abstract         = abstr,
    authors_raw      = paste(auths, collapse = ", "),
    n_authors        = ifelse(length(auths) == 0, NA_real_, as.numeric(length(auths))),
    citations        = citas_val,
    n_references     = refs_val
  )
}

classify_topic <- function(title) {
  dplyr::case_when(
    grepl("generative|LLM|diffusion|large language|GPT|foundation model",
          title, ignore.case = TRUE) ~ "IA Generativa",
    grepl("learning|neural|reinforcement|classification|network",
          title, ignore.case = TRUE) ~ "Machine Learning",
    grepl("bayesian|statistical|inference|probability|variance|theorem",
          title, ignore.case = TRUE) ~ "Estadistica",
    TRUE ~ "Otros"
  )
}

# --- Mapeo completo de volúmenes JAIR (1-86) ---------------------------------
VOL_URL_MAP <- c(
  "1"  = "https://www.jair.org/index.php/jair/issue/view/1085",
  "2"  = "https://www.jair.org/index.php/jair/issue/view/1086",
  "3"  = "https://www.jair.org/index.php/jair/issue/view/1087",
  "4"  = "https://www.jair.org/index.php/jair/issue/view/1088",
  "5"  = "https://www.jair.org/index.php/jair/issue/view/1089",
  "6"  = "https://www.jair.org/index.php/jair/issue/view/1090",
  "7"  = "https://www.jair.org/index.php/jair/issue/view/1091",
  "8"  = "https://www.jair.org/index.php/jair/issue/view/1092",
  "9"  = "https://www.jair.org/index.php/jair/issue/view/1093",
  "10" = "https://www.jair.org/index.php/jair/issue/view/1094",
  "11" = "https://www.jair.org/index.php/jair/issue/view/1095",
  "12" = "https://www.jair.org/index.php/jair/issue/view/1096",
  "13" = "https://www.jair.org/index.php/jair/issue/view/1097",
  "14" = "https://www.jair.org/index.php/jair/issue/view/1098",
  "15" = "https://www.jair.org/index.php/jair/issue/view/1099",
  "16" = "https://www.jair.org/index.php/jair/issue/view/1100",
  "17" = "https://www.jair.org/index.php/jair/issue/view/1101",
  "18" = "https://www.jair.org/index.php/jair/issue/view/1102",
  "19" = "https://www.jair.org/index.php/jair/issue/view/1103",
  "20" = "https://www.jair.org/index.php/jair/issue/view/1104",
  "21" = "https://www.jair.org/index.php/jair/issue/view/1105",
  "22" = "https://www.jair.org/index.php/jair/issue/view/1106",
  "23" = "https://www.jair.org/index.php/jair/issue/view/1107",
  "24" = "https://www.jair.org/index.php/jair/issue/view/1108",
  "25" = "https://www.jair.org/index.php/jair/issue/view/1109",
  "26" = "https://www.jair.org/index.php/jair/issue/view/1110",
  "27" = "https://www.jair.org/index.php/jair/issue/view/1111",
  "28" = "https://www.jair.org/index.php/jair/issue/view/1112",
  "29" = "https://www.jair.org/index.php/jair/issue/view/1113",
  "30" = "https://www.jair.org/index.php/jair/issue/view/1114",
  "31" = "https://www.jair.org/index.php/jair/issue/view/1115",
  "32" = "https://www.jair.org/index.php/jair/issue/view/1116",
  "33" = "https://www.jair.org/index.php/jair/issue/view/1117",
  "34" = "https://www.jair.org/index.php/jair/issue/view/1118",
  "35" = "https://www.jair.org/index.php/jair/issue/view/1119",
  "36" = "https://www.jair.org/index.php/jair/issue/view/1120",
  "37" = "https://www.jair.org/index.php/jair/issue/view/1121",
  "38" = "https://www.jair.org/index.php/jair/issue/view/1122",
  "39" = "https://www.jair.org/index.php/jair/issue/view/1123",
  "40" = "https://www.jair.org/index.php/jair/issue/view/1124",
  "41" = "https://www.jair.org/index.php/jair/issue/view/1125",
  "42" = "https://www.jair.org/index.php/jair/issue/view/1126",
  "43" = "https://www.jair.org/index.php/jair/issue/view/1127",
  "44" = "https://www.jair.org/index.php/jair/issue/view/1128",
  "45" = "https://www.jair.org/index.php/jair/issue/view/1129",
  "46" = "https://www.jair.org/index.php/jair/issue/view/1130",
  "47" = "https://www.jair.org/index.php/jair/issue/view/1131",
  "48" = "https://www.jair.org/index.php/jair/issue/view/1132",
  "49" = "https://www.jair.org/index.php/jair/issue/view/1133",
  "50" = "https://www.jair.org/index.php/jair/issue/view/1134",
  "51" = "https://www.jair.org/index.php/jair/issue/view/1135",
  "52" = "https://www.jair.org/index.php/jair/issue/view/1136",
  "53" = "https://www.jair.org/index.php/jair/issue/view/1137",
  "54" = "https://www.jair.org/index.php/jair/issue/view/1138",
  "55" = "https://www.jair.org/index.php/jair/issue/view/1139",
  "56" = "https://www.jair.org/index.php/jair/issue/view/1140",
  "57" = "https://www.jair.org/index.php/jair/issue/view/1141",
  "58" = "https://www.jair.org/index.php/jair/issue/view/1142",
  "59" = "https://www.jair.org/index.php/jair/issue/view/1143",
  "60" = "https://www.jair.org/index.php/jair/issue/view/1144",
  "61" = "https://www.jair.org/index.php/jair/issue/view/1145",
  "62" = "https://www.jair.org/index.php/jair/issue/view/1150",
  "63" = "https://www.jair.org/index.php/jair/issue/view/1151",
  "64" = "https://www.jair.org/index.php/jair/issue/view/1152",
  "65" = "https://www.jair.org/index.php/jair/issue/view/1153",
  "66" = "https://www.jair.org/index.php/jair/issue/view/1154",
  "67" = "https://www.jair.org/index.php/jair/issue/view/1155",
  "68" = "https://www.jair.org/index.php/jair/issue/view/1156",
  "69" = "https://www.jair.org/index.php/jair/issue/view/1157",
  "70" = "https://www.jair.org/index.php/jair/issue/view/1158",
  "71" = "https://www.jair.org/index.php/jair/issue/view/1159",
  "72" = "https://www.jair.org/index.php/jair/issue/view/1161",
  "73" = "https://www.jair.org/index.php/jair/issue/view/1162",
  "74" = "https://www.jair.org/index.php/jair/issue/view/1163",
  "75" = "https://www.jair.org/index.php/jair/issue/view/1164",
  "76" = "https://www.jair.org/index.php/jair/issue/view/1165",
  "77" = "https://www.jair.org/index.php/jair/issue/view/1166",
  "78" = "https://www.jair.org/index.php/jair/issue/view/1167",
  "79" = "https://www.jair.org/index.php/jair/issue/view/1168",
  "80" = "https://www.jair.org/index.php/jair/issue/view/1169",
  "81" = "https://www.jair.org/index.php/jair/issue/view/1170",
  "82" = "https://www.jair.org/index.php/jair/issue/view/1171",
  "83" = "https://www.jair.org/index.php/jair/issue/view/1172",
  "84" = "https://www.jair.org/index.php/jair/issue/view/1173",
  "85" = "https://www.jair.org/index.php/jair/issue/view/1174",
  "86" = "https://www.jair.org/index.php/jair/issue/view/1175"
)

# --- Funciones de lectura de la DB -------------------------------------------
db_connect <- function() {
  dbConnect(RSQLite::SQLite(), DB_PATH)
}

load_papers <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  # JOIN para incluir autores concatenados desde las tablas normalizadas
  dbGetQuery(con, "
    SELECT p.*,
           COALESCE(
             (SELECT GROUP_CONCAT(a.author_name, ', ')
              FROM paper_authors pa
              JOIN authors a ON pa.author_id = a.author_id
              WHERE pa.paper_id = p.paper_id
              ORDER BY pa.author_order),
             'Sin autores'
           ) AS authors_raw
    FROM papers p
  ")
}

load_authors_with_papers <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  dbGetQuery(con, "
    SELECT a.author_id, a.author_name,
           COUNT(DISTINCT pa.paper_id) AS num_papers,
           ROUND(AVG(p.citations), 2) AS avg_citations,
           MAX(p.citations) AS max_citations
    FROM authors a
    JOIN paper_authors pa ON a.author_id = pa.author_id
    JOIN papers p ON pa.paper_id = p.paper_id
    GROUP BY a.author_id
    ORDER BY num_papers DESC
  ")
}

load_coauthorships <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  dbGetQuery(con, "
    SELECT a1.author_name AS Autor1, a2.author_name AS Autor2,
           COUNT(DISTINCT pa1.paper_id) AS num_coautorias
    FROM paper_authors pa1
    JOIN paper_authors pa2 ON pa1.paper_id = pa2.paper_id AND pa1.author_id < pa2.author_id
    JOIN authors a1 ON pa1.author_id = a1.author_id
    JOIN authors a2 ON pa2.author_id = a2.author_id
    GROUP BY pa1.author_id, pa2.author_id
    HAVING COUNT(DISTINCT pa1.paper_id) > 1
    ORDER BY num_coautorias DESC
    LIMIT 20
  ")
}

load_summary_stats <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  list(
    total_papers  = dbGetQuery(con, "SELECT COUNT(*) AS n FROM papers")$n,
    total_authors = dbGetQuery(con, "SELECT COUNT(*) AS n FROM authors")$n,
    avg_citations = dbGetQuery(con, "SELECT ROUND(AVG(citations),2) AS n FROM papers WHERE citations IS NOT NULL")$n,
    avg_authors   = dbGetQuery(con, "SELECT ROUND(AVG(cnt),2) AS n FROM (SELECT COUNT(*) AS cnt FROM paper_authors GROUP BY paper_id)")$n,
    avg_refs      = dbGetQuery(con, "SELECT ROUND(AVG(cnt),1) AS n FROM (SELECT COUNT(*) AS cnt FROM paper_reference_links GROUP BY paper_id)")$n
  )
}

# --- Paleta de colores (alto contraste, tema claro, no neón) -----------------
PALETTE <- c(
  "Machine Learning" = "#2563EB",
  "IA Generativa"    = "#DC2626",
  "Estadistica"      = "#059669",
  "Otros"            = "#7C3AED"
)

# ==============================================================================
# UI
# ==============================================================================
ui <- page_navbar(
  title = tags$span(
    tags$strong("JAIR Dashboard"),
    style = "font-size: 1.1em;"
  ),
  id = "main_nav",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2563EB",
    "navbar-bg" = "#1E3A5F"
  ),
  navbar_options = navbar_options(bg = "#1E3A5F"),

  # --- Pestaña 1: Resumen General ---
  nav_panel(
    title = "Resumen General",
    icon  = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Filtros",
        width = 260,
        selectInput("filter_topic", "Tema:",
                    choices = c("Todos", "Machine Learning", "IA Generativa", "Estadistica", "Otros"),
                    selected = "Todos"),
        sliderInput("filter_citations", "Rango de Citas:",
                    min = 0, max = 100, value = c(0, 100)),
        hr(),
        tags$small(
          icon("info-circle"), " Datos de JAIR — Journal of Artificial Intelligence Research",
          style = "color: #64748B; line-height: 1.5;"
        )
      ),
      # Value boxes — 5 indicadores requeridos
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(
          title = "Total Articulos",
          value = textOutput("vb_papers"),
          showcase = icon("file-alt"),
          theme = value_box_theme(bg = "#1E3A5F", fg = "#FFFFFF")
        ),
        value_box(
          title = "Autores Unicos",
          value = textOutput("vb_authors"),
          showcase = icon("users"),
          theme = value_box_theme(bg = "#2563EB", fg = "#FFFFFF")
        ),
        value_box(
          title = "Promedio de Citas",
          value = textOutput("vb_avg_cit"),
          showcase = icon("quote-right"),
          theme = value_box_theme(bg = "#059669", fg = "#FFFFFF")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        value_box(
          title = "Autores por Paper",
          value = textOutput("vb_avg_auth"),
          showcase = icon("user-friends"),
          theme = value_box_theme(bg = "#7C3AED", fg = "#FFFFFF")
        ),
        value_box(
          title = "Promedio de Referencias",
          value = textOutput("vb_avg_refs"),
          showcase = icon("book-open"),
          theme = value_box_theme(bg = "#B45309", fg = "#FFFFFF")
        )
      ),
      # Fila de gráficos principales
      layout_columns(
        col_widths = c(5, 7),
        card(
          full_screen = TRUE,
          card_header(
            tags$span(icon("chart-bar"), " Articulos por Tema"),
            class = "fw-semibold"
          ),
          plotlyOutput("plot_topic_dist", height = "380px")
        ),
        card(
          full_screen = TRUE,
          card_header(
            tags$span(icon("chart-area"), " Distribucion de Citas por Articulo"),
            class = "fw-semibold"
          ),
          plotlyOutput("plot_citations_hist", height = "380px")
        )
      ),
      # Tabla top citados
      layout_columns(
        col_widths = c(12),
        card(
          full_screen = TRUE,
          card_header(
            tags$span(icon("trophy"), " Top 10 Articulos mas Citados"),
            class = "fw-semibold"
          ),
          DTOutput("table_top_cited")
        )
      )
    )
  ),

  # --- Pestaña 2: Análisis de Autores ---
  nav_panel(
    title = "Autores",
    icon  = icon("users"),
    layout_columns(
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header(
          tags$span(icon("medal"), " Top 10 Autores mas Prolificos"),
          class = "fw-semibold"
        ),
        plotlyOutput("plot_top_authors", height = "480px")
      ),
      card(
        full_screen = TRUE,
        card_header(
          tags$span(icon("handshake"), " Pares con Coautoria Recurrente"),
          class = "fw-semibold"
        ),
        DTOutput("table_coauthors")
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        full_screen = TRUE,
        card_header(
          tags$span(icon("table"), " Directorio Completo de Autores"),
          class = "fw-semibold"
        ),
        DTOutput("table_all_authors")
      )
    )
  ),

  # --- Pestaña 3: Explorador de Artículos ---
  nav_panel(
    title = "Explorador",
    icon  = icon("search"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Busqueda Avanzada",
        width = 310,
        tags$p(tags$strong("Palabras clave / Titulo"),
               style = "margin-bottom:2px; font-size:0.85em; color:#475569;"),
        textInput("search_title", label = NULL,
                  placeholder = "Ej: reinforcement learning"),
        tags$p(tags$strong("Tema"),
               style = "margin-bottom:2px; font-size:0.85em; color:#475569;"),
        selectInput("search_topic", label = NULL,
                    choices = c("Todos", "Machine Learning", "IA Generativa", "Estadistica", "Otros"),
                    selected = "Todos"),
        tags$p(tags$strong("Autor"),
               style = "margin-bottom:2px; font-size:0.85em; color:#475569;"),
        textInput("search_author", label = NULL,
                  placeholder = "Ej: Yoshua Bengio"),
        tags$p(tags$strong("DOI"),
               style = "margin-bottom:2px; font-size:0.85em; color:#475569;"),
        textInput("search_doi", label = NULL,
                  placeholder = "Ej: 10.1613/jair..."),
        tags$p(tags$strong("Rango de Fechas"),
               style = "margin-bottom:2px; font-size:0.85em; color:#475569;"),
        dateRangeInput("search_dates", label = NULL,
                       start = "2020-01-01", end = Sys.Date(),
                       format = "yyyy-mm-dd", language = "es"),
        tags$p(tags$strong("Citas minimas"),
               style = "margin-bottom:2px; font-size:0.85em; color:#475569;"),
        numericInput("search_min_cit", label = NULL, value = 0, min = 0),
        hr(),
        actionButton("btn_search", label = tagList(icon("search"), " Buscar"),
                     class = "btn-primary w-100"),
        tags$br(),
        tags$small(icon("info-circle"), " Todos los filtros se aplican juntos.",
                   style = "color:#64748B;")
      ),
      card(
        full_screen = TRUE,
        card_header(
          tags$span(icon("list-alt"), " Resultados de la Busqueda"),
          class = "fw-semibold"
        ),
        DTOutput("table_explorer")
      )
    )
  ),

  # --- Pestaña 4: Actualización (Web Scraping) ---
  nav_panel(
    title = "Actualizar Datos",
    icon  = icon("sync"),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header(
          tagList(icon("sliders-h"), tags$strong(" Control de Actualizacion")),
          class = "bg-primary text-white"
        ),
        tags$p(
          "Seleccione el rango de volumenes de JAIR a incorporar en la base de datos.",
          style = "color:#475569; font-size:0.9em; margin-top:10px;"
        ),
        tags$div(
          style = "background:#f1f5f9; border-radius:8px; padding:10px 14px; margin-bottom:12px; font-size:0.85em; color:#334155;",
          icon("info-circle"),
          " Volumenes disponibles: 1 al 86. Los articulos ya existentes no se duplican."
        ),
        sliderInput("update_vol_range", "Rango de Volumenes:",
                    min = 1, max = 86, value = c(85, 86), step = 1),
        actionButton("btn_update",
                     label = tagList(icon("cloud-download-alt"), " Iniciar Actualizacion"),
                     class = "btn-primary w-100"),
        hr(),
        tags$div(
          style = "display:flex; align-items:center; gap:8px;",
          tags$strong("Estado:"),
          tags$span(textOutput("update_status", inline = TRUE),
                    style = "color:#2563EB; font-weight:600;")
        ),
        tags$br(),
        verbatimTextOutput("update_log")
      ),
      card(
        card_header(
          tagList(icon("database"), tags$strong(" Estado de la Base de Datos")),
          class = "fw-semibold"
        ),
        layout_columns(
          col_widths = c(6, 6),
          value_box(
            title = "Articulos en DB",
            value = textOutput("vb_db_papers"),
            showcase = icon("database"),
            theme = value_box_theme(bg = "#0F172A", fg = "#FFFFFF")
          ),
          value_box(
            title = "Autores en DB",
            value = textOutput("vb_db_authors"),
            showcase = icon("id-card"),
            theme = value_box_theme(bg = "#334155", fg = "#FFFFFF")
          )
        ),
        card(
          card_header("Composicion Actual por Tema"),
          plotlyOutput("plot_update_summary", height = "240px")
        ),
        conditionalPanel(
          condition = "output.show_last5 == true",
          card(
            card_header(
              tagList(icon("clock-rotate-left"), " Ultimos 5 Articulos Verificados"),
              class = "fw-semibold"
            ),
            DTOutput("table_last5")
          )
        )
      )
    )
  ),

  # --- Pestaña 5: Acerca de ---
  nav_panel(
    title = "Acerca de",
    icon  = icon("info-circle"),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header("Informacion del Proyecto"),
        tags$h4("Taller Parcial 2 - Mineria de Datos"),
        tags$p("Este tablero interactivo permite explorar y actualizar una base de datos
               de articulos cientificos extraidos del ",
               tags$a("Journal of Artificial Intelligence Research (JAIR)",
                      href = "https://www.jair.org/index.php/jair", target = "_blank"),
               "."),
        tags$h5("Funcionalidades:"),
        tags$ul(
          tags$li("Visualizacion de distribucion de articulos por tema y citaciones."),
          tags$li("Analisis de autores: ranking, coautorias."),
          tags$li("Explorador de articulos con filtros avanzados."),
          tags$li("Actualizacion en vivo via Web Scraping de nuevos volumenes de JAIR.")
        ),
        tags$h5("Tecnologias:"),
        tags$ul(
          tags$li("R + Shiny + bslib (Bootstrap 5)"),
          tags$li("SQLite (base de datos local)"),
          tags$li("rvest + httr (Web Scraping)"),
          tags$li("OpenAlex API (enriquecimiento de citas)"),
          tags$li("plotly + ggplot2 (visualizacion)")
        ),
        tags$hr(),
        tags$p(tags$em("Mateo Venegas Clavijo - CC. 1075878496"), style = "color: #64748B;"),
        tags$p(tags$em("Universidad Nacional de Colombia - 2026"), style = "color: #64748B;")
      ),
      card(
        card_header("Fuente de Datos"),
        tags$p(tags$strong("Revista:"), " Journal of Artificial Intelligence Research"),
        tags$p(tags$strong("Clasificacion:"), " Q1"),
        tags$p(tags$strong("Volumenes iniciales:"), " 82, 83, 84 (2025)"),
        tags$p(tags$strong("URL base:")),
        tags$a("https://www.jair.org/index.php/jair/issue/archive",
               href = "https://www.jair.org/index.php/jair/issue/archive",
               target = "_blank"),
        tags$hr(),
        tags$p(tags$strong("Base de datos:")),
        tags$code("revista_q1_2025.sqlite"),
        tags$p(tags$strong("Tablas:"), " papers, authors, paper_authors, article_references, paper_reference_links",
               style = "margin-top: 8px;")
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # --- Datos reactivos -------------------------------------------------------
  papers_data <- reactiveVal(NULL)
  stats_data  <- reactiveVal(NULL)

  # Carga inicial
  observe({
    papers_data(load_papers())
    stats_data(load_summary_stats())
  })

  # Datos filtrados (para Resumen General)
  filtered_papers <- reactive({
    df <- papers_data()
    req(df)

    if (input$filter_topic != "Todos") {
      df <- df |> filter(topic_label == input$filter_topic)
    }

    df <- df |> filter(
      is.na(citations) | (citations >= input$filter_citations[1] & citations <= input$filter_citations[2])
    )
    df
  })

  # Actualizar el slider de citas según los datos
  observe({
    df <- papers_data()
    req(df)
    max_cit <- max(df$citations, na.rm = TRUE)
    if (is.finite(max_cit)) {
      updateSliderInput(session, "filter_citations", max = max_cit, value = c(0, max_cit))
    }
  })

  # --- VALUE BOXES (Resumen) -------------------------------------------------
  output$vb_papers    <- renderText({ format(stats_data()$total_papers,  big.mark = ",") })
  output$vb_authors   <- renderText({ format(stats_data()$total_authors, big.mark = ",") })
  output$vb_avg_cit   <- renderText({ paste0(stats_data()$avg_citations, " citas") })
  output$vb_avg_auth  <- renderText({ paste0(stats_data()$avg_authors,   " autores") })
  output$vb_avg_refs  <- renderText({ paste0(stats_data()$avg_refs,      " refs.") })

  # --- GRÁFICO: Distribución por Tema ----------------------------------------
  output$plot_topic_dist <- renderPlotly({
    df <- filtered_papers()
    req(nrow(df) > 0)

    topic_counts <- df |>
      count(topic_label) |>
      arrange(n) |>
      mutate(topic_label = factor(topic_label, levels = topic_label),
             pct = round(n / sum(n) * 100, 1))

    p <- ggplot(topic_counts,
                aes(y = topic_label, x = n, fill = topic_label,
                    text = paste0("<b>", topic_label, "</b><br>",
                                  n, " articulos (", pct, "%)"))) +
      geom_col(width = 0.6) +
      geom_text(aes(label = paste0(n, "  (", pct, "%)")),
                hjust = -0.08, size = 3.8, color = "#1e293b", fontface = "bold") +
      scale_fill_manual(values = PALETTE, guide = "none") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
      labs(x = "Cantidad de Articulos", y = NULL) +
      theme_light(base_size = 13) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.y  = element_text(size = 12, color = "#1e293b"),
        axis.text.x  = element_text(size = 11, color = "#475569"),
        plot.margin  = margin(10, 20, 10, 10)
      )

    ggplotly(p, tooltip = "text") |>
      layout(margin = list(l = 10, r = 30, t = 10, b = 40)) |>
      config(displayModeBar = FALSE)
  })

  # --- GRÁFICO: Histograma de Citas ------------------------------------------
  output$plot_citations_hist <- renderPlotly({
    df <- filtered_papers() |> filter(!is.na(citations))
    req(nrow(df) > 0)

    media   <- mean(df$citations,   na.rm = TRUE)
    mediana <- median(df$citations, na.rm = TRUE)
    bw      <- max(1, round(diff(range(df$citations, na.rm = TRUE)) / 20))

    p <- ggplot(df, aes(x = citations)) +
      geom_histogram(binwidth = bw, fill = "#2563EB", color = "white",
                     alpha = 0.82) +
      geom_vline(xintercept = media,   linetype = "dashed",
                 color = "#DC2626", linewidth = 0.9) +
      geom_vline(xintercept = mediana, linetype = "dotted",
                 color = "#059669", linewidth = 0.9) +
      annotate("text", x = media   + bw * 0.6, y = Inf,
               label = paste0("Media: ", round(media, 1)),
               vjust = 2, hjust = 0, color = "#DC2626", size = 3.5, fontface = "bold") +
      annotate("text", x = mediana + bw * 0.6, y = Inf,
               label = paste0("Mediana: ", round(mediana, 1)),
               vjust = 4, hjust = 0, color = "#059669", size = 3.5, fontface = "bold") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      labs(x = "Numero de Citas", y = "Cantidad de Articulos") +
      theme_light(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#475569")
      )

    ggplotly(p, tooltip = c("x", "y")) |>
      layout(margin = list(l = 10, r = 10, t = 10, b = 40)) |>
      config(displayModeBar = FALSE)
  })

  # --- TABLA: Top 10 más citados ---------------------------------------------
  output$table_top_cited <- renderDT({
    df <- filtered_papers() |>
      filter(!is.na(citations)) |>
      arrange(desc(citations)) |>
      head(10) |>
      mutate(titulo_corto = str_trunc(title, 70)) |>
      select(titulo_corto, topic_label, citations, year, doi)

    datatable(
      df,
      colnames = c("Titulo", "Tema", "Citas", "Año", "DOI"),
      rownames  = FALSE,
      options   = list(
        pageLength = 10,
        dom        = "t",
        scrollX    = TRUE,
        columnDefs = list(
          list(width = "45%", targets = 0),
          list(width = "18%", targets = 1),
          list(className = "dt-center", targets = c(2, 3))
        )
      )
    ) |>
      formatStyle("citations",
                  background = styleColorBar(range(df$citations), "#93C5FD"),
                  backgroundSize   = "100% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |>
      formatStyle("topic_label",
                  color = styleEqual(
                    names(PALETTE),
                    unname(PALETTE)
                  ),
                  fontWeight = "bold")
  })

  # --- GRÁFICO: Top Autores --------------------------------------------------
  output$plot_top_authors <- renderPlotly({
    authors_df <- load_authors_with_papers() |>
      head(10) |>
      arrange(num_papers) |>
      mutate(author_name = factor(author_name, levels = author_name))

    req(nrow(authors_df) > 0)

    p <- ggplot(authors_df,
                aes(y = author_name, x = num_papers,
                    text = paste0("<b>", author_name, "</b><br>",
                                  num_papers, " papers<br>",
                                  "Prom. citas: ", avg_citations))) +
      geom_segment(aes(x = 0, xend = num_papers,
                       y = author_name, yend = author_name),
                   color = "#CBD5E1", linewidth = 1.2) +
      geom_point(aes(size = avg_citations), color = "#2563EB", alpha = 0.9) +
      geom_text(aes(label = num_papers), hjust = -0.6,
                size = 3.6, color = "#1e293b", fontface = "bold") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.25)),
                         breaks = scales::pretty_breaks(n = 5)) +
      scale_size_continuous(name = "Prom. Citas", range = c(4, 12)) +
      labs(x = "Numero de Papers", y = NULL) +
      theme_light(base_size = 13) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.y  = element_text(size = 11, color = "#1e293b"),
        axis.text.x  = element_text(size = 10, color = "#475569"),
        legend.position = "bottom",
        legend.title    = element_text(size = 10),
        plot.margin = margin(10, 20, 10, 10)
      )

    ggplotly(p, tooltip = "text") |>
      layout(margin = list(l = 10, r = 20, t = 10, b = 50),
             legend = list(orientation = "h", y = -0.15)) |>
      config(displayModeBar = FALSE)
  })

  # --- TABLA: Coautorías -----------------------------------------------------
  output$table_coauthors <- renderDT({
    coauth <- load_coauthorships()
    datatable(
      coauth,
      colnames  = c("Autor 1", "Autor 2", "Papers Juntos"),
      rownames  = FALSE,
      options   = list(
        pageLength = 10,
        dom        = "tip",
        scrollX    = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = 2)
        )
      )
    ) |>
      formatStyle("num_coautorias",
                  fontWeight = "bold", color = "#2563EB")
  })

  # --- TABLA: Todos los autores ----------------------------------------------
  output$table_all_authors <- renderDT({
    authors_df <- load_authors_with_papers() |>
      select(author_name, num_papers, avg_citations, max_citations)

    datatable(
      authors_df,
      colnames  = c("Nombre del Autor", "Nro. Papers", "Prom. Citas", "Max Citas"),
      rownames  = FALSE,
      filter    = "top",
      options   = list(
        pageLength = 15,
        dom        = "frtip",
        scrollX    = TRUE,
        scrollY    = "400px",
        columnDefs = list(
          list(className = "dt-center", targets = c(1, 2, 3))
        )
      )
    ) |>
      formatStyle("num_papers",
                  background = styleColorBar(c(0, max(authors_df$num_papers, na.rm=TRUE)), "#BFDBFE"),
                  backgroundSize     = "100% 80%",
                  backgroundRepeat   = "no-repeat",
                  backgroundPosition = "center")
  })

  # --- EXPLORADOR: Búsqueda -------------------------------------------------
  search_results <- eventReactive(input$btn_search, {
    df <- papers_data()
    req(df)

    # Filtro: título / palabras clave
    if (nchar(trimws(input$search_title)) > 0) {
      df <- df |> filter(grepl(input$search_title, title, ignore.case = TRUE))
    }
    # Filtro: tema
    if (input$search_topic != "Todos") {
      df <- df |> filter(topic_label == input$search_topic)
    }
    # Filtro: autor (sobre authors_raw)
    if (nchar(trimws(input$search_author)) > 0) {
      df <- df |> filter(grepl(input$search_author, authors_raw, ignore.case = TRUE))
    }
    # Filtro: DOI
    if (nchar(trimws(input$search_doi)) > 0) {
      df <- df |> filter(grepl(input$search_doi, doi, ignore.case = TRUE))
    }
    # Filtro: rango de fechas
    dates <- input$search_dates
    if (!is.null(dates) && !anyNA(dates)) {
      df <- df |>
        filter(!is.na(publication_date)) |>
        filter(as.Date(substr(publication_date, 1, 10)) >= dates[1] &
               as.Date(substr(publication_date, 1, 10)) <= dates[2])
    }
    # Filtro: citas mínimas
    df <- df |> filter(is.na(citations) | citations >= input$search_min_cit)

    df |> select(title, authors_raw, topic_label, citations, year, publication_date, doi)
  }, ignoreNULL = FALSE)

  output$table_explorer <- renderDT({
    df <- search_results()
    datatable(
      df,
      colnames  = c("Titulo", "Autores", "Tema", "Citas", "Año", "Fecha", "DOI"),
      rownames  = FALSE,
      options   = list(
        pageLength = 15,
        dom        = "frtip",
        scrollX    = TRUE,
        scrollY    = "480px",
        columnDefs = list(
          list(width = "30%", targets = 0),
          list(width = "22%", targets = 1),
          list(className = "dt-center", targets = c(3, 4, 5))
        )
      )
    ) |>
      formatStyle("topic_label",
                  color = styleEqual(names(PALETTE), unname(PALETTE)),
                  fontWeight = "bold") |>
      formatStyle("citations", fontWeight = "bold")
  })

  # --- ACTUALIZACIÓN: Web Scraping -------------------------------------------
  update_log_text    <- reactiveVal("Esperando instrucciones...")
  update_status_text <- reactiveVal("Listo")
  last5_data         <- reactiveVal(NULL)

  output$update_log    <- renderText({ update_log_text() })
  output$update_status <- renderText({ update_status_text() })

  output$table_last5 <- renderDT({
    df <- last5_data()
    req(df)
    datatable(
      df |> select(title, topic_label, citations, publication_date),
      colnames  = c("Titulo", "Tema", "Citas", "Fecha"),
      rownames  = FALSE,
      options   = list(dom = "t", pageLength = 5, scrollX = TRUE)
    ) |>
      formatStyle("topic_label",
                  color = styleEqual(names(PALETTE), unname(PALETTE)),
                  fontWeight = "bold")
  })

  # Expone si hay datos de últimos 5 para el conditionalPanel
  output$show_last5 <- reactive({ !is.null(last5_data()) })
  outputOptions(output, "show_last5", suspendWhenHidden = FALSE)

  # Value boxes de la pestaña de actualización
  output$vb_db_papers  <- renderText({ stats_data()$total_papers })
  output$vb_db_authors <- renderText({ stats_data()$total_authors })

  output$plot_update_summary <- renderPlotly({
    df <- papers_data()
    req(df)

    topic_counts <- df |>
      count(topic_label) |>
      arrange(n) |>
      mutate(topic_label = factor(topic_label, levels = topic_label),
             pct = round(n / sum(n) * 100, 1))

    p <- ggplot(topic_counts,
                aes(y = topic_label, x = n, fill = topic_label,
                    text = paste0(topic_label, ": ", n, " (", pct, "%)"))) +
      geom_col(width = 0.55) +
      geom_text(aes(label = paste0(n, " (", pct, "%)")),
                hjust = -0.1, size = 3.4, color = "#1e293b", fontface = "bold") +
      scale_fill_manual(values = PALETTE, guide = "none") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.28))) +
      labs(x = "Articulos", y = NULL) +
      theme_light(base_size = 12) +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor   = element_blank(),
            axis.text.y = element_text(size = 10))

    ggplotly(p, tooltip = "text") |>
      layout(margin = list(l = 5, r = 10, t = 5, b = 30)) |>
      config(displayModeBar = FALSE)
  })

  observeEvent(input$btn_update, {
    vol_range <- input$update_vol_range
    vols <- as.character(seq(vol_range[1], vol_range[2]))

    # Filtrar solo volúmenes que existen en el mapeo
    vols <- vols[vols %in% names(VOL_URL_MAP)]
    if (length(vols) == 0) {
      update_status_text("Error: Ningun volumen valido en el rango seleccionado.")
      update_log_text("No se encontraron volumenes validos para actualizar.")
      return()
    }

    vol_url_map <- VOL_URL_MAP

    update_status_text("En progreso...")
    log_lines <- c()

    withProgress(message = "Actualizando base de datos...", value = 0, {
      con <- db_connect()

      # Obtener URLs existentes para evitar duplicados
      existing_urls <- dbGetQuery(con, "SELECT url FROM papers")$url
      max_id <- dbGetQuery(con, "SELECT COALESCE(MAX(paper_id), 0) AS m FROM papers")$m

      all_new_papers <- tibble()

      for (vol in vols) {
        incProgress(0.1, detail = paste("Explorando volumen", vol))
        log_lines <- c(log_lines, paste0("[", Sys.time(), "] Explorando volumen ", vol, "..."))
        update_log_text(paste(log_lines, collapse = "\n"))

        vol_url <- vol_url_map[vol]
        if (is.na(vol_url)) {
          log_lines <- c(log_lines, paste0("  -> Volumen ", vol, " no mapeado. Saltando."))
          next
        }

        article_links <- scrape_volume_articles(vol_url)
        # Filtrar artículos que ya existen
        new_links <- setdiff(article_links, existing_urls)

        log_lines <- c(log_lines, paste0("  -> Encontrados: ", length(article_links),
                                         " | Nuevos: ", length(new_links)))
        update_log_text(paste(log_lines, collapse = "\n"))

        if (length(new_links) > 0) {
          for (j in seq_along(new_links)) {
            incProgress(0.8 / (length(vols) * max(length(new_links), 1)),
                        detail = paste0("Vol ", vol, ": articulo ", j, "/", length(new_links)))
            paper <- scrape_one_article(new_links[j])
            if (!is.null(paper)) {
              all_new_papers <- bind_rows(all_new_papers, paper)
            }
            Sys.sleep(0.5)
          }
        }
      }

      if (nrow(all_new_papers) > 0) {
        # Clasificar y asignar IDs
        all_new_papers <- all_new_papers |>
          mutate(
            topic_label = classify_topic(title),
            citation = paste0(authors_raw, " (", year, "). ", title,
                              ". Journal of Artificial Intelligence Research. ",
                              ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)),
            paper_id = max_id + row_number()
          )

        # Insertar papers
        papers_to_db <- all_new_papers |>
          select(paper_id, journal_name, title, publication_date, year, doi,
                 url, abstract, topic_label, citations, citation)
        dbWriteTable(con, "papers", papers_to_db, append = TRUE)

        # Insertar autores y relaciones
        for (i in seq_len(nrow(all_new_papers))) {
          autores <- strsplit(all_new_papers$authors_raw[i], ", ")[[1]]
          for (orden in seq_along(autores)) {
            autor <- trimws(autores[orden])
            if (autor == "" || is.na(autor)) next
            tryCatch(dbExecute(con, "INSERT INTO authors (author_name) VALUES (?)",
                               params = list(autor)), error = function(e) NULL)
            aid <- dbGetQuery(con, "SELECT author_id FROM authors WHERE author_name = ?",
                              params = list(autor))
            if (nrow(aid) > 0) {
              tryCatch(dbExecute(con,
                "INSERT INTO paper_authors (paper_id, author_id, author_order) VALUES (?, ?, ?)",
                params = list(all_new_papers$paper_id[i], aid$author_id[1], orden)),
                error = function(e) NULL)
            }
          }
        }

        log_lines <- c(log_lines, paste0("[", Sys.time(), "] ",
                                         nrow(all_new_papers), " articulos nuevos insertados en la base de datos."))

      } else {
        # Sin nuevos artículos: reconsultar los últimos 5 almacenados (requerimiento PDF 2.1.7)
        log_lines <- c(log_lines, paste0("[", Sys.time(), "] No se encontraron articulos nuevos."))
        log_lines <- c(log_lines, paste0("[", Sys.time(), "] Reconsultando los ultimos 5 articulos almacenados..."))
        update_log_text(paste(log_lines, collapse = "\n"))

        ultimos_5 <- dbGetQuery(con, "
          SELECT paper_id, title, publication_date, topic_label, citations, url
          FROM papers
          ORDER BY paper_id DESC
          LIMIT 5
        ")

        log_lines <- c(log_lines, "  --- Ultimos 5 articulos en la base de datos ---")
        for (k in seq_len(nrow(ultimos_5))) {
          log_lines <- c(log_lines, paste0(
            "  [", k, "] ", str_trunc(ultimos_5$title[k], 70),
            " | Tema: ", ultimos_5$topic_label[k],
            " | Citas: ", coalesce(as.character(ultimos_5$citations[k]), "N/A")
          ))
        }

        # Guardar para mostrar en UI
        last5_data(ultimos_5)
      }

      dbDisconnect(con)
      incProgress(0.1, detail = "Finalizando...")
    })

    # Refrescar datos reactivos
    papers_data(load_papers())
    stats_data(load_summary_stats())

    log_lines <- c(log_lines, paste0("[", Sys.time(), "] Actualizacion completada."))
    update_log_text(paste(log_lines, collapse = "\n"))
    update_status_text("Completado")
  })
}

# ==============================================================================
# Lanzar la aplicación
# ==============================================================================
shinyApp(ui = ui, server = server)
