

library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(dplyr)
library(DT)
library(plotly)
library(lubridate)
library(reticulate)
library(shinyWidgets)
source("./utils/functions.R")

def_path <- "./"
setwd(def_path)
python_bin <- ifelse(
  file.exists("C:/Users/sergi/anaconda3/envs/backend/python.exe"),
  "C:/Users/sergi/anaconda3/envs/backend/python.exe",          # local Windows
  Sys.which("python3")             # servidor Linux
)
# Instalar requirements solo en el servidor (no en tu máquina local)
if (!file.exists("C:/Users/sergi/anaconda3/envs/backend/python.exe")) {
  req_path <- file.path(getwd(), "requirements.txt")
  if (file.exists(req_path)) {
    system2(python_bin, args = c("-m", "pip", "install", "-r", req_path, "--quiet"))
  }
}

DB_PATH <- "./revista_q1_2025.sqlite"

palette <- list(
  verde    = "#94B43B",
  verde_dk = "#6a8029",
  bg       = "#F7F8F3",
  card     = "#FFFFFF",
  txt      = "#1C2410",
  txt2     = "#5A6845",
  borde    = "#D9E4C0",
  ml       = "#94B43B",
  ia       = "#3B8FA4",
  est      = "#E07B39",
  otros    = "#A0A0A0"
)

topic_colors <- c(
  "Machine Learning" = palette$ml,
  "IA Generativa"    = palette$ia,
  "Estadística"      = palette$est,
  "Otros"            = palette$otros,
  "no se pudo clasificar" = "#CCCCCC"
)

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════
ui <- fluidPage(
  tags$head(
    includeCSS("www/styles.css"),
    tags$link(rel = "icon", type = "image/png", href = "logo.png")
  ),
  
  div(class = "header-bar",
      img(src = "logo.png"),
      span(class = "header-title", "Dashboard Analítico · Revistas Q1"),
      span(class = "header-badge", "Minería de Datos · 2016325")
  ),
  
  div(class = "main-wrap",
      div(class = "sidebar-panel",
          
          div(class = "sidebar-section",
              div(class = "sidebar-label", "Rango de fechas"),
              airDatepickerInput("first_date",
                                 range = FALSE,
                                 label = NULL,
                                 placeholder = "desde",
              ),
              airDatepickerInput("second_date",
                                 range = FALSE,
                                 label = NULL,
                                 placeholder = "hasta",
              )
          ),
          
          div(class = "sidebar-section",
              div(class = "sidebar-label", "Temática"),
              selectizeInput("topic_filter", NULL,
                             choices  = c("Todas" = "",
                                          "Machine Learning", "IA Generativa",
                                          "Estadística", "Otros",
                                          "no se pudo clasificar"),
                             selected = "",
                             options  = list(placeholder = "Todas las temáticas")
              )
          ),
          
          div(class = "sidebar-section",
              div(class = "sidebar-label", "Autor"),
              textInput("author_filter", NULL, placeholder = "Nombre o apellido…")
          ),
          
          div(class = "sidebar-section",
              div(class = "sidebar-label", "DOI"),
              textInput("doi_filter", NULL, placeholder = "10.xxxx/…")
          ),
          
          div(class = "sidebar-section",
              div(class = "sidebar-label", "Título / Palabra clave"),
              textInput("keyword_filter", NULL, placeholder = "Buscar en título…")
          ),
          div(class = "sidebar-section",
              div(class = "sidebar-label", "escala temporal"),
              radioButtons(
                "temporal_scale", 
                label = NULL,
                choices = c("Año" = "year", "Mes-Año" = "month", "Día" = "day"),
                selected = "year",
                inline = TRUE
              )),
          
          div(style = "margin-top: auto; padding-top: 12px;",
              actionButton("reset_filters", "↺  Limpiar filtros",
                           class = "btn-reset", style = "width:100%;")
          )
      ),
      div(class = "content-panel",
          div(class = "section-title", "Indicadores generales"),
          div(class = "kpi-row",
              div(class = "kpi-card",
                  span(class = "kpi-icon", "📄"),
                  div(class = "kpi-value", textOutput("kpi_total", inline = TRUE)),
                  div(class = "kpi-label", "Artículos")
              ),
              div(class = "kpi-card",
                  span(class = "kpi-icon", "👥"),
                  div(class = "kpi-value", textOutput("kpi_avg_authors", inline = TRUE)),
                  div(class = "kpi-label", "Prom. autores")
              ),
              div(class = "kpi-card",
                  span(class = "kpi-icon", "📣"),
                  div(class = "kpi-value", textOutput("kpi_avg_citas", inline = TRUE)),
                  div(class = "kpi-label", "Prom. citas")
              ),
              div(class = "kpi-card",
                  span(class = "kpi-icon", "📚"),
                  div(class = "kpi-value", textOutput("kpi_avg_refs", inline = TRUE)),
                  div(class = "kpi-label", "Prom. referencias")
              ),
              div(class = "kpi-card",
                  span(class = "kpi-icon", "⬇️"),
                  div(class = "kpi-value", textOutput("kpi_avg_dl", inline = TRUE)),
                  div(class = "kpi-label", "Prom. descargas")
              ),
              div(class = "kpi-card",
                  span(class = "kpi-icon", "🏷"),
                  div(class = "kpi-value", textOutput("kpi_temas", inline = TRUE)),
                  div(class = "kpi-label", "Temáticas")
              )
          ),
          div(class = "section-title", "Artículos destacados"),
          div(class = "highlight-row",
              div(class = "highlight-card",
                  div(class = "highlight-tag", "🏆 Más citado"),
                  div(class = "highlight-title", textOutput("top_cited_title")),
                  div(class = "highlight-meta", textOutput("top_cited_meta"))
              ),
              div(class = "highlight-card",
                  div(class = "highlight-tag", "⬇️ Más descargado"),
                  div(class = "highlight-title", textOutput("top_dl_title")),
                  div(class = "highlight-meta", textOutput("top_dl_meta"))
              )
          ),
          div(class = "section-title", "Visualizaciones"),
          div(class = "charts-row",
              div(class = "chart-card",
                  plotlyOutput("chart_temporal", height = "260px")
              ),
              div(class = "chart-card",
                  plotlyOutput("chart_temas", height = "260px")
              )
          ),
          div(class = "charts-row",
              div(class = "chart-card",
                  plotlyOutput("chart_top_autores", height = "260px")
              ),
              div(class = "chart-card",
                  plotlyOutput("chart_citas_dist", height = "260px")
              )
          ),
          
          div(class = "section-title", "Tabla de artículos"),
          div(class = "table-card",
              DTOutput("tabla_papers")
          ),
          
          
          div(class = "section-title", "Actualización de datos"),
          div(class = "scraping-card",
              p(class = "scraping-desc",
                "Lanza el job de scraping en un proceso paralelo (via reticulate + callr). ",
                "Si existen artículos nuevos se almacenarán automáticamente en la BD SQLite. ",
                "En caso contrario, no actualiza."
              ),
              div(style = "display:flex; align-items:center; gap:10px; flex-wrap:wrap;",
                  actionButton("btn_scrape", "▶  Buscar artículos nuevos",
                               class = "btn-scrape"),
                  actionButton("btn_refresh", "↺  Recargar datos",
                               class = "btn-reset"),
                  conditionalPanel(
                    condition = "input.btn_scrape > 0",
                    tags$small(style = "color:#999; font-family:'DM Mono',monospace;",
                               "El proceso puede tardar varios minutos…")
                  )
              ),
              uiOutput("scrape_result_ui")
          )
      )
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  refresh_trigger <- reactiveVal(0)
  scrape_status   <- reactiveVal(NULL)   # list(type, msg)
  
  raw_data <- reactive({
    refresh_trigger()
    load_papers(DB_PATH)
  })
  
  observe({if (!is.null(input$first_date)) {
    updateAirDateInput(
      session = session,
      inputId = "second_date",
      options = list(minDate = input$first_date)
    )
  }})
  
  filtered_data <- reactive({
    df <- raw_data()
    if (nrow(df) == 0) return(df)
    
    df$publication_date <- as.Date(df$publication_date)
    fecha_desde <- input$"first_date"
    fecha_hasta <- input$"second_date"
    
    if (!is.null(fecha_desde) && !is.null(fecha_hasta)) {
      df <- df[!is.na(df$publication_date) &
                 df$publication_date >= as.Date(fecha_desde) &
                 df$publication_date <= as.Date(fecha_hasta), ]
    }
    
    # Temática
    if (!is.null(input$topic_filter) && nchar(input$topic_filter) > 0)
      df <- df[df$topic_label == input$topic_filter, ]
    
    # Autor
    if (nchar(trimws(input$author_filter)) > 0)
      df <- df[grepl(input$author_filter, df$authors, ignore.case = TRUE), ]
    
    # DOI
    if (nchar(trimws(input$doi_filter)) > 0)
      df <- df[grepl(input$doi_filter, df$doi, ignore.case = TRUE), ]
    
    # Keyword / título
    if (nchar(trimws(input$keyword_filter)) > 0)
      df <- df[grepl(input$keyword_filter, df$title, ignore.case = TRUE), ]
    
    df
  })
  
  
  observeEvent(input$reset_filters, {
    updateAirDateInput(session, "first_date",
                       value = "2020-01-01")
    updateAirDateInput(session, "second_date",
                       value = Sys.Date())
    updateSelectizeInput(session, "topic_filter", selected = "")
    updateTextInput(session, "author_filter",  value = "")
    updateTextInput(session, "doi_filter",     value = "")
    updateTextInput(session, "keyword_filter", value = "")
  })
  
  
  observeEvent(input$btn_refresh, {
    refresh_trigger(refresh_trigger() + 1)
    showNotification("Datos recargados desde la BD.", type = "message", duration = 3)
  })
  
  
  observeEvent(input$btn_scrape, {
    scrape_status(list(type = "info", msg = "⏳  Ejecutando scraping en segundo plano…"))
    
    tryCatch({
      system2(
        python_bin,
        "utils/scraping.py",
        stdout = "",
        stderr = ""
      )
      nuevos <- load_updated_links(DB_PATH)
      scrape_status(list(
        type = "ok",
        msg = glue::glue("Job ejecutado. Se tienen {nuevos} articulos nuevos. Haz clic en 'Recargar datos'")
      ))
      
    }, error = function(e) {
      scrape_status(list(
        type = "err",
        msg  = paste("❌  Error al lanzar el job:", conditionMessage(e))
      ))
    })
  })
  
  output$scrape_result_ui <- renderUI({
    s <- scrape_status()
    if (is.null(s)) return(NULL)
    css_class <- switch(s$type, ok = "scrape-ok", warn = "scrape-warn", "scrape-err")
    div(class = paste("scrape-result", css_class), s$msg)
  })
  
  output$kpi_total <- renderText({
    format(nrow(filtered_data()), big.mark = ",")
  })
  output$kpi_avg_authors <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("—")
    sprintf("%.1f", mean(df$n_authors, na.rm = TRUE))
  })
  output$kpi_avg_citas <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("—")
    sprintf("%.1f", mean(df$citations, na.rm = TRUE))
  })
  output$kpi_avg_refs <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("—")
    sprintf("%.1f", mean(df$n_references, na.rm = TRUE))
  })
  output$kpi_avg_dl <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("—")
    format(round(mean(df$downloads, na.rm = TRUE)), big.mark = ",")
  })
  output$kpi_temas <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("0")
    length(unique(df$topic_label[nchar(df$topic_label) > 0]))
  })
  
  output$top_cited_title <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("Sin datos")
    df$title[which.max(df$citations)]
  })
  output$top_cited_meta <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("")
    idx <- which.max(df$citations)
    paste0(df$citations[idx], " citas · ", df$publication_date[idx])
  })
  output$top_dl_title <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("Sin datos")
    df$title[which.max(df$downloads)]
  })
  output$top_dl_meta <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("")
    idx <- which.max(df$downloads)
    paste0(format(df$downloads[idx], big.mark = ","),
           " descargas · ", df$publication_date[idx])
  })
  
  output$chart_temporal <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(plot_ly() %>%
               layout(title = list(text = "Sin datos", font = list(size = 13)),
                      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"))
    }
    
    scale <- input$temporal_scale
    df$fecha <- as.Date(df$publication_date)
    
    # Agrupar y etiquetar según escala
    by_time <- switch(scale,
                      "year" = df %>%
                        mutate(periodo = year(fecha)) %>%
                        group_by(periodo) %>%
                        summarise(n = n(), .groups = "drop") %>%
                        mutate(label = as.character(periodo)),
                      
                      "month" = df %>%
                        mutate(periodo = floor_date(fecha, "month")) %>%
                        group_by(periodo) %>%
                        summarise(n = n(), .groups = "drop") %>%
                        mutate(label = format(periodo, "%b %Y")),
                      
                      "day" = df %>%
                        mutate(periodo = fecha) %>%
                        group_by(periodo) %>%
                        summarise(n = n(), .groups = "drop") %>%
                        mutate(label = format(periodo, "%d %b %Y"))
    ) %>% arrange(periodo)
    
    # Título dinámico
    titulo <- switch(scale,
                     "year"  = "Publicaciones por año",
                     "month" = "Publicaciones por mes",
                     "day"   = "Publicaciones por día"
    )
    
    # Hover dinámico
    hover <- switch(scale,
                    "year"  = "<b>%{text}</b><br>%{y} artículos<extra></extra>",
                    "month" = "<b>%{text}</b><br>%{y} artículos<extra></extra>",
                    "day"   = "<b>%{text}</b><br>%{y} artículos<extra></extra>"
    )
    
    plot_ly(by_time,
            x = ~periodo, y = ~n, text = ~label,
            type = "scatter", mode = "lines+markers",
            line   = list(color = palette$verde, width = 2.5),
            marker = list(color = palette$verde, size = 7,
                          line = list(color = "#fff", width = 1.5)),
            hovertemplate = hover
    ) %>%
      layout(
        title  = list(text = titulo, font = list(size = 13, color = palette$txt2)),
        xaxis  = list(title = "", showgrid = FALSE, zeroline = FALSE,
                      # Formato del eje X según escala
                      tickformat = switch(scale,
                                          "year"  = "%Y",
                                          "month" = "%b %Y",
                                          "day"   = "%d %b %Y"
                      )),
        yaxis  = list(title = "", gridcolor = "#E8EFD5", zeroline = FALSE),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        margin = list(t = 35, b = 30, l = 30, r = 10),
        font   = list(family = "DM Sans", color = palette$txt2)
      )
  })
  
  output$chart_temas <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(plot_ly() %>%
               layout(title = list(text = "Sin datos", font = list(size = 13)),
                      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"))
    }
    
    by_topic <- df %>%
      group_by(topic_label) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(desc(n))
    
    colors <- sapply(by_topic$topic_label,
                     function(t) ifelse(t %in% names(topic_colors), topic_colors[t], palette$otros))
    
    plot_ly(by_topic,
            x = ~n, y = ~reorder(topic_label, n),
            type = "bar", orientation = "h",
            marker = list(color = colors, line = list(color = "rgba(0,0,0,0)")),
            hovertemplate = "<b>%{y}</b><br>%{x} artículos<extra></extra>"
    ) %>%
      layout(
        title  = list(text = "Artículos por temática", font = list(size = 13, color = palette$txt2)),
        xaxis  = list(title = "", gridcolor = "#E8EFD5", zeroline = FALSE),
        yaxis  = list(title = "", showgrid = FALSE),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        margin = list(t = 35, b = 30, l = 130, r = 10),
        font   = list(family = "DM Sans", color = palette$txt2)
      )
  })
  
  output$chart_top_autores <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0 || all(is.na(df$authors) | df$authors == "")) {
      return(plot_ly() %>%
               layout(title = list(text = "Sin datos", font = list(size = 13)),
                      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"))
    }
    
    autores <- unlist(strsplit(df$authors, ","))
    autores <- trimws(autores[nchar(trimws(autores)) > 0])
    top <- sort(table(autores), decreasing = TRUE)[1:min(10, length(table(autores)))]
    top_df <- data.frame(
      autor = names(top),
      n     = as.integer(top),
      stringsAsFactors = FALSE
    )
    
    plot_ly(top_df,
            x = ~n, y = ~reorder(autor, n),
            type = "bar", orientation = "h",
            marker = list(
              color = colorRampPalette(c("#D9E4C0", palette$verde_dk))(nrow(top_df)),
              line  = list(color = "rgba(0,0,0,0)")
            ),
            hovertemplate = "<b>%{y}</b><br>%{x} artículos<extra></extra>"
    ) %>%
      layout(
        title  = list(text = "Top 10 autores", font = list(size = 13, color = palette$txt2)),
        xaxis  = list(title = "", gridcolor = "#E8EFD5", zeroline = FALSE),
        yaxis  = list(title = "", showgrid = FALSE),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        margin = list(t = 35, b = 30, l = 130, r = 10),
        font   = list(family = "DM Sans", color = palette$txt2)
      )
  })
  
  output$chart_citas_dist <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(plot_ly() %>%
               layout(title = list(text = "Sin datos", font = list(size = 13)),
                      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"))
    }
    
    plot_ly(df,
            x = ~citations,
            type = "histogram",
            nbinsx = 30,
            marker = list(
              color = palette$ia,
              line  = list(color = "rgba(0,0,0,0)")
            ),
            hovertemplate = "%{x} citas · %{y} arts<extra></extra>"
    ) %>%
      layout(
        title  = list(text = "Distribución de citas", font = list(size = 13, color = palette$txt2)),
        xaxis  = list(title = "Citas", gridcolor = "#E8EFD5", zeroline = FALSE),
        yaxis  = list(title = "", gridcolor = "#E8EFD5", zeroline = FALSE),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        bargap = 0.05,
        margin = list(t = 35, b = 40, l = 30, r = 10),
        font   = list(family = "DM Sans", color = palette$txt2)
      )
  })
  
  output$tabla_papers <- renderDT({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(datatable(
        data.frame(Mensaje = "No hay datos con los filtros actuales."),
        options = list(dom = "t"), rownames = FALSE
      ))
    }
    
    display <- df %>%
      mutate(
        doi_link = ifelse(
          !is.na(doi) & nchar(doi) > 0,
          paste0('<a href="https://doi.org/', doi,
                 '" target="_blank" style="color:var(--verde-dk)">',
                 doi, '</a>'),
          "—"
        ),
        title_short = ifelse(nchar(title) > 90,
                             paste0(substr(title, 1, 90), "…"), title),
        pub_date = as.character(publication_date)
      ) %>%
      select(
        Título       = title_short,
        Autores      = authors,
        Fecha        = pub_date,
        Temática     = topic_label,
        DOI          = doi_link,
        Citas        = citations,
        Descargas    = downloads
      )
    
    datatable(
      display,
      escape    = FALSE,
      rownames  = FALSE,
      selection = "single",
      filter    = "top",
      options   = list(
        pageLength    = 10,
        scrollX       = TRUE,
        dom           = "lftip",
        language      = list(
          url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json"
        ),
        columnDefs    = list(
          list(width = "35%", targets = 0),
          list(width = "25%", targets = 1),
          list(className = "dt-center", targets = c(2, 5, 6))
        )
      )
    ) %>%
      formatStyle("Temática",
                  backgroundColor = styleEqual(
                    names(topic_colors),
                    sapply(topic_colors, function(c) paste0(c, "22"))
                  ),
                  color = styleEqual(
                    names(topic_colors), topic_colors
                  ),
                  fontWeight = "600",
                  fontSize   = "0.72rem"
      ) %>%
      formatStyle(c("Citas", "Descargas"),
                  fontFamily = "DM Mono, monospace",
                  textAlign  = "center"
      )
  })
}

shinyApp(ui = ui, server = server)