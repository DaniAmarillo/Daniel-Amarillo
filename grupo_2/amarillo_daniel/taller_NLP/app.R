#### Taller 4 - Minería de Datos ####
#### Aplicación Shiny ampliada con buscador de artículos ####

paquetes <- c( 
  "RSQLite","knitr","tidyverse","knitr","highcharter",
  "bslib","lubridate","DT","bsicons","plotly","shiny"
)

library(R.utils)
library(rvest)
library(bsicons)
library(DT)
library(lubridate)
library(shiny)
library(plotly)
library(bslib)
library(highcharter)
library(readr)
library(knitr)
library(tidyverse)
library(RSQLite)
library(openalexR)
library(stringr)
library(xml2)
library(httr2)
library(purrr)
library(tibble)
library(janitor)

# Cargar funciones de búsqueda (usa objetos en caché)
source("03_funciones_busqueda.R")

# Script para actualizar la base de datos
source("actualizar_bd.R")
source("02_corpus_representacion.R")
#### Carga de datos y normalización ####
con <- dbConnect(RSQLite::SQLite(), "JBA_25_26.sqlite")

for (i in dbListTables(con)) {
  assign(i, dbReadTable(con, i))
}
papers <- papers |> mutate(issue = str_remove(issue, ".*:"))

papers$year <- as.numeric(papers$year)
papers$topic_label <- as.factor(papers$topic_label)
papers$fecha_publicacion <- dmy(papers$fecha_publicacion)
papers$issue <- factor(papers$issue, c(" Issue 1 (Mar 2025)", " Issue 2 (Jun 2025)",
                                       " Issue 3 (Sep 2025)", " Issue 4 (Dec 2025)", 
                                       " Issue 1 (Mar 2026)"), ordered = TRUE)

#### UI ####
ui <- fluidPage(
  page_navbar(
    sidebar = sidebar(
      selectInput("filtro_year", "Año",
                  choices  = NULL,
                  multiple = TRUE,
                  selected = NULL),
      selectInput("filtro_topic", "Categoría",
                  choices  = NULL,
                  multiple = TRUE,
                  selected = NULL),
      selectInput("filtro_autor", "Autor",
                  choices  = NULL,
                  multiple = TRUE,
                  selected = NULL),
      textInput("filtro_doi", "DOI",
                placeholder = "Buscar por DOI..."),
      actionButton("btn_limpiar", "Limpiar filtros",
                   icon  = icon("eraser"),
                   class = "btn-warning btn-sm",
                   width = "100%"),
      hr(),
      actionButton("updt_btn", "Buscar nuevas publicaciones",
                   icon  = icon("rotate"),
                   class = "btn-primary btn-sm",
                   width = "100%"),
    ),
    # ---- Pestaña Resumen ----
    nav_panel(
      "Resumen",
      h3("Resumen"),
      layout_columns(
        col_widths = c(6, 3, 3),
        card(highchartOutput("issues_gen")),
        column(
          width = 12,
          fluidRow(
            wellPanel(value_box(
              title = "referencias promedio",
              value =  h3(textOutput(outputId = "mean_referencias")),
              showcase = bs_icon("book", fill = "rgb(119,171,67) !important"),
              p("por paper"),
              styles = list(header = "font-size: 0.9rem; font-weight: bold;")
            ))
          ),
          fluidRow(
            wellPanel(value_box(
              title = "# promedio de citas",
              value =  h3(textOutput(outputId = "mean_citas")),
              showcase = bs_icon("Flag", fill = "rgb(119,171,67) !important"),
              p("por paper"),
              styles = list(header = "font-size: 0.9rem; font-weight: bold;")
            ))
          )
        ),
        column(
          width = 12,
          fluidRow(
            wellPanel(value_box(
              title = "# de papers",
              value =  h3(textOutput(outputId = "n_papers")),
              showcase = bs_icon("file-text", fill = "rgb(119,171,67) !important"),
              p("publicados"),
              styles = list(header = "font-size: 0.9rem; font-weight: bold;")
            ))
          ),
          fluidRow(
            wellPanel(value_box(
              title = "FCWI promedio",
              value =  h3(textOutput(outputId = "mean_fwci")),
              showcase = bs_icon("graph-up", fill = "rgb(119,171,67) !important"),
              p("impacto"),
              styles = list(header = "font-size: 0.9rem; font-weight: bold;")
            ))
          )
        )
      ),
      layout_columns(
        card(highchartOutput("fcwi_issue"))
      ),
      DTOutput("top_5_citas")
    ),
    # ---- Pestaña Papers ----
    nav_panel(
      title = "Papers",
      col_widths = c(8, 2, 2),
      nav_panel(
        title = "Detalle de Paper",
        icon  = icon("file-alt"),
        layout_sidebar(
          sidebar = sidebar(
            width = 320,
            title = "Buscar paper",
            selectizeInput(
              "selector_paper",
              label       = "Buscar por título o DOI",
              choices     = NULL,
              multiple    = FALSE,
              options     = list(
                placeholder = "Escribe para buscar...",
                maxOptions  = 50
              )
            ),
            hr(),
            uiOutput("meta_sidebar")
          ),
          uiOutput("tarjeta_paper")
        )
      ),
      layout_columns(
        selectInput("paper_title", "Paper:", choices = NULL),
        column(
          width = 12,
          fluidRow(
            wellPanel(value_box(
              title = "",
              value =  highchartOutput("gauge_papers_fcwi"),
              styles = list(header = "font-size: 0.9rem; font-weight: bold;")
            ))
          ),
          fluidRow(
            wellPanel(value_box(
              title = "",
              value =  highchartOutput("gauge_papers_auth"),
              styles = list(header = "font-size: 0.9rem; font-weight: bold;")
            ))
          )
        ),
        column(
          width = 12,
          fluidRow(
            wellPanel(value_box(
              title = "",
              value =  highchartOutput("gauge_papers_reff"),
              styles = list(header = "font-size: 0.9rem; font-weight: bold;")
            ))
          ),
          fluidRow(
            wellPanel(value_box(
              title = "",
              value =  highchartOutput("gauge_papers_citas"),
              styles = list(header = "font-size: 0.9rem; font-weight: bold;")
            ))
          )
        )
      )
    ),
    # ---- Pestaña Búsqueda (nueva) ----
    nav_panel(
      "Búsqueda",
      br(),
      fluidRow(
        column(6,
               textInput("consulta", "Escribe tu consulta en lenguaje natural",
                         placeholder = "Ej. aplicaciones de inteligencia artificial en diagnóstico",
                         width = "100%")
        ),
        column(3,
               selectInput("estrategia", "Estrategia de recuperación",
                           choices = c("TF‑IDF (léxica)" = "tfidf",
                                       "LSA (semántica reducida)" = "lsa",
                                       "Ambas" = "ambas"),
                           selected = "tfidf")
        ),
        column(3,
               selectInput("n_resultados", "Número de resultados",
                           choices = c(5, 10, 20, 50), selected = 10)
        )
      ),
      actionButton("buscar_btn", "Buscar", icon = icon("search"), class = "btn-primary"),
      br(), br(),
      uiOutput("resultados_ui")
    ),
    nav_item(
      a("Link revista", href = "https://www.akjournals.com/view/journals/2006/2006-overview.xml?contents=toc-30645", target = "_blank")
    )
  ),
  id = "tab"
)

#### Server ####
server <- function(input, output, session) {
  datos <- reactiveValues()
  
  #### Carga inicial de datos ####
  observe({
    datos$papers           <- papers
    datos$authors          <- authors
    datos$paper_authors    <- paper_authors
    datos$references       <- references
    datos$paper_references <- paper_references
    datos$abstract         <- abstract
  })
  
  observe({
    req(nrow(datos$papers) > 0)
    updateSelectInput(session, "filtro_year",
                      choices = sort(unique(datos$papers$year), decreasing = TRUE))
    updateSelectInput(session, "filtro_topic",
                      choices = sort(unique(datos$papers$topic_label)))
    updateSelectInput(session, "filtro_autor",
                      choices = sort(unique(datos$authors$autores)))
  })
  
  #### Filtros ####
  papers_filtrados <- reactive({
    req(nrow(datos$papers) > 0)
    df <- datos$papers
    
    if (length(input$filtro_year) > 0)
      df <- df |> filter(year %in% input$filtro_year)
    
    if (length(input$filtro_topic) > 0)
      df <- df |> filter(topic_label %in% input$filtro_topic)
    
    if (length(input$filtro_autor) > 0) {
      dois_autor <- datos$authors |>
        filter(autores %in% input$filtro_autor) |>
        left_join(datos$paper_authors, by = "id_autor") |>
        pull(doi)
      df <- df |> filter(doi %in% dois_autor)
    }
    
    if (nchar(trimws(input$filtro_doi)) > 0)
      df <- df |> filter(grepl(trimws(input$filtro_doi), doi, ignore.case = TRUE))
    
    df
  })
  
  observeEvent(input$btn_limpiar, {
    updateSelectInput(session, "filtro_year",  selected = character(0))
    updateSelectInput(session, "filtro_topic", selected = character(0))
    updateSelectInput(session, "filtro_autor", selected = character(0))
    updateTextInput(session,   "filtro_doi",   value    = "")
  })
  
  #### Outputs de Resumen ####
  output$n_papers <- renderText({
    nrow(papers_filtrados())
  })
  
  output$mean_referencias <- renderText({
    round(mean(papers_filtrados()$n_referencia), 2)
  })
  
  output$mean_fwci <- renderText({
    round(mean(papers_filtrados()$fwci), 2)
  })
  
  output$mean_citas <- renderText({
    round(mean(replace_na(papers_filtrados()$nro_de_citas, 0)), 2)
  })
  
  output$issues_gen <- renderHighchart({
    papers_q1 <- papers_filtrados()
    q1 <- papers_q1 |>
      group_by(topic_label, year) |>
      summarize(temas = n())
    q1 |> hchart(type = "column",
                 hcaes(x = year, y = temas, group = topic_label),
                 stacking = list(enabled = TRUE)) |>
      hc_title(text = "<b>Gráfico de temas por issue</b>") |>
      hc_add_theme(hc_theme_538())
  })
  
  output$fcwi_issue <- renderHighchart({
    papers_q2 <- papers_filtrados()
    q2 <- papers_q2[c("fwci", "n_referencia", "n_autores", "topic_label")]
    hchart(q2, type = "bubble",
           hcaes(x = n_autores, y = n_referencia, z = fwci, group = topic_label)) |>
      hc_tooltip(
        pointFormat = "
          <b>Número de autores: </b>{point.x}<br>
          <b>Número de Referencias: </b>{point.y}<br>
          <b>Relevancia: </b>{point.z:,.2f}"
      ) |>
      hc_title(text = paste("<b>Relevancia de papers publicados",
                            min(papers_q2$year), "-", max(papers_q2$year), "</b>")) |>
      hc_add_theme(hc_theme_538())
  })
  
  output$top_5_citas <- renderDT({
    tbl_1 <- papers_filtrados()
    datatable(
      tbl_1[, -c(4, 5, 12)],
      options = list(pageLength = 5, searchHighlight = TRUE,
                     list(className = 'dt-center', targets = c(2, 3, 4, 5, 6))),
      filter = "top",
      rownames = FALSE,
      class = 'cell-border stripe compact'
    )
  })
  
  #### Selector de paper (pestaña Papers) ####
  observe({
    df <- papers_filtrados()
    opciones_nuevas <- unique(df$titulo)
    updateSelectInput(session, "paper_title",
                      choices = opciones_nuevas,
                      selected = head(opciones_nuevas, 1))
  })
  
  # Gauges
  output$gauge_papers_fcwi <- renderHighchart({
    papersg_fcwi <- papers_filtrados()
    df_gauge <- papersg_fcwi |> filter(titulo == input$paper_title)
    highchart() %>%
      hc_chart(type = "gauge", plotBackgroundColor = NULL, plotBackgroundImage = NULL,
               plotBorderWidth = 0, plotShadow = FALSE) %>%
      hc_title(text = "FCWI") %>%
      hc_pane(startAngle = -90, endAngle = 90,
              background = list(
                list(backgroundColor = "#FFF", borderWidth = 0, outerRadius = "109%", innerRadius = "107%")
              )) %>%
      hc_yAxis(min = min(papersg_fcwi$fwci), max = max(papersg_fcwi$fwci),
               minorTickInterval = "auto", minorTickWidth = 1, minorTickLength = 10,
               minorTickPosition = "inside", minorTickColor = "#666",
               tickPixelInterval = 30, tickWidth = 2, tickPosition = "inside",
               tickLength = 10, tickColor = "#666",
               labels = list(step = 2, rotation = "auto"),
               plotBands = list(
                 list(from = min(papersg_fcwi$fwci), to = 10, color = "#DF5353"),
                 list(from = 10, to = max(papers$fwci) * 0.8, color = "#DDDF0D"),
                 list(from = max(papersg_fcwi$fwci) * 0.8, to = max(papersg_fcwi$fwci), color = "#55BF3B")
               )) %>%
      hc_add_series(name = "Valor", data = list(round(df_gauge$fwci, 2)))
  })
  
  output$gauge_papers_auth <- renderHighchart({
    papersg_auth <- papers_filtrados()
    df_gauge <- papersg_auth |> filter(titulo == input$paper_title)
    highchart() %>%
      hc_chart(type = "gauge", plotBackgroundColor = NULL, plotBackgroundImage = NULL,
               plotBorderWidth = 0, plotShadow = FALSE) %>%
      hc_title(text = "Autores") %>%
      hc_pane(startAngle = -90, endAngle = 90,
              background = list(
                list(backgroundColor = "#FFF", borderWidth = 0, outerRadius = "109%", innerRadius = "107%")
              )) %>%
      hc_yAxis(min = min(papersg_auth$n_autores), max = max(papersg_auth$n_autores),
               minorTickInterval = "auto", minorTickWidth = 1, minorTickLength = 10,
               minorTickPosition = "inside", minorTickColor = "#666",
               tickPixelInterval = 30, tickWidth = 2, tickPosition = "inside",
               tickLength = 10, tickColor = "#666",
               labels = list(step = 2, rotation = "auto"),
               plotBands = list(
                 list(from = min(papersg_auth$n_autores), to = 10, color = "#DF5353"),
                 list(from = 10, to = max(papers$n_autores) * 0.8, color = "#DDDF0D"),
                 list(from = max(papersg_auth$n_autores) * 0.8, to = max(papersg_auth$n_autores), color = "#55BF3B")
               )) %>%
      hc_add_series(name = "Valor", data = list(round(df_gauge$n_autores, 2)),
                    tooltip = list(valueSuffix = " autores"))
  })
  
  output$gauge_papers_reff <- renderHighchart({
    papersg_reff <- papers_filtrados()
    df_gauge <- papersg_reff |> filter(titulo == input$paper_title)
    highchart() %>%
      hc_chart(type = "gauge", plotBackgroundColor = NULL, plotBackgroundImage = NULL,
               plotBorderWidth = 0, plotShadow = FALSE) %>%
      hc_title(text = "Referencias") %>%
      hc_pane(startAngle = -90, endAngle = 90,
              background = list(
                list(backgroundColor = "#FFF", borderWidth = 0, outerRadius = "109%", innerRadius = "107%")
              )) %>%
      hc_yAxis(min = min(papersg_reff$n_referencia), max = max(papersg_reff$n_referencia),
               minorTickInterval = "auto", minorTickWidth = 1, minorTickLength = 10,
               minorTickPosition = "inside", minorTickColor = "#666",
               tickPixelInterval = 30, tickWidth = 2, tickPosition = "inside",
               tickLength = 10, tickColor = "#666",
               labels = list(step = 2, rotation = "auto"),
               plotBands = list(
                 list(from = min(papersg_reff$n_referencia), to = 10, color = "#DF5353"),
                 list(from = 10, to = max(papers$n_referencia) * 0.8, color = "#DDDF0D"),
                 list(from = max(papersg_reff$n_referencia) * 0.8, to = max(papersg_reff$n_referencia), color = "#55BF3B")
               )) %>%
      hc_add_series(name = "Valor", data = list(round(df_gauge$n_referencia, 2)),
                    tooltip = list(valueSuffix = " referencias"))
  })
  
  output$gauge_papers_citas <- renderHighchart({
    papersg_reff <- papers_filtrados()
    df_gauge <- papersg_reff |> filter(titulo == input$paper_title)
    highchart() %>%
      hc_chart(type = "gauge", plotBackgroundColor = NULL, plotBackgroundImage = NULL,
               plotBorderWidth = 0, plotShadow = FALSE) %>%
      hc_title(text = "Citas") %>%
      hc_pane(startAngle = -90, endAngle = 90,
              background = list(
                list(backgroundColor = "#FFF", borderWidth = 0, outerRadius = "109%", innerRadius = "107%")
              )) %>%
      hc_yAxis(min = min(papersg_reff$nro_de_citas), max = max(papersg_reff$nro_de_citas),
               minorTickInterval = "auto", minorTickWidth = 1, minorTickLength = 10,
               minorTickPosition = "inside", minorTickColor = "#666",
               tickPixelInterval = 30, tickWidth = 2, tickPosition = "inside",
               tickLength = 10, tickColor = "#666",
               labels = list(step = 2, rotation = "auto"),
               plotBands = list(
                 list(from = min(papersg_reff$nro_de_citas), to = 10, color = "#DF5353"),
                 list(from = 10, to = max(papers$nro_de_citas) * 0.8, color = "#DDDF0D"),
                 list(from = max(papersg_reff$nro_de_citas) * 0.8, to = max(papersg_reff$nro_de_citas), color = "#55BF3B")
               )) %>%
      hc_add_series(name = "Valor", data = list(round(df_gauge$nro_de_citas, 2)),
                    tooltip = list(valueSuffix = " citaciones"))
  })
  
  #### Botón actualizar ####
  observeEvent(input$updt_btn, {
    btn1_df <- papers_filtrados()
    updt <- withProgress(
      message = 'Procesando datos...',
      detail = 'Por favor, espere un momento...',
      value = NULL, # Al ser NULL, se convierte en una barra animada infinita (spinner)
      {   # Step 1
        actualizar_tabla(max(btn1_df$fecha_publicacion), authors, paper_authors,
                         references, paper_references)
        incProgress(0.3, detail = "Reading database...")
        Sys.sleep(1)
        
        # Step 2
        incProgress(0.4, detail = "Cleaning data rows...")
        Sys.sleep(1)
        
        # Step 3
        incProgress(0.3, detail = "Generating charts...")
        Sys.sleep(1)
 })
    datos$papers <- papers |> rbind(updt[[1]])
    datos$paper_references <- paper_references |> rbind(updt[[2]])
    datos$references <- references |> rbind(updt[[3]])
    datos$abstract <- abstract |>  rbind(updt[[4]])
    datos$paper_authors <- paper_authors |> rbind(updt[[5]])
    datos$authors <- authors |> rbind(updt[[6]])
    source("02_corpus_representacion.R")
    showNotification("Datos actualizados desde la base de datos", type = "message")
  })
  
  #### Pestaña Papers - detalle ####
  observe({
    req(nrow(datos$papers) > 0)
    opciones <- setNames(
      datos$papers$doi,
      paste0(datos$papers$titulo)
    )
    updateSelectizeInput(session, "selector_paper",
                         choices  = opciones,
                         server   = TRUE)
  })
  
  paper_sel <- reactive({
    req(input$selector_paper)
    datos$papers |> filter(doi == input$selector_paper)
  })
  
  autores_sel <- reactive({
    req(input$selector_paper)
    datos$paper_authors |>
      filter(doi == input$selector_paper) |>
      left_join(datos$authors, by = "id_autor") |>
      pull(autores)
  })
  
  abstract_sel <- reactive({
    req(input$selector_paper)
    datos$abstract |> filter(doi == input$selector_paper)
  })
  
  output$meta_sidebar <- renderUI({
    req(paper_sel())
    p <- paper_sel()
    tagList(
      tags$small(class = "text-muted", "Año: ",        tags$b(p$year)),            br(),
      tags$small(class = "text-muted", "Citas: ",      tags$b(p$nro_de_citas)),    br(),
      tags$small(class = "text-muted", "FWCI: ",       tags$b(round(p$fwci, 2))),  br(),
      tags$small(class = "text-muted", "Referencias: ", tags$b(p$n_referencia))
    )
  })
  
  output$tarjeta_paper <- renderUI({
    req(paper_sel(), autores_sel(), abstract_sel())
    p  <- paper_sel()
    ab <- abstract_sel()
    bslib::card(
      full_screen = TRUE,
      card_header(
        class = "bg-primary text-white",
        tags$h5(class = "mb-0", p$titulo)
      ),
      card_body(
        div(
          class = "d-flex flex-wrap gap-2 mb-3",
          span(class = "badge bg-secondary",              p$year),
          span(class = "badge bg-info",                   p$topic_label),
          span(class = "badge bg-light text-dark border", p$journal_name)
        ),
        bslib::card(
          class = "mb-3 border-0 bg-light",
          card_body(
            class = "py-2",
            tags$p(
              tags$strong(icon("users"), " Autores"),
              br(),
              tags$span(class = "text-muted",
                        paste(autores_sel(), collapse = " · "))
            )
          )
        ),
        tags$p(
          tags$strong(icon("link"), " DOI: "),
          tags$a(href = p$url, target = "_blank", p$doi,
                 class = "text-decoration-none")
        ),
        hr(),
        tags$p(tags$strong(icon("align-left"), " Abstract")),
        tags$p(class = "text-muted fst-italic small", ab$resumen),
        hr(),
        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::card(
            class = "border-start border-3 border-info",
            card_body(
              class = "py-2",
              tags$p(tags$strong("Background")),
              tags$p(class = "small text-muted", ab$Background %||% "—")
            )
          ),
          bslib::card(
            class = "border-start border-3 border-success",
            card_body(
              class = "py-2",
              tags$p(tags$strong("Métodos")),
              tags$p(class = "small text-muted", ab$Metodos %||% "—")
            )
          ),
          bslib::card(
            class = "border-start border-3 border-warning",
            card_body(
              class = "py-2",
              tags$p(tags$strong("Resultados")),
              tags$p(class = "small text-muted", ab$Resultados %||% "—")
            )
          ),
          bslib::card(
            class = "border-start border-3 border-danger",
            card_body(
              class = "py-2",
              tags$p(tags$strong("Conclusión")),
              tags$p(class = "small text-muted", ab$Conclusion %||% "—")
            )
          )
        )
      ),
      card_footer(
        class = "text-muted small",
        icon("calendar"), " Publicado: ", p$fecha_publicacion,
        "  |  ",
        icon("quote-right"), " Citas: ", p$nro_de_citas
      )
    )
  })
  
  resultados_busqueda <- reactiveValues(tfidf = NULL, lsa = NULL)
  
  # Función auxiliar para enriquecer resultados con autores, fecha, resumen
  enriquecer_resultados <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    df %>%
      left_join(datos$papers %>% select(doi, fecha_publicacion, url), by = "doi") %>%
      left_join(datos$abstract %>% select(doi, resumen), by = "doi") %>%
      rename( doi_res = doi)  %>% 
      rowwise() %>%
      mutate(
        puntaje = round(puntaje,3),
        autores = {
          ids <- datos$paper_authors %>% filter(doi_res == doi) %>% pull(id_autor)
          if (length(ids) > 0) {
            paste(datos$authors %>% filter(id_autor %in% ids) %>% pull(autores) %>% unique(), collapse = ", ")
          } else { NA_character_ }
        },
        fragmento = texto_completo
      ) %>%
      ungroup() %>%
      rename( doi = doi_res)  %>% 
      select(posicion, titulo, autores, fecha_publicacion, topic_label, doi, puntaje,, fragmento, url)
  }
  
  # Ejecutar búsqueda al presionar el botón
  observeEvent(input$buscar_btn, {
    req(input$consulta, nchar(trimws(input$consulta)) > 0)
    n <- as.integer(input$n_resultados)
    
    if (input$estrategia %in% c("tfidf", "ambas")) {
      res <- buscar_tfidf(input$consulta, n)
      resultados_busqueda$tfidf <- enriquecer_resultados(res)
    } else {
      resultados_busqueda$tfidf <- NULL
    }
    
    if (input$estrategia %in% c("lsa", "ambas")) {
      res <- buscar_lsa(input$consulta, n)
      resultados_busqueda$lsa <- enriquecer_resultados(res)
    } else {
      resultados_busqueda$lsa <- NULL
    }
  })
  
  # Renderizado condicional de las tablas
  output$resultados_ui <- renderUI({
    if (input$estrategia == "ambas") {
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("TF‑IDF (léxica)"),
          DTOutput("tabla_tfidf")
        ),
        card(
          card_header("LSA (semántica reducida)"),
          DTOutput("tabla_lsa")
        )
      )
    } else if (input$estrategia == "tfidf") {
      card(
        card_header("Resultados TF‑IDF"),
        DTOutput("tabla_tfidf")
      )
    } else { # lsa
      card(
        card_header("Resultados LSA"),
        DTOutput("tabla_lsa")
      )
    }
  })
  
  output$tabla_tfidf <- renderDT({
    req(resultados_busqueda$tfidf)
    df <- resultados_busqueda$tfidf
    df$doi <- paste0('<a href="', df$url, '" target="_blank">', df$doi, '</a>')
    datatable(df %>% select(-url),
              escape = FALSE,
              options = list(pageLength = nrow(df), dom = 't', scrollX = TRUE,    columnDefs = list(list(
                targets = c(2, 7), # ÍNDICES de las columnas que quieres truncar (¡Empiezan en 0!)
                render = JS(
                  "function(data, type, row) {",
                  "  if (type === 'display' && data !== null && data.length > 200) {",
                  "    return '<span title=\"' + data + '\">' + data.substr(0, 200) + '...</span>';",
                  "  }",
                  "  return data;",
                  "}"
                )
              ))
              ),
              rownames = FALSE)
  })
  
  output$tabla_lsa <- renderDT({
    req(resultados_busqueda$lsa)
    df <- resultados_busqueda$lsa
    df$doi <- paste0('<a href="', df$url, '" target="_blank">', df$doi, '</a>')
    datatable(df %>% select(-url),
              escape = FALSE,
              options = list(pageLength = nrow(df), dom = 't', scrollX = TRUE,
                             columnDefs = list(list(
                               targets = c(2, 7), 
                               render = JS(
                                 "function(data, type, row) {",
                                 "  if (type === 'display' && data !== null && data.length > 200) {",
                                 "    return '<span title=\"' + data + '\">' + data.substr(0, 200) + '...</span>';",
                                 "  }",
                                 "  return data;",
                                 "}"
                               )
                             ))
              ),
              rownames = FALSE)
  })
}

#### Ejecutar aplicación ####
shinyApp(ui = ui, server = server)