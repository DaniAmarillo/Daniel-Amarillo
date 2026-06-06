
paquetes <- c( 
  "RSQLite","knitr","tidyverse","knitr","highcharter"
  ,"bslib","tidyverse","lubridate","DT","bsicons","plotly"

)
# Verificamos qué paquetes faltan
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  install.packages(pendientes)
}
source("actualizar_bd.r")
# Cargamos los paquetes sin mostrar mensajes
lapply(paquetes, library, character.only = TRUE)

#### carga de datos y normalización ####
con <- dbConnect(RSQLite::SQLite(), "JBA_25_26.sqlite")

for( i in dbListTables(con)){
  assign(i,dbReadTable(con, i))
}
papers <- papers |> mutate(issue = str_remove(issue,".*:"))

papers$year <- as.numeric(papers$year)
papers$topic_label <- as.factor(papers$topic_label)
papers$fecha_publicacion <- dmy(papers$fecha_publicacion)
papers$issue <- factor(papers$issue,c(" Issue 1 (Mar 2025)"," Issue 2 (Jun 2025)",
                                      " Issue 3 (Sep 2025)"," Issue 4 (Dec 2025)", 
                                      " Issue 1 (Mar 2026)"),ordered = TRUE)

library(shiny)
#### UI ####
ui <- fluidPage(page_navbar( 
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
###### pestaña general #####
    nav_panel(
              "Resumen",
              h3("Resumen"),
              layout_columns(
                col_widths = c(6,3,3),
                card(
                  highchartOutput("issues_gen"),
                    ),
                column(
                  width = 12,
                fluidRow(
                  wellPanel(value_box(
                    title = "referencias promedio",
                    value =  h3(textOutput(outputId = "mean_referencias")),
                    showcase = bs_icon("book",fill = "rgb(119,171,67) !important"),
                    p("por paper"),
                    styles = list(header = "font-size: 0.9rem; font-weight: bold;")
                  ))
                ), 
                fluidRow(wellPanel(value_box(
                  title = "# promedio de citas",
                  value =  h3(textOutput(outputId = "mean_citas")),
                  showcase = bs_icon("Flag",fill = "rgb(119,171,67) !important"),
                  p("por paper"),
                  styles = list(header = "font-size: 0.9rem; font-weight: bold;")
                )))),
                column(
                  width = 12,
                  fluidRow(
                    wellPanel(value_box(
                      title = "# de papers",
                      value =  h3(textOutput(outputId = "n_papers")),
                      showcase = bs_icon("file-text",fill = "rgb(119,171,67) !important"), 
                      p("publicados"),
                      styles = list(header = "font-size: 0.9rem; font-weight: bold;")
                    ))
                  ), 
                  fluidRow(wellPanel(value_box(
                    title = "FCWI promedio",
                    value =  h3(textOutput(outputId = "mean_fwci")),
                    showcase = bs_icon("graph-up",fill = "rgb(119,171,67) !important"),
                    p("impacto"),
                    styles = list(header = "font-size: 0.9rem; font-weight: bold;")
                  ))))
                            ),
              layout_columns( 
                card(highchartOutput("fcwi_issue")
                     ),
                ),
              DTOutput("top_5_citas")
              ), 
###### Papers ######
    nav_panel(title="Papers", 
                col_widths = c(8,2,2),nav_panel(
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
                      uiOutput("meta_sidebar")   # métricas rápidas bajo el selector
                    ),
                    
                    uiOutput("tarjeta_paper")    # tarjeta principal
                  )
                ),
              layout_columns(
                selectInput("paper_title","Paper:",choices = NULL),
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
              ),column(width = 12, 
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
                       ))
              )),
        nav_item( 
            a("Link revista", href = "https://www.akjournals.com/view/journals/2006/2006-overview.xml?contents=toc-30645", target = "_blank") 
        ) 
  ), 
  id = "tab" 

)


# Define server logic required to draw a histogram
server <- function(input, output,session) {
datos <- reactiveValues()
####cargar ####
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
##### base de datos filtrada
papers_filtrados <- reactive({
  req(nrow(datos$papers) > 0)
  df <- datos$papers
  
  # Filtro año
  if (length(input$filtro_year) > 0)
    df <- df |> filter(year %in% input$filtro_year)
  
  # Filtro categoría
  if (length(input$filtro_topic) > 0)
    df <- df |> filter(topic_label %in% input$filtro_topic)
  
  # Filtro autor (join con paper_authors)
  if (length(input$filtro_autor) > 0) {
    dois_autor <- datos$authors |>
      filter(autores %in% input$filtro_autor) |>
      left_join(datos$paper_authors, by = "id_autor") |>
      pull(doi)
    df <- df |> filter(doi %in% dois_autor)
  }
  
  # Filtro DOI (búsqueda parcial)
  if (nchar(trimws(input$filtro_doi)) > 0)
    df <- df |> filter(grepl(trimws(input$filtro_doi), doi,
                             ignore.case = TRUE))
  
  df
})

##### borrar filtros ####
# ── Limpiar filtros ───────────────────────────────────────────────────────
observeEvent(input$btn_limpiar, {
  updateSelectInput(session, "filtro_year",  selected = character(0))
  updateSelectInput(session, "filtro_topic", selected = character(0))
  updateSelectInput(session, "filtro_autor", selected = character(0))
  updateTextInput(session,   "filtro_doi",   value    = "")
})
papers_paper_info <- reactive({
  req(input$paper_title)
  
  papers|> filter(titulo == input$paper_title)
})

####observador selector topics####
observe({
  # Extraemos la columna de interés de nuestra tabla reactiva
  df <- papers_filtrados()
  opciones_nuevas <- append(as.factor("Todos"),unique(df$topic_label))
  
  # 5. Actualizamos el selector de la UI de forma eficiente
  updateSelectInput(session, "topic_selector",
                    choices = opciones_nuevas,
                    selected = head(opciones_nuevas, 1))
})
####observador selector AUTORES####
observe({
  # Extraemos la columna de interés de nuestra tabla reactiva
  req(datos$authors)
  opciones_nuevas <- unique(c("Todos",datos$authors$autores))
  
  # 5. Actualizamos el selector de la UI de forma eficiente
  updateSelectInput(session, "author_selector",
                    choices = opciones_nuevas,
                    selected = head(opciones_nuevas, 1))
})

#### número de papers en general ####
     output$n_papers <- renderText({
       papers_n <- papers_filtrados()
       print(nrow(papers_n))
       
     })
#### promedio referencias ####
     output$mean_referencias <- renderText({
       papers_n <- papers_filtrados()
       
       print(round(mean(papers_n$n_referencia),2))
     })
#### promedio fwci ####
     output$mean_fwci <- renderText({
       papers_n <- papers_filtrados()
       print(round(mean(papers_n$fwci),2))
     })
####numero promedio de citas ####     
     output$mean_citas <- renderText({
       papers_n <- papers_filtrados()
       
       print(round(mean(replace_na(papers_n$nro_de_citas, 0)),2))
       
       
     })

#### GRAFICA temas por issue ####
    output$issues_gen <- renderHighchart({
      
      # Build chart based on input$type
      papers_q1 <- papers_filtrados()
      
      q1 <- papers_q1 |>
            group_by(topic_label,year)|>
            summarize(temas = n())
      
      q1 |> hchart(type="column",
                   hcaes(x = year, y = temas, group = topic_label),
                   stacking = list(enabled = TRUE))|>
        hc_title(text = "<b>Gráfico de temas por issue</b>") |>
        hc_add_theme(hc_theme_538())
    
        
    })
#### Grafica scatter ####
    output$fcwi_issue <- renderHighchart({
      
      papers_q2 <- papers_filtrados()
      
      q2 <- papers_q2[c("fwci","n_referencia","n_autores","topic_label")]
      
      
      hchart(q2,type="bubble",hcaes(x= n_autores, y = n_referencia,z = fwci, group = topic_label)) |> 
        hc_tooltip(
          pointFormat = "
    <b>Número de autores: </b>{point.x}<br>
    <b>Número de Referencias: </b>{point.y}<br>
    <b>Relevancia: </b>{point.z:,.2f}"
        ) |>
        hc_title(text = paste("<b>Relevancia de papers publicados",min(papers_q2$year),"-",max(papers_q2$year),"</b>")) |>
        hc_add_theme(hc_theme_538())
      
      
      
      
      
      
      
    })
#### Tabla Dinámica ####
    output$top_5_citas <- renderDT({
      tbl_1 <-papers_filtrados()
      datatable(
        tbl_1[,-c(4,5,12)],
        options = list(pageLength = 5, searchHighlight = TRUE,
                       list(className = 'dt-center', targets = c(2,3,4,5,6))),
        filter = "top", # Adds column-specific search boxes
        rownames = FALSE,
        class = 'cell-border stripe compact'
      )
    })
####observador selector papers####

observe({
  # Extraemos la columna de interés de nuestra tabla reactiva
  df <- papers_filtrados()
  opciones_nuevas <- unique(df$titulo)
  
  # 5. Actualizamos el selector de la UI de forma eficiente
  updateSelectInput(session, "paper_title",
                    choices = opciones_nuevas,
                    selected = head(opciones_nuevas, 1))
})


#### velocimetro papers (fcwi) ####
output$gauge_papers_fcwi <- renderHighchart({
 
 papersg_fcwi <- papers_filtrados()
 
 df_gauge <- papersg_fcwi |> filter(titulo == input$paper_title)
 
  
 highchart() %>%
    hc_chart(type = "gauge", plotBackgroundColor = NULL, plotBackgroundImage = NULL, plotBorderWidth = 0, plotShadow = FALSE) %>%
    hc_title(text = "FCWI") %>%
    hc_pane(startAngle = -90, endAngle = 90,
            background = list(
              list(backgroundColor = "#FFF", borderWidth = 0, outerRadius = "109%", innerRadius = "107%")
            )) %>%
    hc_yAxis(min = min(papersg_fcwi$fwci), max = max(papersg_fcwi$fwci),
             minorTickInterval = "auto", minorTickWidth = 1, minorTickLength = 10, minorTickPosition = "inside", minorTickColor = "#666",
             tickPixelInterval = 30, tickWidth = 2, tickPosition = "inside", tickLength = 10, tickColor = "#666",
             labels = list(step = 2, rotation = "auto"),
             plotBands = list(
               list(from = min(papersg_fcwi$fwci), to = 10, color = "#DF5353"),    # Verde
               list(from = 10, to = max(papers$fwci)*0.8, color = "#DDDF0D"),   # Amarillo
               list(from = max(papersg_fcwi$fwci)*0.8, to = max(papersg_fcwi$fwci), color = "#55BF3B")   # Rojo
             )) %>%
    hc_add_series(name = "Valor", data = list(round(df_gauge$fwci,2)))
})
#### valocimetro papers (autores) ####
output$gauge_papers_auth <- renderHighchart({
  
  papersg_auth <- papers_filtrados()
  
  df_gauge <- papersg_auth |> filter(titulo == input$paper_title)
  
  
  highchart() %>%
    hc_chart(type = "gauge", plotBackgroundColor = NULL, plotBackgroundImage = NULL, plotBorderWidth = 0, plotShadow = FALSE) %>%
    hc_title(text = "Autores") %>%
    hc_pane(startAngle = -90, endAngle = 90,
            background = list(
              list(backgroundColor = "#FFF", borderWidth = 0, outerRadius = "109%", innerRadius = "107%")
            )) %>%
    hc_yAxis(min = min(papersg_auth$n_autores), max = max(papersg_auth$n_autores),
             minorTickInterval = "auto", minorTickWidth = 1, minorTickLength = 10, minorTickPosition = "inside", minorTickColor = "#666",
             tickPixelInterval = 30, tickWidth = 2, tickPosition = "inside", tickLength = 10, tickColor = "#666",
             labels = list(step = 2, rotation = "auto"),
             plotBands = list(
               list(from = min(papersg_auth$n_autores), to = 10, color = "#DF5353"),    # Verde
               list(from = 10, to = max(papers$n_autores)*0.8, color = "#DDDF0D"),   # Amarillo
               list(from = max(papersg_auth$n_autores)*0.8, to = max(papersg_auth$n_autores), color = "#55BF3B")   # Rojo
             )) %>%
    hc_add_series(name = "Valor", data = list(round(df_gauge$n_autores,2)),
                  tooltip = list(valueSuffix = " autores"))
})
#### valocimetro papers (autores) ####
output$gauge_papers_reff <- renderHighchart({
  
  papersg_reff <- papers_filtrados()
  
  df_gauge <- papersg_reff |> filter(titulo == input$paper_title)
  
  
  highchart() %>%
    hc_chart(type = "gauge", plotBackgroundColor = NULL, plotBackgroundImage = NULL, plotBorderWidth = 0, plotShadow = FALSE) %>%
    hc_title(text = "Referencias") %>%
    hc_pane(startAngle = -90, endAngle = 90,
            background = list(
              list(backgroundColor = "#FFF", borderWidth = 0, outerRadius = "109%", innerRadius = "107%")
            )) %>%
    hc_yAxis(min = min(papersg_reff$n_referencia), max = max(papersg_reff$n_referencia),
             minorTickInterval = "auto", minorTickWidth = 1, minorTickLength = 10, minorTickPosition = "inside", minorTickColor = "#666",
             tickPixelInterval = 30, tickWidth = 2, tickPosition = "inside", tickLength = 10, tickColor = "#666",
             labels = list(step = 2, rotation = "auto"),
             plotBands = list(
               list(from = min(papersg_reff$n_referencia), to = 10, color = "#DF5353"),    # Verde
               list(from = 10, to = max(papers$n_referencia)*0.8, color = "#DDDF0D"),   # Amarillo
               list(from = max(papersg_reff$n_referencia)*0.8, to = max(papersg_reff$n_referencia), color = "#55BF3B")   # Rojo
             )) %>%
    hc_add_series(name = "Valor", data = list(round(df_gauge$n_referencia,2)),
                  tooltip = list(valueSuffix = " referencias"))
})
#### velocimetro papers (citaciones) ####
output$gauge_papers_citas <- renderHighchart({
  
  papersg_reff <- papers_filtrados()
  
  df_gauge <- papersg_reff |> filter(titulo == input$paper_title)
  
  
  highchart() %>%
    hc_chart(type = "gauge", plotBackgroundColor = NULL, plotBackgroundImage = NULL, plotBorderWidth = 0, plotShadow = FALSE) %>%
    hc_title(text = "Citas") %>%
    hc_pane(startAngle = -90, endAngle = 90,
            background = list(
              list(backgroundColor = "#FFF", borderWidth = 0, outerRadius = "109%", innerRadius = "107%")
            )) %>%
    hc_yAxis(min = min(papersg_reff$nro_de_citas), max = max(papersg_reff$nro_de_citas),
             minorTickInterval = "auto", minorTickWidth = 1, minorTickLength = 10, minorTickPosition = "inside", minorTickColor = "#666",
             tickPixelInterval = 30, tickWidth = 2, tickPosition = "inside", tickLength = 10, tickColor = "#666",
             labels = list(step = 2, rotation = "auto"),
             plotBands = list(
               list(from = min(papersg_reff$nro_de_citas), to = 10, color = "#DF5353"),    # Verde
               list(from = 10, to = max(papers$nro_de_citas)*0.8, color = "#DDDF0D"),   # Amarillo
               list(from = max(papersg_reff$nro_de_citas)*0.8, to = max(papersg_reff$nro_de_citas), color = "#55BF3B")   # Rojo
             )) %>%
    hc_add_series(name = "Valor", data = list(round(df_gauge$nro_de_citas,2)),
                  tooltip = list(valueSuffix = " citaciones"))
})
#### Botón de actualizar ####
observeEvent(input$updt_btn, {
  btn1_df <- papers_filtrados() 
  updt <- actualizar_tabla(max(btn1_df$fecha_publicacion), authors, paper_authors,
                   references, paper_references)
  datos$papers <- papers |> rbind(updt[[1]])
  datos$paper_references <- updt[2]
  datos$references <- updt[3]
  datos$abstract <- rbind(updt[[4]])
  datos$paper_authors <- updt[5]
  datos$authors <- updt[6]
  print("LISTO!!")
  showNotification("Datos actualizados desde la base de datos", type = "message")
})
####resumen####
# Poblar selector con "Título — DOI"
observe({
  req(nrow(datos$papers) > 0)
  
  opciones <- setNames(
    datos$papers$doi,
    paste0(datos$papers$titulo)
  )
  
  updateSelectizeInput(session, "selector_paper",
                       choices  = opciones,
                       server   = TRUE   # carga bajo demanda, eficiente con muchos papers
  )
})

# Paper seleccionado
paper_sel <- reactive({
  req(input$selector_paper)
  datos$papers |> filter(doi == input$selector_paper)
})

# Autores del paper seleccionado
autores_sel <- reactive({
  req(input$selector_paper)
  datos$paper_authors |>
    filter(doi == input$selector_paper) |>
    left_join(datos$authors, by = "id_autor") |>
    pull(autores)
})

# Abstract del paper seleccionado
abstract_sel <- reactive({
  req(input$selector_paper)
  datos$abstract |> filter(doi == input$selector_paper)
})

# ── Métricas rápidas en sidebar ───────────────────────────────────────────────
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

# ── Tarjeta principal ─────────────────────────────────────────────────────────
output$tarjeta_paper <- renderUI({
  req(paper_sel(), autores_sel(), abstract_sel())
  
  p  <- paper_sel()
  ab <- abstract_sel()
  
  bslib::card(
    full_screen = TRUE,
    
    # ── Encabezado ────────────────────────────────────────────────────────────
    card_header(
      class = "bg-primary text-white",
      tags$h5(class = "mb-0", p$titulo)
    ),
    
    card_body(
      # Badges de metadata
      div(
        class = "d-flex flex-wrap gap-2 mb-3",
        span(class = "badge bg-secondary",              p$year),
        span(class = "badge bg-info",                   p$topic_label),
        span(class = "badge bg-light text-dark border", p$journal_name)
      ),
      
      # Autores
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
      
      # DOI con enlace
      tags$p(
        tags$strong(icon("link"), " DOI: "),
        tags$a(href = p$url, target = "_blank", p$doi,
               class = "text-decoration-none")
      ),
      
      hr(),
      
      # Abstract completo
      tags$p(tags$strong(icon("align-left"), " Abstract")),
      tags$p(class = "text-muted fst-italic small", ab$resumen),
      
      hr(),
      
      # Secciones estructuradas (si existen)
      bslib::layout_columns(
        col_widths = c(6, 6),
        
        bslib::card(
          class = "border-start border-3 border-info",
          card_body(
            class = "py-2",
            tags$p(tags$strong("Background")),
            tags$p(class = "small text-muted",
                   ab$Background %||% "—")
          )
        ),
        
        bslib::card(
          class = "border-start border-3 border-success",
          card_body(
            class = "py-2",
            tags$p(tags$strong("Métodos")),
            tags$p(class = "small text-muted",
                   ab$Metodos %||% "—")
          )
        ),
        
        bslib::card(
          class = "border-start border-3 border-warning",
          card_body(
            class = "py-2",
            tags$p(tags$strong("Resultados")),
            tags$p(class = "small text-muted",
                   ab$Resultados %||% "—")
          )
        ),
        
        bslib::card(
          class = "border-start border-3 border-danger",
          card_body(
            class = "py-2",
            tags$p(tags$strong("Conclusión")),
            tags$p(class = "small text-muted",
                   ab$Conclusion %||% "—")
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
}

# Run the application 
shinyApp(ui = ui, server = server)


