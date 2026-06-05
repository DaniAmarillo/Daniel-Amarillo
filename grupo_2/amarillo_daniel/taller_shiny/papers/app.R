
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
    title = "Controles Generales",
    sliderInput("year", "Año:", min = min(papers$year), max = max(papers$year), value = c(2024,2026)),
    actionButton(inputId = "mi_boton", label = "¡Haz clic aquí!")
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
              h3("Información de papers"),
              selectInput("paper_title","Paper:",choices = NULL),
              layout_columns(
                col_widths = c(4,4,4),
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
###### Autores #####
    nav_panel("Autores", "Page C content"), 
###### Referencias #####
    nav_panel("Referencias", "Page C content"),
    nav_menu( 
        "Other links", 
        nav_panel("D", "Panel D content"), 
        "----", 
        "Description:", 
        nav_item( 
            a("Link revista", href = "https://www.akjournals.com/view/journals/2006/2006-overview.xml?contents=toc-30645", target = "_blank") 
        ), 
    ), 
  ), 
  id = "tab" 

)


#### server ####
# Define server logic required to draw a histogram
server <- function(input, output,session) {
####  filtro años ####
papers_year_gen <- reactive({
  req(input$year)
  
  filter(papers,between(year,input$year[1],input$year[2]))
  })

papers_paper_info <- reactive({
  req(input$paper_title)
  
  papers|> filter(titulo == input$paper_title)
})

#### numero de papers en general ####
     output$n_papers <- renderText({
       papers_n <- papers_year_gen()
       print(nrow(papers_n))
       
     })
#### promedio referencias ####
     output$mean_referencias <- renderText({
       papers_n <- papers_year_gen()
       
       print(round(mean(papers_n$n_referencia),2))
     })
#### promedio fwci ####
     output$mean_fwci <- renderText({
       papers_n <- papers_year_gen()
       print(round(mean(papers_n$fwci),2))
     })
####numero promedio de citas ####     
     output$mean_citas <- renderText({
       papers_n <- papers_year_gen()
       
       print(round(mean(replace_na(papers_n$nro_de_citas, 0)),2))
       
       
     })

#### temas por issue ####
    output$issues_gen <- renderHighchart({
      
      # Build chart based on input$type
      papers_q1 <- papers_year_gen()
      
      q1 <- papers_q1 |>
            group_by(topic_label,issue)|>
            summarize(temas = n())
      
      q1 |> hchart(type="column",
                   hcaes(x = issue, y = temas, group = topic_label),
                   stacking = list(enabled = TRUE))|>
        hc_title(text = "<b>Gráfico de temas por issue</b>") |>
        hc_add_theme(hc_theme_538())
    
        
    })
#### Grafica scatter ####
    output$fcwi_issue <- renderHighchart({
      
      papers_q2 <- papers_year_gen()
      
      q2 <- papers_q2[c("fwci","n_referencia","n_autores","topic_label")]
      
      
      hchart(q2,type="bubble",hcaes(x= n_autores, y = n_referencia,z = fwci, group = topic_label)) |> 
        hc_tooltip(
          pointFormat = "
    <b>Número de autores: </b>{point.x}<br>
    <b>Número de Referencias: </b>{point.y}<br>
    <b>Relevancia: </b>{point.z:,.2f}"
        ) |>
        hc_title(text = paste("<b>Relevancia de papers publicados",min(papers$year),"-",max(papers$year),"</b>")) |>
        hc_add_theme(hc_theme_538())
      
      
      
      
      
      
      
    })
#### Tabla Dinámica ####
    output$top_5_citas <- renderDT({
      tbl_1 <-papers_year_gen()
      datatable(
        tbl_1[,-c(2,4,5,12)],
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
  df <- papers_year_gen()
  opciones_nuevas <- unique(df$titulo)
  
  # 5. Actualizamos el selector de la UI de forma eficiente
  updateSelectInput(session, "paper_title",
                    choices = opciones_nuevas,
                    selected = head(opciones_nuevas, 1))
})


#### velocimetro papers (fcwi) ####
output$gauge_papers_fcwi <- renderHighchart({
 
 papersg_fcwi <- papers_year_gen()
 
 df_gauge <- papersg_fcwi |> filter(titulo == input$paper_title)
 
  
 highchart() %>%
    hc_chart(type = "gauge", plotBackgroundColor = NULL, plotBackgroundImage = NULL, plotBorderWidth = 0, plotShadow = FALSE) %>%
    hc_title(text = "FCWI") %>%
    hc_pane(startAngle = -90, endAngle = 90,
            background = list(
              list(backgroundColor = "#FFF", borderWidth = 0, outerRadius = "109%", innerRadius = "107%")
            )) %>%
    hc_yAxis(min = min(papers$fwci), max = max(papers$fwci),
             minorTickInterval = "auto", minorTickWidth = 1, minorTickLength = 10, minorTickPosition = "inside", minorTickColor = "#666",
             tickPixelInterval = 30, tickWidth = 2, tickPosition = "inside", tickLength = 10, tickColor = "#666",
             labels = list(step = 2, rotation = "auto"),
             plotBands = list(
               list(from = min(papers$fwci), to = 10, color = "#DF5353"),    # Verde
               list(from = 10, to = max(papers$fwci)*0.8, color = "#DDDF0D"),   # Amarillo
               list(from = max(papers$fwci)*0.8, to = max(papers$fwci), color = "#55BF3B")   # Rojo
             )) %>%
    hc_add_series(name = "Valor", data = list(round(df_gauge$fwci,2)))
})
#### valocimetro papers (autores) ####
output$gauge_papers_auth <- renderHighchart({
  
  papersg_auth <- papers_year_gen()
  
  df_gauge <- papersg_auth |> filter(titulo == input$paper_title)
  
  
  highchart() %>%
    hc_chart(type = "gauge", plotBackgroundColor = NULL, plotBackgroundImage = NULL, plotBorderWidth = 0, plotShadow = FALSE) %>%
    hc_title(text = "Autores") %>%
    hc_pane(startAngle = -90, endAngle = 90,
            background = list(
              list(backgroundColor = "#FFF", borderWidth = 0, outerRadius = "109%", innerRadius = "107%")
            )) %>%
    hc_yAxis(min = min(papers$n_autores), max = max(papers$n_autores),
             minorTickInterval = "auto", minorTickWidth = 1, minorTickLength = 10, minorTickPosition = "inside", minorTickColor = "#666",
             tickPixelInterval = 30, tickWidth = 2, tickPosition = "inside", tickLength = 10, tickColor = "#666",
             labels = list(step = 2, rotation = "auto"),
             plotBands = list(
               list(from = min(papers$n_autores), to = 10, color = "#DF5353"),    # Verde
               list(from = 10, to = max(papers$n_autores)*0.8, color = "#DDDF0D"),   # Amarillo
               list(from = max(papers$n_autores)*0.8, to = max(papers$n_autores), color = "#55BF3B")   # Rojo
             )) %>%
    hc_add_series(name = "Valor", data = list(round(df_gauge$n_autores,2)),
                  tooltip = list(valueSuffix = " autores"))
})
#### valocimetro papers (autores) ####
output$gauge_papers_reff <- renderHighchart({
  
  papersg_reff <- papers_year_gen()
  
  df_gauge <- papersg_reff |> filter(titulo == input$paper_title)
  
  
  highchart() %>%
    hc_chart(type = "gauge", plotBackgroundColor = NULL, plotBackgroundImage = NULL, plotBorderWidth = 0, plotShadow = FALSE) %>%
    hc_title(text = "Referencias") %>%
    hc_pane(startAngle = -90, endAngle = 90,
            background = list(
              list(backgroundColor = "#FFF", borderWidth = 0, outerRadius = "109%", innerRadius = "107%")
            )) %>%
    hc_yAxis(min = min(papers$n_referencia), max = max(papers$n_referencia),
             minorTickInterval = "auto", minorTickWidth = 1, minorTickLength = 10, minorTickPosition = "inside", minorTickColor = "#666",
             tickPixelInterval = 30, tickWidth = 2, tickPosition = "inside", tickLength = 10, tickColor = "#666",
             labels = list(step = 2, rotation = "auto"),
             plotBands = list(
               list(from = min(papers$n_referencia), to = 10, color = "#DF5353"),    # Verde
               list(from = 10, to = max(papers$n_referencia)*0.8, color = "#DDDF0D"),   # Amarillo
               list(from = max(papers$n_referencia)*0.8, to = max(papers$n_referencia), color = "#55BF3B")   # Rojo
             )) %>%
    hc_add_series(name = "Valor", data = list(round(df_gauge$n_referencia,2)),
                  tooltip = list(valueSuffix = " referencias"))
})
#### valocimetro papers (citaciones) ####
output$gauge_papers_citas <- renderHighchart({
  
  papersg_reff <- papers_year_gen()
  
  df_gauge <- papersg_reff |> filter(titulo == input$paper_title)
  
  
  highchart() %>%
    hc_chart(type = "gauge", plotBackgroundColor = NULL, plotBackgroundImage = NULL, plotBorderWidth = 0, plotShadow = FALSE) %>%
    hc_title(text = "Citas") %>%
    hc_pane(startAngle = -90, endAngle = 90,
            background = list(
              list(backgroundColor = "#FFF", borderWidth = 0, outerRadius = "109%", innerRadius = "107%")
            )) %>%
    hc_yAxis(min = min(papers$nro_de_citas), max = max(papers$nro_de_citas),
             minorTickInterval = "auto", minorTickWidth = 1, minorTickLength = 10, minorTickPosition = "inside", minorTickColor = "#666",
             tickPixelInterval = 30, tickWidth = 2, tickPosition = "inside", tickLength = 10, tickColor = "#666",
             labels = list(step = 2, rotation = "auto"),
             plotBands = list(
               list(from = min(papers$nro_de_citas), to = 10, color = "#DF5353"),    # Verde
               list(from = 10, to = max(papers$nro_de_citas)*0.8, color = "#DDDF0D"),   # Amarillo
               list(from = max(papers$nro_de_citas)*0.8, to = max(papers$nro_de_citas), color = "#55BF3B")   # Rojo
             )) %>%
    hc_add_series(name = "Valor", data = list(round(df_gauge$nro_de_citas,2)),
                  tooltip = list(valueSuffix = " citaciones"))
})
#### Botón de refrescar ####
observeEvent(input$mi_boton, {
  # Aquí escribes la acción que deseas ejecutar al pulsar el botón
  print("El usuario ha presionado el botón.")
})
}

# Run the application 
shinyApp(ui = ui, server = server)
