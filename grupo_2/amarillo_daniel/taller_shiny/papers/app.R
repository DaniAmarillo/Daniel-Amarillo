
paquetes <- c( 
  "RSQLite","knitr","tidyverse","knitr","highcharter"
  ,"bslib","tidyverse","lubridate","DT","bsicons"

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
    sliderInput("year", "Año:", min = min(papers$year), max = max(papers$year), value = c(2024,2026))
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
                    showcase = bs_icon("book",fill = "rgb(119,152,191) !important"),
                    p("por paper"),
                    styles = list(header = "font-size: 0.9rem; font-weight: bold;")
                  ))
                ), 
                fluidRow(wellPanel(value_box(
                  title = "# promedio de citas",
                  value =  h3(textOutput(outputId = "mean_citas")),
                  showcase = bs_icon("Flag",fill = "rgb(119,152,191) !important"),
                  p("por paper"),
                  styles = list(header = "font-size: 0.9rem; font-weight: bold;")
                )))),
                column(
                  width = 12,
                  fluidRow(
                    wellPanel(value_box(
                      title = "# de papers",
                      value =  h3(textOutput(outputId = "n_papers")),
                      showcase = bs_icon("file-text",fill = "rgb(119,152,191) !important"), 
                      p("publicados"),
                      styles = list(header = "font-size: 0.9rem; font-weight: bold;")
                    ))
                  ), 
                  fluidRow(wellPanel(value_box(
                    title = "FCWI promedio",
                    value =  h3(textOutput(outputId = "mean_fwci")),
                    showcase = bs_icon("graph-up",fill = "rgb(119,152,191) !important"),
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
              selectInput("paper_title","Paper:",papers$titulo),
              layout_columns(
                col_widths = c(4,4,4),
                column(
                  width = 12,
                  fluidRow(
                    wellPanel(value_box(
                      title = "referencias promedio",
                      value =  h3(textOutput(outputId = "mean_referencias")),
                      showcase = bs_icon("book",fill = "rgb(119,152,191) !important"),
                      p("por paper"),
                      styles = list(header = "font-size: 0.9rem; font-weight: bold;")
                    ))
                  )
              )
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
server <- function(input, output) {
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
####  ####
}

# Run the application 
shinyApp(ui = ui, server = server)
