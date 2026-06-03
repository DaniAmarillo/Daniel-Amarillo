
paquetes <- c( 
  "RSQLite","knitr","tidyverse","knitr","highcharter","bslib","tidyverse","lubridate"

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
papers$fecha_publicacion <- dmy(papers$fecha_publicacion)
papers$issue <- factor(papers$issue,c(" Issue 1 (Mar 2025)"," Issue 2 (Jun 2025)",
                                      " Issue 3 (Sep 2025)"," Issue 4 (Dec 2025)", 
                                      " Issue 1 (Mar 2026)"),ordered = TRUE)

library(shiny)
#### UI ####
ui <- fluidPage(page_navbar( 
  theme = bs_theme(version = 5),
  input_dark_mode(id = "dark_mode_trigger"),
  sidebar = sidebar(
    title = "Controles Generales",
    sliderInput("year", "Año:", min = min(papers$year), max = max(papers$year), value = c(2024,2026)),
  ),
###### pestaña general #####
    nav_panel(
              "General",
              layout_columns( 
                col_widths = c(7,5),
                card(
                  highchartOutput("issues_gen"),
                    ),
                column(
                  width = 5,
                fluidRow(
                  wellPanel(value_box(
                    title = "Papers publicados",
                    value =  h3(textOutput(outputId = "n_papers")),
                    showcase = NULL, # Puedes añadir un icono aquí
                    p("tasks completed"),
                    styles = list(header = "font-size: 0.9rem; font-weight: bold;")
                  ))
                ), 
                fluidRow(wellPanel(value_box(
                  title = "Citas Promedio",
                  value =  h3(textOutput(outputId = "mean_citas")),
                  showcase = NULL, # Puedes añadir un icono aquí
                  p("tasks completed"),
                  styles = list(header = "font-size: 0.9rem; font-weight: bold;")
                ))))
                            ),
              layout_columns( 
                card(highchartOutput("fcwi_issue")
                     ),
                )
              ), 
###### Papers ######
    nav_panel(title="Papers", "Page B content"), 
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

#### numero de papers en general ####
     output$n_papers <- renderText({
       papers_n <- papers_year_gen()
       
       print(nrow(papers_n))
       
       
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
        hc_add_theme(hc_theme_darkunica())
    
        
    })
    output$fcwi_issue <- renderHighchart({
      
      papers_q2 <- papers_year_gen()
      
      q2 <- papers_q2[c("fwci","n_referencia","n_autores","topic_label")]
      
      
      hchart(q2,type="bubble",hcaes(x= n_autores, y = n_referencia,z = fwci, group = topic_label)) |> 
        hc_tooltip(
          pointFormat = "
    <b>Número de autores: </b>{point.x:,.2f}<br>
    <b>Número de Referencias: </b>{point.y:,.2f}<br>
    <b>Relevancia: </b>{point.z:,.2f}"
        ) |>
        hc_title(text = paste("<b>Relevancia de papers publicados",min(papers$year),"-",max(papers$year),"</b>")) |>
        hc_add_theme(hc_theme_darkunica())
      
      
      
      
      
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
