
paquetes <- c( 
  "RSQLite","knitr","tidyverse","knitr","highcharter","bslib","tidyverse"

)

# Verificamos qué paquetes faltan
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  install.packages(pendientes)
}

# Cargamos los paquetes sin mostrar mensajes
lapply(paquetes, library, character.only = TRUE)


con <- dbConnect(RSQLite::SQLite(), "JBA_25_26.sqlite")

for( i in dbListTables(con)){
  assign(i,dbReadTable(con, i))
}
papers <- papers |> mutate(issue = str_remove(issue,".*:"))

papers$year <- as.numeric(papers$year)
papers$issue <- factor(papers$issue,c(" Issue 1 (Mar 2025)"," Issue 2 (Jun 2025)",
                                      " Issue 3 (Sep 2025)"," Issue 4 (Dec 2025)", 
                                      " Issue 1 (Mar 2026)"),ordered = TRUE)

library(shiny)

ui <- fluidPage(page_navbar( 
  sidebar = sidebar(
    title = "Controles Generales",
    sliderInput("year", "Número de barras:", min = min(papers$year), max = max(papers$year), value = 30),
  ),
###### pestaña general #####
    nav_panel(
              "general",
              layout_columns( 
                col_widths = c(7,5),
                card(
                  highchartOutput("issues_gen"),
                    ), 
                            ),
              layout_columns( 
                col_widths = c(6,6),
                card(card_header("HOLA1")),
                card(card_header("HOLA2"))
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

papers_year_gen <- reactive({
  req(input$year)
  
  filter(papers,year == input$year)
  })

q1 <- reactive({papers_year_gen() |> 
    group_by(topic_label,issue, .drop = FALSE)|>
    summarize(temas = n())})

levels(papers$issue)
#### temas por issue ####
    output$issues_gen <- renderHighchart({
      
      # Build chart based on input$type
      papers_q1 <- papers_year_gen()
      
      q1 <- papers_q1 |>
            group_by(topic_label,issue, .drop = FALSE)|>
            summarize(temas = n())
      
      q1 |> hchart(type="column",
                   hcaes(x = issue, y = temas, group = topic_label),
                   stacking = list(enabled = TRUE))|>
        hc_title(text = "<b>Gráfico de temas por issue</b>") 
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
