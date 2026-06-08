library(shiny)
library(bslib)

ui <- bslib::page_fluid(
  theme = bs_theme(version = 5, bootswatch = "minty"), # Un toque de estilo
  titlePanel("Simulador de Distribución Normal"),
  
  layout_sidebar(
    sidebar = sidebar(
      sliderInput("n", "Tamaño de la muestra", 0, 100, 25),
      helpText("Ajusta el slider para actualizar los resultados en ambas pestañas.")
    ),
    navset_card_tab(
      nav_panel(
        title = "Visualización",
        plotOutput("hist")
      ),
      nav_panel(
        title = "Datos Numéricos",
        verbatimTextOutput("summary"),
        tableOutput("table")
      )
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    req(input$n)
    rnorm(input$n)
  })
  
  output$hist <- renderPlot({
    hist(data(), col = "steelblue", border = "white", 
         main = paste("Histograma de n =", input$n))
  })
  
  output$summary <- renderPrint({
    summary(data())
  })
  
  output$table <- renderTable({
    data.frame(Valores = data())
  })
}

shinyApp(ui, server)