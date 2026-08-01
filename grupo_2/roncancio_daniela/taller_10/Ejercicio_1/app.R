library(shiny)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("Perfil Estadistico de Variables"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo", "Cargar archivo CSV",
                accept = ".csv",
                buttonLabel = "Buscar...",
                placeholder = "Sin archivo (usando iris)"),
      
      uiOutput("ui_variable"),
      
      hr(),
      downloadButton("descargar", "Descargar estadisticas CSV")
    ),
    
    mainPanel(
      h4(textOutput("titulo_var")),
      tableOutput("tabla_stats"),
      plotOutput("grafico")
    )
  )
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # 1. Datos reactivos: CSV cargado o iris por defecto
  datos <- reactive({
    if (is.null(input$archivo)) return(iris)
    read.csv(input$archivo$datapath, stringsAsFactors = TRUE)
  })
  
  # 2. selectInput dinamico con columnas del archivo
  output$ui_variable <- renderUI({
    selectInput("variable", "Seleccionar variable",
                choices = names(datos()))
  })
  
  # Variable seleccionada (vector)
  columna <- reactive({
    req(input$variable)
    datos()[[input$variable]]
  })
  
  es_numerica <- reactive({ is.numeric(columna()) })
  
  # 3. Titulo
  output$titulo_var <- renderText({
    req(input$variable)
    tipo <- if (es_numerica()) "Numerica" else "Categorica"
    paste0(input$variable, "  [", tipo, "]")
  })
  
  # 4. Tabla de estadisticas
  stats_df <- reactive({
    req(input$variable)
    x <- columna()
    n_total   <- length(x)
    n_faltant <- sum(is.na(x))
    n_unicos  <- length(unique(x))
    
    if (es_numerica()) {
      data.frame(
        Estadistico = c("N total", "Faltantes", "Unicos",
                        "Media", "Mediana", "Desv. estandar", "Minimo", "Maximo"),
        Valor = c(n_total, n_faltant, n_unicos,
                  round(mean(x, na.rm = TRUE), 3),
                  round(median(x, na.rm = TRUE), 3),
                  round(sd(x, na.rm = TRUE), 3),
                  round(min(x, na.rm = TRUE), 3),
                  round(max(x, na.rm = TRUE), 3))
      )
    } else {
      tbl <- sort(table(x), decreasing = TRUE)
      data.frame(
        Categoria           = names(tbl),
        Frec_absoluta       = as.integer(tbl),
        Frec_relativa       = paste0(round(prop.table(tbl) * 100, 1), "%")
      )
    }
  })
  
  output$tabla_stats <- renderTable(stats_df(), striped = TRUE, hover = TRUE)
  
  # 5. Grafico
  output$grafico <- renderPlot({
    req(input$variable)
    x <- columna()
    
    if (es_numerica()) {
      hist(x, main = paste("Histograma -", input$variable),
           xlab = input$variable, ylab = "Frecuencia",
           col = "#4e79a7", border = "white")
    } else {
      tbl <- sort(table(x), decreasing = TRUE)
      barplot(tbl,
              main = paste("Frecuencias -", input$variable),
              xlab = input$variable, ylab = "Frecuencia",
              col = "#59a14f", border = "white",
              las = 2, cex.names = 0.85)
    }
  })
  
  # 6. Descarga CSV
  output$descargar <- downloadHandler(
    filename = function() paste0("stats_", input$variable, ".csv"),
    content  = function(file) write.csv(stats_df(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)