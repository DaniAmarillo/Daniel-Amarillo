library(shiny)
library(ggplot2)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("Simulador de Distribuciones de Probabilidad"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Distribucion",
                  choices = c("Normal", "t-Student", "Ji-cuadrado",
                              "Exponencial", "Binomial", "Poisson")),
      
      sliderInput("n", "Tamano de muestra", 50, 5000, 500, step = 50),
      
      hr(),
      uiOutput("ui_params"),
      hr(),
      actionButton("simular", "Nueva simulacion", icon = icon("play"),
                   class = "btn-success btn-block")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Histograma + Densidad", plotOutput("plot_hist", height = "450px")),
        tabPanel("QQ-Plot",              plotOutput("plot_qq",   height = "450px"))
      )
    )
  )
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # 1. Parametros dinamicos segun distribucion
  output$ui_params <- renderUI({
    switch(input$dist,
           "Normal" = tagList(
             sliderInput("p1", "Media",            -10, 10,  0,   step = 0.5),
             sliderInput("p2", "Desv. estandar",     0.1, 5, 1,   step = 0.1)
           ),
           "t-Student" = tagList(
             sliderInput("p1", "Grados de libertad", 1, 30, 5, step = 1)
           ),
           "Ji-cuadrado" = tagList(
             sliderInput("p1", "Grados de libertad", 1, 30, 5, step = 1)
           ),
           "Exponencial" = tagList(
             sliderInput("p1", "Tasa (lambda)",  0.1, 5, 1, step = 0.1)
           ),
           "Binomial" = tagList(
             sliderInput("p1", "Num. ensayos (n)", 1, 100, 20, step = 1),
             sliderInput("p2", "Prob. exito (p)",  0.01, 0.99, 0.5, step = 0.01)
           ),
           "Poisson" = tagList(
             sliderInput("p1", "Lambda",  0.5, 20, 3, step = 0.5)
           )
    )
  })
  
  # 2. Muestra reactiva — se regenera con el boton o al cambiar parametros
  muestra <- eventReactive(
    list(input$simular, input$dist, input$n, input$p1, input$p2),
    {
      req(input$p1)
      set.seed(NULL)   # seed aleatoria cada vez
      switch(input$dist,
             "Normal"       = rnorm(input$n,  mean = input$p1, sd = input$p2),
             "t-Student"    = rt(input$n,     df   = input$p1),
             "Ji-cuadrado"  = rchisq(input$n, df   = input$p1),
             "Exponencial"  = rexp(input$n,   rate = input$p1),
             "Binomial"     = rbinom(input$n, size = input$p1, prob = input$p2),
             "Poisson"      = rpois(input$n,  lambda = input$p1)
      )
    },
    ignoreNULL = FALSE
  )
  
  # Funcion densidad teorica para cada distribucion
  densidad_teorica <- reactive({
    req(input$p1)
    x <- muestra()
    xseq <- seq(min(x), max(x), length.out = 300)
    
    y <- switch(input$dist,
                "Normal"      = dnorm(xseq,  mean = input$p1, sd = input$p2),
                "t-Student"   = dt(xseq,     df   = input$p1),
                "Ji-cuadrado" = dchisq(xseq, df   = input$p1),
                "Exponencial" = dexp(xseq,   rate = input$p1),
                "Binomial"    = {
                  xi <- floor(xseq)
                  dbinom(xi, size = input$p1, prob = input$p2)
                },
                "Poisson"     = {
                  xi <- floor(xseq)
                  dpois(xi, lambda = input$p1)
                }
    )
    data.frame(x = xseq, y = y)
  })
  
  # 3. Histograma + densidad teorica
  output$plot_hist <- renderPlot({
    x  <- muestra()
    df_teo <- densidad_teorica()
    
    es_discreta <- input$dist %in% c("Binomial", "Poisson")
    
    p <- ggplot(data.frame(x = x), aes(x = x)) +
      labs(title = paste("Distribucion", input$dist),
           x = "Valor", y = "Densidad / Frecuencia relativa") +
      theme_minimal(base_size = 14)
    
    if (es_discreta) {
      p <- p +
        geom_bar(aes(y = after_stat(prop)), fill = "#4e79a7", alpha = 0.7) +
        geom_line(data = df_teo, aes(x = x, y = y),
                  color = "#e15759", linewidth = 1.2)
    } else {
      p <- p +
        geom_histogram(aes(y = after_stat(density)),
                       bins = 40, fill = "#4e79a7", alpha = 0.7, color = "white") +
        geom_line(data = df_teo, aes(x = x, y = y),
                  color = "#e15759", linewidth = 1.2)
    }
    p
  })
  
  # 4. QQ-plot
  output$plot_qq <- renderPlot({
    x <- muestra()
    ggplot(data.frame(x = x), aes(sample = x)) +
      stat_qq(color = "#4e79a7", alpha = 0.6) +
      stat_qq_line(color = "#e15759", linewidth = 1) +
      labs(title = paste("QQ-Plot -", input$dist),
           x = "Cuantiles teoricos", y = "Cuantiles muestrales") +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)

