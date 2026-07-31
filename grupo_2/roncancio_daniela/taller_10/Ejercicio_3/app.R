install.packages("DT")

library(shiny)
library(ggplot2)
library(DT)
library(bslib)
library(dplyr)

# ── DATOS ─────────────────────────────────────────────────────────────────────
set.seed(42)

departamentos <- c(
  "Amazonas","Antioquia","Arauca","Atlantico","Bolivar","Boyaca","Caldas",
  "Caqueta","Casanare","Cauca","Cesar","Choco","Cordoba","Cundinamarca",
  "Guainia","Guaviare","Huila","La Guajira","Magdalena","Meta","Narino",
  "Norte de Santander","Putumayo","Quindio","Risaralda","San Andres",
  "Santander","Sucre","Tolima","Valle del Cauca","Vaupes","Vichada"
)

regiones <- c(
  "Amazonia","Andina","Orinoquia","Caribe","Caribe","Andina","Andina",
  "Amazonia","Orinoquia","Andina","Caribe","Pacifico","Caribe","Andina",
  "Amazonia","Amazonia","Andina","Caribe","Caribe","Orinoquia","Pacifico",
  "Andina","Amazonia","Andina","Andina","Caribe",
  "Andina","Caribe","Andina","Pacifico","Amazonia","Orinoquia"
)

muns_por_depto <- c(
  2,125,7,23,45,123,27,16,19,42,25,30,30,116,
  2,4,37,15,30,29,64,40,13,12,14,2,
  87,26,47,42,2,4
)
muns_por_depto[2] <- muns_por_depto[2] + (1122 - sum(muns_por_depto))

municipios_list <- lapply(seq_along(departamentos), function(i) {
  nd  <- muns_por_depto[i]
  dep <- departamentos[i]
  reg <- regiones[i]
  base_ipm    <- switch(reg, "Amazonia"=65,"Orinoquia"=45,"Caribe"=55,"Pacifico"=60,"Andina"=35)
  base_nbi    <- switch(reg, "Amazonia"=70,"Orinoquia"=50,"Caribe"=55,"Pacifico"=55,"Andina"=30)
  base_desemp <- switch(reg, "Amazonia"=12,"Orinoquia"=10,"Caribe"=14,"Pacifico"=13,"Andina"=11)
  data.frame(
    Municipio    = paste0(dep, "_", seq_len(nd)),
    Departamento = dep,
    Region       = reg,
    IPM          = round(pmax(0, pmin(100, rnorm(nd, base_ipm,    15))), 2),
    NBI          = round(pmax(0, pmin(100, rnorm(nd, base_nbi,    12))), 2),
    Desempleo    = round(pmax(0, pmin(50,  rnorm(nd, base_desemp,  4))), 2),
    Poblacion    = round(runif(nd, 2000, 500000)),
    stringsAsFactors = FALSE
  )
})
df_mun <- bind_rows(municipios_list)

# ── TEMA UNAL ─────────────────────────────────────────────────────────────────
tema_unal <- bs_theme(
  version  = 5,
  bg       = "#ffffff",
  fg       = "#212529",
  primary  = "#1a6b3c",
  secondary= "#5cb85c"
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = "Indicadores Socioeconómicos Colombia",
  theme = tema_unal,
  id    = "navbar",
  
  # Pestaña 1: Resumen
  tabPanel("Resumen",
           br(),
           fluidRow(
             column(3,
                    selectInput("depto_filtro", "Filtrar por departamento:",
                                choices  = c("Todos", sort(departamentos)),
                                selected = "Todos")
             )
           ),
           fluidRow(
             column(3, uiOutput("card_muns")),
             column(3, uiOutput("card_ipm")),
             column(3, uiOutput("card_nbi")),
             column(3, uiOutput("card_desemp"))
           ),
           br(),
           fluidRow(
             column(6,
                    h5("IPM por region", style="color:#1a6b3c;font-weight:600;"),
                    plotOutput("plot_resumen_ipm", height="280px")
             ),
             column(6,
                    h5("Desempleo por region", style="color:#1a6b3c;font-weight:600;"),
                    plotOutput("plot_resumen_desemp", height="280px")
             )
           )
  ),
  
  # Pestaña 2: Municipios
  tabPanel("Municipios",
           br(),
           fluidRow(
             column(3,
                    selectInput("depto_tabla", "Departamento:",
                                choices  = c("Todos", sort(departamentos)),
                                selected = "Todos")
             ),
             column(3,
                    sliderInput("rango_ipm", "Rango IPM:",
                                min=0, max=100, value=c(0,100), step=1)
             ),
             column(3,
                    sliderInput("rango_nbi", "Rango NBI:",
                                min=0, max=100, value=c(0,100), step=1)
             ),
             column(3,
                    sliderInput("rango_desemp", "Rango Desempleo (%):",
                                min=0, max=50, value=c(0,50), step=0.5)
             )
           ),
           DT::dataTableOutput("tabla_municipios")
  ),
  
  # Pestaña 3: Comparación
  tabPanel("Comparacion",
           br(),
           fluidRow(
             column(3,
                    selectInput("eje_x", "Eje X:",
                                choices=c("IPM","NBI","Desempleo"), selected="IPM")
             ),
             column(3,
                    selectInput("eje_y", "Eje Y:",
                                choices=c("IPM","NBI","Desempleo"), selected="NBI")
             ),
             column(3,
                    selectInput("depto_comp", "Departamento:",
                                choices=c("Todos", sort(departamentos)), selected="Todos")
             ),
             column(3,
                    br(),
                    checkboxInput("mostrar_linea", "Linea de tendencia", value=TRUE)
             )
           ),
           plotOutput("plot_dispersion", height="460px")
  )
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  rv <- reactiveValues(depto = "Todos")
  
  observeEvent(input$depto_filtro, { rv$depto <- input$depto_filtro })
  observeEvent(input$depto_tabla,  { rv$depto <- input$depto_tabla  })
  observeEvent(input$depto_comp,   { rv$depto <- input$depto_comp   })
  
  observeEvent(rv$depto, {
    updateSelectInput(session, "depto_filtro", selected = rv$depto)
    updateSelectInput(session, "depto_tabla",  selected = rv$depto)
    updateSelectInput(session, "depto_comp",   selected = rv$depto)
  }, ignoreInit = TRUE)
  
  df_filtrado <- reactive({
    if (rv$depto == "Todos") df_mun
    else filter(df_mun, Departamento == rv$depto)
  })
  
  df_tabla <- reactive({
    df_filtrado() %>%
      filter(
        IPM       >= input$rango_ipm[1],    IPM       <= input$rango_ipm[2],
        NBI       >= input$rango_nbi[1],    NBI       <= input$rango_nbi[2],
        Desempleo >= input$rango_desemp[1], Desempleo <= input$rango_desemp[2]
      )
  })
  
  make_card <- function(titulo, valor, subtitulo, color) {
    div(style=paste0(
      "background:",color,";border-radius:10px;padding:16px 20px;",
      "margin-bottom:16px;color:white;box-shadow:0 3px 10px rgba(0,0,0,0.15);"
    ),
    div(style="font-size:0.8rem;opacity:0.9;text-transform:uppercase;", titulo),
    div(style="font-size:1.9rem;font-weight:700;margin:4px 0;", valor),
    div(style="font-size:0.75rem;opacity:0.85;", subtitulo)
    )
  }
  
  output$card_muns  <- renderUI({
    make_card("Municipios", nrow(df_filtrado()), "en el filtro actual", "#1a6b3c")
  })
  output$card_ipm   <- renderUI({
    make_card("IPM promedio",
              paste0(round(mean(df_filtrado()$IPM, na.rm=TRUE), 1), "%"),
              "Indice de Pobreza Multidimensional", "#2980b9")
  })
  output$card_nbi   <- renderUI({
    make_card("NBI promedio",
              paste0(round(mean(df_filtrado()$NBI, na.rm=TRUE), 1), "%"),
              "Necesidades Basicas Insatisfechas", "#8e44ad")
  })
  output$card_desemp <- renderUI({
    make_card("Desempleo promedio",
              paste0(round(mean(df_filtrado()$Desempleo, na.rm=TRUE), 1), "%"),
              "Tasa de desempleo municipal", "#c0392b")
  })
  
  output$plot_resumen_ipm <- renderPlot({
    ggplot(df_filtrado(), aes(x=Region, y=IPM, fill=Region)) +
      geom_boxplot(alpha=0.8, outlier.size=0.8) +
      scale_fill_brewer(palette="Set2") +
      labs(x=NULL, y="IPM (%)") +
      theme_minimal(base_size=11) +
      theme(legend.position="none",
            axis.text.x=element_text(angle=25, hjust=1))
  })
  
  output$plot_resumen_desemp <- renderPlot({
    ggplot(df_filtrado(), aes(x=Region, y=Desempleo, fill=Region)) +
      geom_boxplot(alpha=0.8, outlier.size=0.8) +
      scale_fill_brewer(palette="Set1") +
      labs(x=NULL, y="Desempleo (%)") +
      theme_minimal(base_size=11) +
      theme(legend.position="none",
            axis.text.x=element_text(angle=25, hjust=1))
  })
  
  output$tabla_municipios <- DT::renderDataTable({
    df_tabla() %>%
      select(Municipio, Departamento, Region, IPM, NBI, Desempleo, Poblacion) %>%
      arrange(Departamento, Municipio)
  },
  options  = list(pageLength=15, scrollX=TRUE),
  rownames = FALSE,
  filter   = "top",
  class    = "table table-striped table-hover table-sm"
  )
  
  output$plot_dispersion <- renderPlot({
    req(input$eje_x, input$eje_y)
    df <- df_filtrado()
    p  <- ggplot(df, aes_string(x=input$eje_x, y=input$eje_y, color="Region")) +
      geom_point(alpha=0.5, size=1.8) +
      scale_color_brewer(palette="Dark2") +
      labs(title=paste("Relacion:", input$eje_x, "vs", input$eje_y),
           x=input$eje_x, y=input$eje_y, color="Region") +
      theme_minimal(base_size=12) +
      theme(plot.title=element_text(face="bold", color="#1a6b3c", size=14))
    if (input$mostrar_linea) {
      p <- p + geom_smooth(method="lm", se=FALSE, color="#c0392b",
                           linewidth=0.8, aes(group=1))
    }
    p
  })
}

shinyApp(ui, server)
