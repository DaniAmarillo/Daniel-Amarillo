library(shiny)
library(ggplot2)
library(bslib)

# ── TEMA UNAL ─────────────────────────────────────────────────────────────────
tema_unal <- bs_theme(
  version   = 5,
  bg        = "#ffffff",
  fg        = "#212529",
  primary   = "#1a6b3c",
  secondary = "#5cb85c"
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = "Regresion Lineal — App Pedagogica",
  theme = tema_unal,
  
  # ── Pestaña 1: Datos y modelo ───────────────────────────────────────────────
  tabPanel("Modelo",
           br(),
           fluidRow(
             # Panel izquierdo: controles
             column(3,
                    div(style="background:#f8f9fa;border-radius:8px;padding:16px;margin-bottom:14px;",
                        h6("1. Datos", style="color:#1a6b3c;font-weight:700;"),
                        fileInput("archivo", NULL,
                                  buttonLabel = "Cargar CSV",
                                  placeholder = "Sin archivo"),
                        uiOutput("info_datos")
                    ),
                    div(style="background:#f8f9fa;border-radius:8px;padding:16px;margin-bottom:14px;",
                        h6("2. Variables", style="color:#1a6b3c;font-weight:700;"),
                        uiOutput("sel_y"),
                        uiOutput("sel_x")
                    ),
                    div(style="background:#f8f9fa;border-radius:8px;padding:16px;margin-bottom:14px;",
                        h6("3. Transformaciones", style="color:#1a6b3c;font-weight:700;"),
                        uiOutput("check_transf")
                    ),
                    div(style="text-align:center;",
                        actionButton("btn_ajustar", "Ajustar modelo",
                                     style="background:#1a6b3c;color:white;border:none;
                              border-radius:6px;padding:10px 24px;
                              font-size:1rem;width:100%;cursor:pointer;")
                    )
             ),
             
             # Panel derecho: resultados
             column(9,
                    tabsetPanel(id = "tabs_resultado",
                                # Sub-pestaña: Coeficientes
                                tabPanel("Coeficientes",
                                         br(),
                                         uiOutput("metricas_rapidas"),
                                         br(),
                                         h6("Coeficientes con intervalos de confianza (95%)",
                                            style="color:#1a6b3c;font-weight:600;"),
                                         tableOutput("tabla_coef"),
                                         br(),
                                         h6("Grafico de coeficientes", style="color:#1a6b3c;font-weight:600;"),
                                         plotOutput("plot_coef", height="260px")
                                ),
                                
                                # Sub-pestaña: Diagnósticos
                                tabPanel("Diagnosticos",
                                         br(),
                                         h6("Graficos de diagnostico del modelo",
                                            style="color:#1a6b3c;font-weight:600;"),
                                         plotOutput("plot_diagnosticos", height="560px")
                                ),
                                
                                # Sub-pestaña: Comparación de modelos
                                tabPanel("Comparacion R2",
                                         br(),
                                         uiOutput("comparacion_modelos"),
                                         br(),
                                         plotOutput("plot_comparacion", height="300px")
                                )
                    )
             )
           )
  ),
  
  # ── Pestaña 2: Datos cargados ───────────────────────────────────────────────
  tabPanel("Vista de datos",
           br(),
           tableOutput("tabla_datos")
  )
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Datos ──────────────────────────────────────────────────────────────────
  datos <- reactive({
    if (is.null(input$archivo)) {
      mtcars
    } else {
      tryCatch(
        read.csv(input$archivo$datapath),
        error = function(e) { showNotification("Error al leer el CSV.", type="error"); mtcars }
      )
    }
  })
  
  usando_ejemplo <- reactive({ is.null(input$archivo) })
  
  output$info_datos <- renderUI({
    df  <- datos()
    lbl <- if (usando_ejemplo()) "Usando: mtcars (ejemplo)" else input$archivo$name
    div(style="font-size:0.8rem;color:#6c757d;margin-top:4px;",
        lbl, br(),
        paste0(nrow(df), " filas · ", ncol(df), " columnas"))
  })
  
  # Columnas numericas
  cols_num <- reactive({
    df <- datos()
    names(df)[sapply(df, is.numeric)]
  })
  
  # ── Selectores dinámicos ───────────────────────────────────────────────────
  output$sel_y <- renderUI({
    cols <- cols_num()
    req(length(cols) > 0)
    selectInput("var_y", "Variable dependiente (Y):",
                choices = cols, selected = cols[[1]])
  })
  
  output$sel_x <- renderUI({
    cols <- cols_num()
    req(length(cols) > 1)
    resto <- setdiff(cols, input$var_y)
    selectInput("var_x", "Variables independientes (X):",
                choices  = resto,
                selected = resto[seq_len(min(2, length(resto)))],
                multiple = TRUE)
  })
  
  output$check_transf <- renderUI({
    req(input$var_y, input$var_x)
    todas <- c(input$var_y, input$var_x)
    checkboxGroupInput("transf_vars",
                       "Aplicar log() a:",
                       choices  = todas,
                       selected = NULL)
  })
  
  # ── Modelo (disparado por botón) ───────────────────────────────────────────
  modelo <- eventReactive(input$btn_ajustar, {
    req(input$var_y, input$var_x)
    df    <- datos()
    vars  <- c(input$var_y, input$var_x)
    transf <- input$transf_vars
    
    # Aplicar transformaciones
    for (v in vars) {
      if (v %in% transf) {
        nuevo_nombre <- paste0("log_", v)
        df[[nuevo_nombre]] <- log(df[[v]])
      }
    }
    
    y_name <- if (input$var_y %in% transf) paste0("log_", input$var_y) else input$var_y
    x_names <- sapply(input$var_x, function(v) {
      if (v %in% transf) paste0("log_", v) else v
    })
    
    formula_str <- paste(y_name, "~", paste(x_names, collapse=" + "))
    formula_obj <- as.formula(formula_str)
    
    df_clean <- df[, c(y_name, x_names), drop=FALSE]
    df_clean <- df_clean[complete.cases(df_clean), ]
    
    modelo_fit <- lm(formula_obj, data = df_clean)
    
    n_obs   <- nrow(df_clean)
    n_total <- nrow(datos())
    
    showNotification(
      paste0("Modelo ajustado: ", formula_str,
             " | Observaciones usadas: ", n_obs,
             if (n_obs < n_total) paste0(" (", n_total - n_obs, " eliminadas por NA)") else ""),
      type     = "message",
      duration = 6
    )
    
    list(fit=modelo_fit, formula=formula_str, n=n_obs, df=df_clean)
  })
  
  # ── Modelo SIN transformaciones (para comparar R2) ─────────────────────────
  modelo_base <- eventReactive(input$btn_ajustar, {
    req(input$var_y, input$var_x)
    df  <- datos()
    fml <- as.formula(paste(input$var_y, "~", paste(input$var_x, collapse=" + ")))
    df_clean <- df[, c(input$var_y, input$var_x), drop=FALSE]
    df_clean <- df_clean[complete.cases(df_clean), ]
    lm(fml, data=df_clean)
  })
  
  # ── Métricas rápidas ───────────────────────────────────────────────────────
  output$metricas_rapidas <- renderUI({
    req(modelo())
    fit <- modelo()$fit
    s   <- summary(fit)
    r2  <- round(s$r.squared, 4)
    r2a <- round(s$adj.r.squared, 4)
    rmse <- round(sqrt(mean(residuals(fit)^2)), 4)
    aic  <- round(AIC(fit), 2)
    
    make_m <- function(lbl, val, col) {
      div(style=paste0("background:",col,";color:white;border-radius:8px;",
                       "padding:12px 16px;text-align:center;"),
          div(style="font-size:0.75rem;opacity:0.9;", lbl),
          div(style="font-size:1.5rem;font-weight:700;", val)
      )
    }
    fluidRow(
      column(3, make_m("R²",          r2,   "#1a6b3c")),
      column(3, make_m("R² ajustado", r2a,  "#2980b9")),
      column(3, make_m("RMSE",        rmse, "#8e44ad")),
      column(3, make_m("AIC",         aic,  "#c0392b"))
    )
  })
  
  # ── Tabla de coeficientes ──────────────────────────────────────────────────
  output$tabla_coef <- renderTable({
    req(modelo())
    fit <- modelo()$fit
    cf  <- as.data.frame(coef(summary(fit)))
    ic  <- confint(fit)
    df_out <- data.frame(
      Variable   = rownames(cf),
      Estimacion = round(cf[,1], 5),
      Error_Std  = round(cf[,2], 5),
      t_valor    = round(cf[,3], 3),
      p_valor    = signif(cf[,4], 4),
      IC_2.5     = round(ic[,1], 5),
      IC_97.5    = round(ic[,2], 5)
    )
    df_out
  }, striped=TRUE, hover=TRUE, spacing="s")
  
  # ── Gráfico de coeficientes ────────────────────────────────────────────────
  output$plot_coef <- renderPlot({
    req(modelo())
    fit <- modelo()$fit
    cf  <- as.data.frame(coef(summary(fit)))
    ic  <- confint(fit)
    df_plot <- data.frame(
      var    = rownames(cf),
      est    = cf[,1],
      lo     = ic[,1],
      hi     = ic[,2]
    )
    df_plot <- df_plot[df_plot$var != "(Intercept)", ]
    
    ggplot(df_plot, aes(x=reorder(var, est), y=est)) +
      geom_hline(yintercept=0, linetype="dashed", color="grey60") +
      geom_errorbar(aes(ymin=lo, ymax=hi), width=0.2, color="#2980b9", linewidth=0.9) +
      geom_point(size=3, color="#1a6b3c") +
      coord_flip() +
      labs(title="Coeficientes con IC 95%", x=NULL, y="Estimacion") +
      theme_minimal(base_size=12) +
      theme(plot.title=element_text(face="bold", color="#1a6b3c"))
  })
  
  # ── Diagnósticos (4 gráficos estándar) ────────────────────────────────────
  output$plot_diagnosticos <- renderPlot({
    req(modelo())
    fit <- modelo()$fit
    par(mfrow=c(2,2), mar=c(4,4,3,1))
    plot(fit, which=1, main="Residuos vs Ajustados",   col="#1a6b3c", pch=16, cex=0.7)
    plot(fit, which=2, main="QQ-Plot de residuos",     col="#2980b9", pch=16, cex=0.7)
    plot(fit, which=3, main="Scale-Location",          col="#8e44ad", pch=16, cex=0.7)
    plot(fit, which=4, main="Distancia de Cook",       col="#c0392b", pch=16, cex=0.7)
    par(mfrow=c(1,1))
  })
  
  # ── Comparación R² ─────────────────────────────────────────────────────────
  output$comparacion_modelos <- renderUI({
    req(modelo(), modelo_base())
    r2_base  <- round(summary(modelo_base())$r.squared, 4)
    r2_transf <- round(summary(modelo()$fit)$r.squared, 4)
    dif <- round(r2_transf - r2_base, 4)
    color_dif <- if (dif > 0) "#1a6b3c" else "#c0392b"
    signo     <- if (dif > 0) "+" else ""
    
    div(
      fluidRow(
        column(4,
               div(style="background:#f8f9fa;border-radius:8px;padding:16px;text-align:center;",
                   div(style="font-size:0.8rem;color:#6c757d;", "R² modelo original"),
                   div(style="font-size:2rem;font-weight:700;color:#2980b9;", r2_base)
               )
        ),
        column(4,
               div(style="background:#f8f9fa;border-radius:8px;padding:16px;text-align:center;",
                   div(style="font-size:0.8rem;color:#6c757d;", "R² con transformaciones"),
                   div(style="font-size:2rem;font-weight:700;color:#1a6b3c;", r2_transf)
               )
        ),
        column(4,
               div(style=paste0("background:#f8f9fa;border-radius:8px;padding:16px;text-align:center;"),
                   div(style="font-size:0.8rem;color:#6c757d;", "Diferencia"),
                   div(style=paste0("font-size:2rem;font-weight:700;color:",color_dif,";"),
                       paste0(signo, dif))
               )
        )
      ),
      br(),
      div(style="font-size:0.85rem;color:#495057;",
          strong("Modelo original: "),
          paste(input$var_y, "~", paste(input$var_x, collapse=" + ")),
          br(),
          strong("Modelo transformado: "), modelo()$formula
      )
    )
  })
  
  output$plot_comparacion <- renderPlot({
    req(modelo(), modelo_base())
    r2_base   <- summary(modelo_base())$r.squared
    r2_transf <- summary(modelo()$fit)$r.squared
    
    df_bar <- data.frame(
      Modelo = c("Original", "Con transformaciones"),
      R2     = c(r2_base, r2_transf)
    )
    ggplot(df_bar, aes(x=Modelo, y=R2, fill=Modelo)) +
      geom_bar(stat="identity", width=0.5, show.legend=FALSE) +
      geom_text(aes(label=round(R2,4)), vjust=-0.4, fontface="bold", size=5) +
      scale_fill_manual(values=c("Original"="#2980b9","Con transformaciones"="#1a6b3c")) +
      scale_y_continuous(limits=c(0,1)) +
      labs(title="Comparacion R² entre modelos", x=NULL, y="R²") +
      theme_minimal(base_size=12) +
      theme(plot.title=element_text(face="bold", color="#1a6b3c"))
  })
  
  # ── Vista de datos ─────────────────────────────────────────────────────────
  output$tabla_datos <- renderTable({
    head(datos(), 20)
  }, striped=TRUE, hover=TRUE, spacing="s")
}

shinyApp(ui, server)

