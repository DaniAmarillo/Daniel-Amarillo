source("global.R")

server <- function(input, output, session) {

  # ── Estado reactivo ─────────────────────────────────────────────────────────
  rv <- reactiveValues(
    df_filtrado  = NULL,
    df_nuevos    = NULL,
    n_nuevos     = NULL,
    scrape_msg   = NULL,
    scrape_tipo  = "ok" ,
    highlight_doi = NULL
  )

  # ── Carga inicial ────────────────────────────────────────────────────────────
  observe({
    rv$df_filtrado <- get_papers_filtrados()
  })

  # ── Restablecer filtros ──────────────────────────────────────────────────────
  observeEvent(input$btn_reset, {
    updateDateRangeInput(session, "fecha_rango",
      start = "2025-01-01", end = Sys.Date())
    updateSelectInput(session,   "topic_sel",    selected = "Todos")
    updateTextInput(session,     "autor_busq",   value = "")
    updateTextInput(session,     "doi_busq",     value = "")
    updateTextInput(session,     "keyword_busq", value = "")
    rv$df_filtrado <- get_papers_filtrados()
  })

  # ── Aplicar filtros ──────────────────────────────────────────────────────────
  observeEvent(input$btn_filtrar, {
    rv$df_filtrado <- get_papers_filtrados(
      fecha_inicio = as.character(input$fecha_rango[1]),
      fecha_fin    = as.character(input$fecha_rango[2]),
      topic        = input$topic_sel,
      autor        = trimws(input$autor_busq),
      doi_busq     = trimws(input$doi_busq),
      keyword      = trimws(input$keyword_busq)
    )
  })

  # ── Scraping de artículos nuevos ─────────────────────────────────────────────
  observeEvent(input$btn_scrape, {
    rv$scrape_msg  <- "⏳ Buscando artículos nuevos, por favor espera..."
    rv$scrape_tipo <- "ok"
    rv$df_nuevos   <- NULL
    rv$n_nuevos    <- NULL

    withProgress(message = "Scrapeando PLOS ONE...", value = 0, {
      resultado <- buscar_articulos_nuevos(
        progress_callback = function(msg, val) {
          setProgress(value = val, message = msg)
        }
      )
    })

    if (resultado$nuevos == 0) {
      rv$scrape_msg  <- paste0(
        "ℹ️ No se encontraron artículos nuevos. Se verificaron los últimos 5 DOIs almacenados:\n",
        paste(resultado$verificados, collapse = "\n")
      )
      rv$scrape_tipo <- "warn"
      rv$df_nuevos   <- NULL
      rv$n_nuevos    <- 0
    } else {
      rv$scrape_msg  <- paste0("✅ Se encontraron y almacenaron ", resultado$nuevos, " artículo(s) nuevo(s).")
      rv$scrape_tipo <- "ok"
      rv$df_nuevos   <- resultado$df_nuevos
      rv$n_nuevos    <- resultado$nuevos
      # Refrescar datos filtrados
      rv$df_filtrado <- get_papers_filtrados()
    }
  })

  # ── Mensaje de scraping ───────────────────────────────────────────────────────
  output$scrape_msg <- renderUI({
    req(rv$scrape_msg)
    cls <- if (rv$scrape_tipo == "warn") "scrape-box warn" else "scrape-box"
    div(class = cls, rv$scrape_msg)
  })

  # ── KPI Cards ────────────────────────────────────────────────────────────────
  output$kpi_cards <- renderUI({
    df <- rv$df_filtrado
    req(df)
    
    # Identificar la temática activa para el título
    topic_activo <- if (is.null(input$topic_sel) || input$topic_sel == "Todos") {
      "Todas las temáticas"
    } else {
      input$topic_sel
    }
    
    n_total <- nrow(df)
    
    # ── Cálculos seguros (evitan NaN e -Inf si el filtro no arroja resultados) ──
    if (n_total > 0) {
      avg_autores <- round(mean(df$n_authors, na.rm = TRUE), 1)
      avg_citas   <- round(mean(df$citations, na.rm = TRUE), 1)
      avg_refs    <- round(mean(df$n_references, na.rm = TRUE), 1)
      
      # Buscar artículo más citado
      idx_citas  <- which.max(df$citations)
      max_citas  <- df$citations[idx_citas]
      top_citado <- paste0(substr(df$title[idx_citas], 1, 35), "…")
      
      # Buscar artículo más descargado
      idx_views  <- which.max(df$views)
      max_views  <- format(df$views[idx_views], big.mark = ",")
      top_views  <- paste0(substr(df$title[idx_views], 1, 35), "…")
    } else {
      avg_autores <- 0
      avg_citas   <- 0
      avg_refs    <- 0
      max_citas   <- 0
      max_views   <- 0
      top_citado  <- "—"
      top_views   <- "—"
    }
    
    # Función interna para estructurar limpiamente cada tarjeta HTML
    make_kpi <- function(cls, label, value, sub = NULL) {
      div(class = paste("kpi-card", cls),
          div(class = "kpi-label", label),
          div(class = "kpi-value", value),
          if (!is.null(sub)) div(class = "kpi-sub", sub)
      )
    }
    
    # Retornamos todo envuelto de forma correcta dentro de un único tagList
    tagList(
      div(
        style = "font-family: 'Space Mono', monospace; font-size: 11px;
                 letter-spacing: 0.1em; text-transform: uppercase;
                 color: var(--muted); margin-bottom: 16px;",
        "Categoría: ",
        tags$span(style = "color: var(--accent); font-weight: 700;", topic_activo)
      ),
      div(class = "kpi-grid",
          make_kpi("c1", "Total artículos",          format(n_total, big.mark = ",")),
          make_kpi("c2", "Promedio autores",         avg_autores),
          make_kpi("c3", "Promedio de citas",        avg_citas),
          make_kpi("c4", "Promedio de referencias",  avg_refs),
          # card más citado — clicable
          div(class = "kpi-card c5",
              style = "cursor: pointer;",
              onclick = sprintf("Shiny.setInputValue('click_top_citado', '%s', {priority: 'event'})",
                                df$doi[which.max(df$citations)]),
              div(class = "kpi-label", "Más citado"),
              div(class = "kpi-value", max(df$citations, na.rm = TRUE)),
              div(class = "kpi-sub", title = df$title[which.max(df$citations)], top_citado)
          ),
          
          # card más descargado — clicable
          div(class = "kpi-card c6",
              style = "cursor: pointer;",
              onclick = sprintf("Shiny.setInputValue('click_top_vistas', '%s', {priority: 'event'})",
                                df$doi[which.max(df$views)]),
              div(class = "kpi-label", "Más descargado"),
              div(class = "kpi-value", format(max(df$views, na.rm = TRUE), big.mark = ",")),
              div(class = "kpi-sub", title = df$title[which.max(df$views)], top_views)
          ),
      )
    )
  })
  
  
  
  # Gráfico: evolución temporal 
  output$chart_temporal <- renderHighchart({
    df <- rv$df_filtrado
    req(df, nrow(df) > 0)
    
    df_mes <- df %>%
      mutate(mes = format(as.Date(publication_date), "%Y-%m")) %>%
      count(mes) %>%
      arrange(mes)
    
    highchart() %>%
      hc_chart(backgroundColor = "#13161e", style = list(fontFamily = "Syne")) %>%
      hc_title(text = "Publicaciones por mes",
               style = list(color = "#e8eaf0", fontSize = "14px", fontWeight = "600")) %>%
      hc_xAxis(categories = df_mes$mes,
               labels = list(
                 style = list(color = "#6b7280"),
                 rotation = -45,
                 step = max(1, floor(nrow(df_mes) / 10))
               ),
               lineColor = "#1f2433", tickColor = "#1f2433") %>%
      hc_yAxis(title = list(text = "Artículos"),
               labels = list(style = list(color = "#6b7280")),
               gridLineColor = "#1f2433") %>%
      hc_add_series(
        name = "Artículos", data = df_mes$n, type = "spline",
        color = "#00e5a0",
        marker = list(enabled = TRUE, radius = 3, fillColor = "#00e5a0"),
        lineWidth = 2
      ) %>%
      hc_tooltip(
        backgroundColor = "#0d0f14",
        style = list(color = "#e8eaf0"),
        borderColor = "#1f2433",
        pointFormat = "<b>{point.y}</b> artículos"
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_credits(enabled = FALSE)
  })
  
  # ── Gráfico: artículos por temática (ESTILIZADO) ─────────────────────────────
  output$chart_topics <- renderHighchart({
    df <- rv$df_filtrado
    req(df, nrow(df) > 0)
    
    df_top <- df %>% count(topic_label, sort = TRUE)
    
    # Nuevos colores profesionales para las barras
    colores <- c("#3b82f6", "#6366f1", "#f59e0b", "#ef4444")
    
    highchart() %>%
      hc_chart(backgroundColor = "#1e293b", style = list(fontFamily = "Inter")) %>%
      hc_title(text = "Artículos por temática",
               style = list(color = "#f8fafc", fontSize = "14px", fontFamily = "Outfit", fontWeight = "600")) %>%
      hc_add_series(
        name = "Artículos",
        data = df_top$n,
        type = "bar",
        colorByPoint = TRUE,
        colors = colores
      ) %>%
      hc_xAxis(categories = df_top$topic_label,
               labels = list(style = list(color = "#94a3b8")),
               lineColor = "#334155") %>%
      hc_yAxis(title = list(text ="Número de artículos" ),
               labels = list(style = list(color = "#94a3b8")),
               gridLineColor = "#334155") %>%
      hc_legend(enabled = FALSE) %>%
      hc_tooltip(backgroundColor = "#0f172a", style = list(color = "#f8fafc"),
                 borderColor = "#334155") %>%
      hc_credits(enabled = FALSE)
  })
  
  # ── Gráfico: top 10 autores (ESTILIZADO) ──────────────────────────────────────
  output$chart_top_autores <- renderHighchart({
    df <- rv$df_filtrado
    req(df, nrow(df) > 0)
    
    df_autores <- df %>%
      select(authors) %>%
      mutate(author = strsplit(authors, ",\\s*")) %>%
      unnest(author) %>%
      mutate(author = trimws(author)) %>%
      filter(author != "") %>%
      count(author, sort = TRUE) %>%
      slice_head(n = 10)
    
    highchart() %>%
      hc_chart(backgroundColor = "#1e293b", style = list(fontFamily = "Inter")) %>%
      hc_title(text = "Top 10 autores",
               style = list(color = "#f8fafc", fontSize = "14px", fontFamily = "Outfit", fontWeight = "600")) %>%
      hc_add_series(
        name = "Artículos", data = df_autores$n, type = "bar", 
        color = "#6366f1" # Color Índigo
      ) %>%
      hc_xAxis(categories = df_autores$author,
               labels = list(style = list(color = "#94a3b8", fontSize = "11px")),
               lineColor = "#334155") %>%
      hc_yAxis(title = list(text = "Participación en artículos"),
               labels = list(style = list(color = "#94a3b8")),
               gridLineColor = "#334155") %>%
      hc_legend(enabled = FALSE) %>%
      hc_tooltip(backgroundColor = "#0f172a", style = list(color = "#f8fafc"),
                 borderColor = "#334155") %>%
      hc_credits(enabled = FALSE)
  })
  
  # ── Gráfico: distribución de citas (ESTILIZADO) ──────────────────────────────
  output$chart_citas_dist <- renderHighchart({
    df <- rv$df_filtrado
    req(df, nrow(df) > 0)
    
    citas <- df$citations[!is.na(df$citations)]
    breaks <- c(0, 5, 10, 25, 50, 100, Inf)
    labels <- c("0-5", "6-10", "11-25", "26-50", "51-100", ">100")
    conteos <- as.integer(table(cut(citas, breaks = breaks, labels = labels, right = TRUE)))
    
    highchart() %>%
      hc_chart(backgroundColor = "#1e293b", style = list(fontFamily = "Inter")) %>%
      hc_title(text = "Distribución de citas",
               style = list(color = "#f8fafc", fontSize = "14px", fontFamily = "Outfit", fontWeight = "600")) %>%
      hc_add_series(
        name = "Artículos", data = conteos, type = "column",
        color = "#f59e0b", # Color Ámbar/Dorado suave
        borderRadius = 4
      ) %>%
      hc_xAxis(title = "Rango número de citas", categories = labels,
               labels = list(style = list(color = "#94a3b8")),
               lineColor = "#334155") %>%
      hc_yAxis(title = list(text = "Número de artículos"),
               labels = list(style = list(color = "#94a3b8")),
               gridLineColor = "#334155") %>%
      hc_legend(enabled = FALSE) %>%
      hc_tooltip(backgroundColor = "#0f172a", style = list(color = "#f8fafc"),
                 borderColor = "#334155") %>%
      hc_credits(enabled = FALSE)
  })

  # ── Tabla de artículos ────────────────────────────────────────────────────────
  output$tabla_papers <- renderDT({
    df <- rv$df_filtrado
    req(df)

    df %>%
      select(
        Título        = title,
        Autores       = authors,
        Fecha         = publication_date,
        Tema          = topic_label,
        DOI           = doi,
        Citas         = citations,
        Descargas     = views
      ) %>%
      datatable(
        options = list(
          pageLength  = 10,
          scrollX     = TRUE,
          dom         = "lfrtip",
          language    = list(
            search      = "Buscar:",
            lengthMenu  = "Mostrar _MENU_ registros",
            info        = "Mostrando _START_ a _END_ de _TOTAL_ artículos",
            paginate    = list(previous = "←", `next` = "→")
          ),
          initComplete = JS(sprintf("
            function(settings, json) {
              var doi = '%s';
              this.api().rows().every(function() {
                if (this.data()[4] === doi) {
                  $(this.node()).css('background', '#1a2a1a');
                  this.node().scrollIntoView({behavior: 'smooth', block: 'center'});
                }
              });
            }
          ", if (!is.null(rv$highlight_doi)) rv$highlight_doi else ""))
                      
            
            
            
          
        ),
        rownames  = FALSE,
        selection = "single",
        class     = "cell-border"
      )
  })

  # ── Tab: Nuevos artículos ─────────────────────────────────────────────────────
  output$nuevos_header <- renderUI({
    if (is.null(rv$n_nuevos)) {
      div(style = "color: var(--muted); font-family: 'Space Mono', monospace; font-size:13px;",
        "Haz clic en '🔍 Buscar artículos nuevos' para iniciar.")
    } else if (rv$n_nuevos == 0) {
      div(class = "scrape-box warn",
        "No se encontraron artículos nuevos en las primeras páginas de PLOS ONE.")
    } else {
      div(class = "scrape-box",
        paste0("✅ ", rv$n_nuevos, " artículo(s) nuevo(s) encontrados y almacenados en la base de datos."))
    }
  })

  output$tabla_nuevos <- renderDT({
    req(rv$df_nuevos, nrow(rv$df_nuevos) > 0)

    rv$df_nuevos %>%
      select(
        Título        = title,
        Autores       = authors,
        Fecha         = publication_date,
        Tema          = topic_label,
        DOI           = doi,
        Citas         = citations,
        Descargas     = views
      ) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX    = TRUE,
          dom        = "lfrtip"
        ),
        rownames  = FALSE,
        class     = "cell-border"
      )
  })
  
  observeEvent(input$click_top_citado, {
    updateTextInput(session, "doi_busq", value = input$click_top_citado)
    rv$df_filtrado <- get_papers_filtrados(doi_busq = input$click_top_citado)
    updateTabsetPanel(session, "tabs", selected = "📋 Artículos")
    showNotification(
      " Filtro aplicado — usa el botón 🗑limpiar en el sidebar para volver a todos los artículos.",
      type     = "message",
      duration = 5
    )
  })
  
  observeEvent(input$click_top_vistas, {
    updateTextInput(session, "doi_busq", value = input$click_top_vistas)
    rv$df_filtrado <- get_papers_filtrados(doi_busq = input$click_top_vistas)
    updateTabsetPanel(session, "tabs", selected = "📋 Artículos")
    showNotification(
      " Filtro aplicado — usa el botón 🗑limpiar en el sidebar para volver a todos los artículos.",
      type     = "message",
      duration = 5
    )
  })
  
  output$filtro_kpi_msg <- renderUI({
    req(rv$highlight_doi)
    con <- get_con()
    titulo <- dbGetQuery(con,
                         paste0("SELECT title FROM papers WHERE doi = '", rv$highlight_doi, "'"))$title
    dbDisconnect(con)
    div(
      style = "font-family: 'Space Mono', monospace; font-size: 11px;
             color: var(--accent); margin-bottom: 12px;
             padding: 8px 12px; border: 1px solid var(--border);
             border-radius: 6px; background: var(--panel);",
      " Mostrando: ", tags$strong(titulo)
    )
  })
  
  
  
}



