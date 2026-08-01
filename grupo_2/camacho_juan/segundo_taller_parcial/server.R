# ==================================================
# SERVER.R
# Taller 2 - Minería de Datos
# Lógica del dashboard
# ==================================================

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    df_filtrado = NULL,
    df_nuevos = NULL,
    n_nuevos = NULL,
    scrape_msg = NULL,
    scrape_tipo = "ok",
    highlight_doi = NULL
  )
  
  # Paleta Frontiers / estilo claro
  color_naranja <- "#ff6b00"
  color_azul <- "#2563eb"
  color_verde <- "#16a34a"
  color_morado <- "#7c3aed"
  color_gris <- "#64748b"
  fondo_grafico <- "#ffffff"
  texto_principal <- "#1f2937"
  texto_secundario <- "#6b7280"
  grid_color <- "#e5e7eb"
  
  observe({
    rv$df_filtrado <- get_papers_filtrados()
  })
  
  observeEvent(input$btn_reset, {
    updateDateRangeInput(
      session,
      "fecha_rango",
      start = "2025-01-01",
      end = Sys.Date()
    )
    
    updateSelectInput(session, "topic_sel", selected = "Todos")
    updateTextInput(session, "autor_busq", value = "")
    updateTextInput(session, "doi_busq", value = "")
    updateTextInput(session, "keyword_busq", value = "")
    
    rv$highlight_doi <- NULL
    rv$df_filtrado <- get_papers_filtrados()
  })
  
  observeEvent(input$btn_filtrar, {
    rv$highlight_doi <- NULL
    
    rv$df_filtrado <- get_papers_filtrados(
      fecha_inicio = as.character(input$fecha_rango[1]),
      fecha_fin = as.character(input$fecha_rango[2]),
      topic = input$topic_sel,
      autor = trimws(input$autor_busq),
      doi_busq = trimws(input$doi_busq),
      keyword = trimws(input$keyword_busq)
    )
  })
  
  output$kpi_cards <- renderUI({
    df <- rv$df_filtrado
    req(df)
    
    topic_activo <- if (is.null(input$topic_sel) || input$topic_sel == "Todos") {
      "Todas las categorías"
    } else {
      input$topic_sel
    }
    
    n_total <- nrow(df)
    
    if (n_total > 0) {
      avg_autores <- round(mean(df$n_authors, na.rm = TRUE), 1)
      avg_citas <- round(mean(df$citations, na.rm = TRUE), 1)
      avg_refs <- round(mean(df$n_references, na.rm = TRUE), 1)
      
      idx_citas <- which.max(df$citations)
      max_citas <- df$citations[idx_citas]
      top_citado <- paste0(substr(df$title[idx_citas], 1, 35), "…")
      
      idx_down <- which.max(df$downloads)
      max_down <- format(df$downloads[idx_down], big.mark = ",")
      top_down <- paste0(substr(df$title[idx_down], 1, 35), "…")
    } else {
      avg_autores <- 0
      avg_citas <- 0
      avg_refs <- 0
      max_citas <- 0
      max_down <- 0
      top_citado <- "—"
      top_down <- "—"
    }
    
    make_kpi <- function(cls, label, value, sub = NULL) {
      div(
        class = paste("kpi-card", cls),
        div(class = "kpi-label", label),
        div(class = "kpi-value", value),
        if (!is.null(sub)) div(class = "kpi-sub", sub)
      )
    }
    
    tagList(
      div(
        style = "font-family: 'Montserrat', sans-serif; font-size: 11px;
                 letter-spacing: 0.1em; text-transform: uppercase;
                 color: var(--muted); margin-bottom: 16px;",
        "Categoría: ",
        tags$span(style = "color: var(--accent); font-weight: 700;", topic_activo)
      ),
      
      div(
        class = "kpi-grid",
        make_kpi("c1", "Total artículos", format(n_total, big.mark = ",")),
        make_kpi("c2", "Promedio autores", avg_autores),
        make_kpi("c3", "Promedio de citas", avg_citas),
        make_kpi("c4", "Promedio de referencias", avg_refs),
        
        div(
          class = "kpi-card c5",
          style = "cursor: pointer;",
          onclick = if (n_total > 0) {
            sprintf(
              "Shiny.setInputValue('click_top_citado', '%s', {priority: 'event'})",
              df$doi[idx_citas]
            )
          },
          div(class = "kpi-label", "Más citado"),
          div(class = "kpi-value", max_citas),
          div(class = "kpi-sub", top_citado)
        ),
        
        div(
          class = "kpi-card c6",
          style = "cursor: pointer;",
          onclick = if (n_total > 0) {
            sprintf(
              "Shiny.setInputValue('click_top_descargado', '%s', {priority: 'event'})",
              df$doi[idx_down]
            )
          },
          div(class = "kpi-label", "Más descargado"),
          div(class = "kpi-value", max_down),
          div(class = "kpi-sub", top_down)
        )
      )
    )
  })
  
  # ==================================================
  # PUBLICACIONES POR MES
  # ==================================================
  
  output$chart_temporal <- renderHighchart({
    df <- rv$df_filtrado
    req(df, nrow(df) > 0)
    
    df_mes <- df %>%
      mutate(mes = format(publication_date_clean, "%Y-%m")) %>%
      count(mes) %>%
      arrange(mes)
    
    highchart() %>%
      hc_chart(backgroundColor = fondo_grafico, style = list(fontFamily = "Montserrat")) %>%
      hc_title(
        text = "Publicaciones por mes",
        style = list(color = texto_principal, fontSize = "16px", fontWeight = "700")
      ) %>%
      hc_xAxis(
        categories = df_mes$mes,
        labels = list(style = list(color = texto_secundario), rotation = -45),
        lineColor = grid_color,
        tickColor = grid_color
      ) %>%
      hc_yAxis(
        title = list(text = "Artículos", style = list(color = texto_secundario)),
        labels = list(style = list(color = texto_secundario)),
        gridLineColor = grid_color
      ) %>%
      hc_add_series(
        name = "Artículos",
        data = df_mes$n,
        type = "spline",
        color = color_naranja,
        marker = list(enabled = TRUE, radius = 4),
        lineWidth = 3
      ) %>%
      hc_tooltip(
        backgroundColor = "#ffffff",
        style = list(color = texto_principal),
        borderColor = grid_color,
        pointFormat = "<b>{point.y}</b> artículos"
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_credits(enabled = FALSE)
  })
  
  # ==================================================
  # ARTÍCULOS POR CATEGORÍA
  # ==================================================
  
  output$chart_topics <- renderHighchart({
    df <- rv$df_filtrado
    req(df, nrow(df) > 0)
    
    df_topic <- df %>%
      count(topic_label, sort = TRUE)
    
    highchart() %>%
      hc_chart(backgroundColor = fondo_grafico, style = list(fontFamily = "Montserrat")) %>%
      hc_title(
        text = "Artículos por categoría",
        style = list(color = texto_principal, fontSize = "16px", fontWeight = "700")
      ) %>%
      hc_add_series(
        name = "Artículos",
        data = df_topic$n,
        type = "bar",
        colorByPoint = TRUE,
        colors = c(color_naranja, color_azul, color_verde, color_morado, color_gris)
      ) %>%
      hc_xAxis(
        categories = df_topic$topic_label,
        labels = list(style = list(color = texto_secundario)),
        lineColor = grid_color
      ) %>%
      hc_yAxis(
        title = list(text = "Número de artículos", style = list(color = texto_secundario)),
        labels = list(style = list(color = texto_secundario)),
        gridLineColor = grid_color
      ) %>%
      hc_tooltip(
        backgroundColor = "#ffffff",
        style = list(color = texto_principal),
        borderColor = grid_color
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_credits(enabled = FALSE)
  })
  
  # ==================================================
  # DESCARGAS POR CATEGORÍA
  # ==================================================
  
  output$chart_descargas_categoria <- renderHighchart({
    df <- rv$df_filtrado
    req(df, nrow(df) > 0)
    
    df_down <- df %>%
      group_by(topic_label) %>%
      summarise(descargas = sum(downloads, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(descargas))
    
    highchart() %>%
      hc_chart(type = "column", backgroundColor = fondo_grafico, style = list(fontFamily = "Montserrat")) %>%
      hc_title(
        text = "Descargas por categoría",
        style = list(color = texto_principal, fontSize = "16px", fontWeight = "700")
      ) %>%
      hc_xAxis(
        categories = df_down$topic_label,
        labels = list(style = list(color = texto_secundario)),
        lineColor = grid_color
      ) %>%
      hc_yAxis(
        title = list(text = "Total de descargas", style = list(color = texto_secundario)),
        labels = list(style = list(color = texto_secundario)),
        gridLineColor = grid_color
      ) %>%
      hc_add_series(
        name = "Descargas",
        data = df_down$descargas,
        color = color_naranja,
        borderRadius = 5
      ) %>%
      hc_tooltip(
        backgroundColor = "#ffffff",
        style = list(color = texto_principal),
        borderColor = grid_color,
        pointFormat = "<b>{point.y}</b> descargas"
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_credits(enabled = FALSE)
  })
  
  # ==================================================
  # TOP 10 ARTÍCULOS MÁS DESCARGADOS
  # ==================================================
  
  output$chart_top_descargados <- renderHighchart({
    df <- rv$df_filtrado
    req(df, nrow(df) > 0)
    
    df_top <- df %>%
      arrange(desc(downloads)) %>%
      slice_head(n = 10) %>%
      mutate(titulo_corto = paste0(substr(title, 1, 55), "…")) %>%
      arrange(downloads)
    
    highchart() %>%
      hc_chart(type = "bar", backgroundColor = fondo_grafico, style = list(fontFamily = "Montserrat")) %>%
      hc_title(
        text = "Top 10 artículos más descargados",
        style = list(color = texto_principal, fontSize = "16px", fontWeight = "700")
      ) %>%
      hc_xAxis(
        categories = df_top$titulo_corto,
        labels = list(style = list(color = texto_secundario, fontSize = "11px")),
        lineColor = grid_color
      ) %>%
      hc_yAxis(
        title = list(text = "Descargas", style = list(color = texto_secundario)),
        labels = list(style = list(color = texto_secundario)),
        gridLineColor = grid_color
      ) %>%
      hc_add_series(
        name = "Descargas",
        data = df_top$downloads,
        color = color_azul,
        borderRadius = 5
      ) %>%
      hc_tooltip(
        backgroundColor = "#ffffff",
        style = list(color = texto_principal),
        borderColor = grid_color,
        pointFormat = "<b>{point.y}</b> descargas"
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_credits(enabled = FALSE)
  })
  
  # ==================================================
  # TOP 10 ARTÍCULOS MÁS CITADOS
  # ==================================================
  
  output$chart_top_citados <- renderHighchart({
    df <- rv$df_filtrado
    req(df, nrow(df) > 0)
    
    df_top <- df %>%
      arrange(desc(citations)) %>%
      slice_head(n = 10) %>%
      mutate(titulo_corto = paste0(substr(title, 1, 55), "…")) %>%
      arrange(citations)
    
    highchart() %>%
      hc_chart(type = "bar", backgroundColor = fondo_grafico, style = list(fontFamily = "Montserrat")) %>%
      hc_title(
        text = "Top 10 artículos más citados",
        style = list(color = texto_principal, fontSize = "16px", fontWeight = "700")
      ) %>%
      hc_xAxis(
        categories = df_top$titulo_corto,
        labels = list(style = list(color = texto_secundario, fontSize = "11px")),
        lineColor = grid_color
      ) %>%
      hc_yAxis(
        title = list(text = "Citas", style = list(color = texto_secundario)),
        labels = list(style = list(color = texto_secundario)),
        gridLineColor = grid_color
      ) %>%
      hc_add_series(
        name = "Citas",
        data = df_top$citations,
        color = color_verde,
        borderRadius = 5
      ) %>%
      hc_tooltip(
        backgroundColor = "#ffffff",
        style = list(color = texto_principal),
        borderColor = grid_color,
        pointFormat = "<b>{point.y}</b> citas"
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_credits(enabled = FALSE)
  })
  
  # ==================================================
  # RELEVANCIA DE PAPERS
  # ==================================================
  
  output$chart_relevancia <- renderHighchart({
    df <- rv$df_filtrado
    req(df, nrow(df) > 0)
    
    df_rel <- df %>%
      mutate(
        n_authors = ifelse(is.na(n_authors), 0, n_authors),
        n_references = ifelse(is.na(n_references), 0, n_references),
        downloads = ifelse(is.na(downloads), 0, downloads),
        citations = ifelse(is.na(citations), 0, citations),
        relevancia = citations + log1p(downloads)
      ) %>%
      filter(n_authors >= 0, n_references >= 0)
    
    series_list <- lapply(unique(df_rel$topic_label), function(cat) {
      datos <- df_rel %>%
        filter(topic_label == cat) %>%
        transmute(
          x = n_authors,
          y = n_references,
          z = pmax(downloads, 1),
          name = title,
          doi = doi,
          citations = citations,
          downloads = downloads
        )
      
      list(
        name = cat,
        data = list_parse(datos),
        type = "bubble"
      )
    })
    
    highchart() %>%
      hc_chart(type = "bubble", backgroundColor = fondo_grafico, style = list(fontFamily = "Montserrat")) %>%
      hc_title(
        text = "Relevancia de papers publicados",
        style = list(color = texto_principal, fontSize = "16px", fontWeight = "700")
      ) %>%
      hc_xAxis(
        title = list(text = "Número de autores", style = list(color = texto_secundario)),
        labels = list(style = list(color = texto_secundario)),
        gridLineColor = grid_color
      ) %>%
      hc_yAxis(
        title = list(text = "Número de referencias", style = list(color = texto_secundario)),
        labels = list(style = list(color = texto_secundario)),
        gridLineColor = grid_color
      ) %>%
      hc_plotOptions(
        bubble = list(
          minSize = 5,
          maxSize = 45,
          opacity = 0.65
        )
      ) %>%
      hc_colors(c(color_naranja, color_azul, color_verde, color_morado, color_gris)) %>%
      hc_add_series_list(series_list) %>%
      hc_tooltip(
        useHTML = TRUE,
        backgroundColor = "#ffffff",
        borderColor = grid_color,
        style = list(color = texto_principal),
        pointFormat = paste0(
          "<b>{point.name}</b><br>",
          "Autores: {point.x}<br>",
          "Referencias: {point.y}<br>",
          "Descargas: {point.downloads}<br>",
          "Citas: {point.citations}<br>",
          "DOI: {point.doi}"
        )
      ) %>%
      hc_credits(enabled = FALSE)
  })
  
  # ==================================================
  # TABLA DE ARTÍCULOS
  # ==================================================
  
  output$tabla_papers <- renderDT({
    df <- rv$df_filtrado
    req(df)
    
    df %>%
      select(
        Título = title,
        Autores = authors_raw,
        Fecha = publication_date,
        Tema = topic_label,
        DOI = doi,
        Citas = citations,
        Descargas = downloads
      ) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "lfrtip",
          language = list(
            search = "Buscar:",
            lengthMenu = "Mostrar _MENU_ registros",
            info = "Mostrando _START_ a _END_ de _TOTAL_ artículos",
            paginate = list(previous = "←", `next` = "→")
          )
        ),
        rownames = FALSE,
        selection = "single",
        class = "cell-border"
      )
  })
  
  # ==================================================
  # SCRAPING
  # ==================================================
  
  observeEvent(input$btn_scrape, {
    rv$scrape_msg <- "Buscando artículos nuevos..."
    rv$scrape_tipo <- "ok"
    rv$df_nuevos <- NULL
    rv$n_nuevos <- NULL
    
    resultado <- buscar_articulos_nuevos(anio_inicio = 2026, max_paginas = 5)
    
    if (resultado$nuevos == 0) {
      rv$scrape_msg <- paste0(
        "No se encontraron artículos nuevos. Se muestran los últimos 5 artículos almacenados."
      )
      rv$scrape_tipo <- "warn"
      rv$df_nuevos <- resultado$df_nuevos
      rv$n_nuevos <- 0
    } else {
      rv$scrape_msg <- paste0(
        "✅ Se encontraron y almacenaron ",
        resultado$nuevos,
        " artículo(s) nuevo(s)."
      )
      rv$scrape_tipo <- "ok"
      rv$df_nuevos <- resultado$df_nuevos
      rv$n_nuevos <- resultado$nuevos
      rv$df_filtrado <- get_papers_filtrados()
    }
  })
  
  output$scrape_msg <- renderUI({
    req(rv$scrape_msg)
    cls <- if (rv$scrape_tipo == "warn") "scrape-box warn" else "scrape-box"
    div(class = cls, rv$scrape_msg)
  })
  
  output$nuevos_header <- renderUI({
    if (is.null(rv$n_nuevos)) {
      div(
        style = "color: var(--muted); font-family: 'Montserrat', sans-serif; font-size:13px;",
        "Haz clic en 'Actualizar base de datos' para iniciar."
      )
    } else if (rv$n_nuevos == 0) {
      div(class = "scrape-box warn", "No se encontraron artículos nuevos.")
    } else {
      div(class = "scrape-box", paste0("✅ ", rv$n_nuevos, " artículo(s) nuevo(s) encontrados."))
    }
  })
  
  output$tabla_nuevos <- renderDT({
    req(rv$df_nuevos, nrow(rv$df_nuevos) > 0)
    
    rv$df_nuevos %>%
      select(
        Título = title,
        Autores = authors_raw,
        Fecha = publication_date,
        Tema = topic_label,
        DOI = doi,
        Citas = citations,
        Descargas = downloads
      ) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        class = "cell-border"
      )
  })
  
  # ==================================================
  # CLIC EN KPI
  # ==================================================
  
  observeEvent(input$click_top_citado, {
    rv$highlight_doi <- input$click_top_citado
    updateTextInput(session, "doi_busq", value = input$click_top_citado)
    rv$df_filtrado <- get_papers_filtrados(doi_busq = input$click_top_citado)
    updateTabsetPanel(session, "tabs", selected = "Artículos")
  })
  
  observeEvent(input$click_top_descargado, {
    rv$highlight_doi <- input$click_top_descargado
    updateTextInput(session, "doi_busq", value = input$click_top_descargado)
    rv$df_filtrado <- get_papers_filtrados(doi_busq = input$click_top_descargado)
    updateTabsetPanel(session, "tabs", selected = "Artículos")
  })
  
  output$filtro_kpi_msg <- renderUI({
    req(rv$highlight_doi)
    div(
      style = "font-family: 'Montserrat', sans-serif; font-size: 11px;
               color: var(--accent); margin-bottom: 12px;
               padding: 8px 12px; border: 1px solid var(--border);
               border-radius: 6px; background: var(--panel);",
      "Mostrando artículo filtrado por DOI: ",
      tags$strong(rv$highlight_doi)
    )
  })

  }

