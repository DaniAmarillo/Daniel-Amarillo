# ---------------------------------------------------------
# SERVER
# ---------------------------------------------------------

server <- function(input, output, session) {
  
  datos_reactivos <- reactiveVal(papers_base)
  resultados_actualizacion <- reactiveVal(data.frame())
  
  # -------------------------------------------------------
  # Estado de la base
  # -------------------------------------------------------
  
  estado_base <- reactive({
    d <- datos_reactivos()
    
    total <- nrow(d)
    
    doi_unicos <- d %>%
      filter(!is.na(doi), doi != "") %>%
      distinct(doi) %>%
      nrow()
    
    doi_duplicados <- d %>%
      filter(!is.na(doi), doi != "") %>%
      count(doi) %>%
      filter(n > 1) %>%
      nrow()
    
    fecha_min <- min(d$publication_date, na.rm = TRUE)
    fecha_max <- max(d$publication_date, na.rm = TRUE)
    
    data.frame(
      Indicador = c(
        "Total de artículos",
        "DOI únicos",
        "DOI duplicados",
        "Fecha inicial",
        "Fecha final",
        "Categorías temáticas",
        "Promedio de citas",
        "Promedio de referencias"
      ),
      Valor = c(
        total,
        doi_unicos,
        doi_duplicados,
        as.character(fecha_min),
        as.character(fecha_max),
        length(unique(d$topic_label)),
        round(mean(d$citations, na.rm = TRUE), 2),
        round(mean(d$n_references, na.rm = TRUE), 2)
      ),
      stringsAsFactors = FALSE
    )
  })
  
  output$estado_total <- renderText({
    nrow(datos_reactivos())
  })
  
  output$estado_doi_unicos <- renderText({
    datos_reactivos() %>%
      filter(!is.na(doi), doi != "") %>%
      distinct(doi) %>%
      nrow()
  })
  
  output$estado_doi_duplicados <- renderText({
    datos_reactivos() %>%
      filter(!is.na(doi), doi != "") %>%
      count(doi) %>%
      filter(n > 1) %>%
      nrow()
  })
  
  output$estado_fecha_max <- renderText({
    as.character(max(datos_reactivos()$publication_date, na.rm = TRUE))
  })
  
  output$tabla_estado_base <- renderDT({
    datatable(
      estado_base(),
      rownames = FALSE,
      options = list(
        pageLength = 8,
        dom = "t",
        language = list(
          url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"
        )
      )
    )
  })
  
  output$grafico_estado_anios <- renderHighchart({
    datos <- datos_reactivos() %>%
      count(year, name = "cantidad") %>%
      arrange(year)
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Artículos por año") %>%
      hc_subtitle(text = "Verificación de actualización 2025–2026") %>%
      hc_xAxis(categories = as.character(datos$year), title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Artículos")) %>%
      hc_add_series(
        name = "Artículos",
        data = datos$cantidad,
        color = "#e10600"
      ) %>%
      hc_tooltip(pointFormat = "<b>{point.y}</b> artículos") %>%
      hc_plotOptions(column = list(borderRadius = 0)) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$grafico_estado_temas <- renderHighchart({
    datos <- datos_reactivos() %>%
      count(topic_label, name = "cantidad") %>%
      arrange(desc(cantidad))
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Composición temática") %>%
      hc_subtitle(text = "Distribución de categorías en la base completa") %>%
      hc_xAxis(categories = datos$topic_label, title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Artículos")) %>%
      hc_add_series(
        name = "Artículos",
        data = datos$cantidad,
        color = "#0a0a0a"
      ) %>%
      hc_tooltip(pointFormat = "<b>{point.y}</b> artículos") %>%
      hc_plotOptions(bar = list(borderRadius = 0)) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  # -------------------------------------------------------
  # Resumen reactivo del inicio
  # -------------------------------------------------------
  
  output$inicio_resumen_base <- renderText({
    d <- datos_reactivos()
    
    paste0(
      nrow(d), " artículos indexados · ",
      length(unique(d$topic_label)), " categorías · ",
      min(d$year, na.rm = TRUE), "–",
      max(d$year, na.rm = TRUE)
    )
  })
  
  output$inicio_tabla_activa <- renderText({
    paste0("Tabla: papers · ", nrow(datos_reactivos()), " registros")
  })
  
  output$inicio_total <- renderText({
    nrow(datos_reactivos())
  })
  
  output$inicio_categorias <- renderText({
    length(unique(datos_reactivos()$topic_label))
  })
  
  output$inicio_anio_inicial <- renderText({
    min(datos_reactivos()$year, na.rm = TRUE)
  })
  
  output$inicio_anio_final <- renderText({
    max(datos_reactivos()$year, na.rm = TRUE)
  })
  
  # -------------------------------------------------------
  # Estado de filtros
  # -------------------------------------------------------
  
  filtros_aplicados <- reactiveVal(list(
    fecha = c(
      min(papers_base$publication_date, na.rm = TRUE),
      max(papers_base$publication_date, na.rm = TRUE)
    ),
    tema = "Todos",
    autor = "Todos",
    doi = "",
    palabra = ""
  ))
  
  observeEvent(input$aplicar_filtros, {
    filtros_aplicados(list(
      fecha = input$fecha,
      tema = input$tema,
      autor = input$autor,
      doi = input$doi,
      palabra = input$palabra
    ))
  })
  
  observeEvent(input$limpiar, {
    d1 <- min(datos_reactivos()$publication_date, na.rm = TRUE)
    d2 <- max(datos_reactivos()$publication_date, na.rm = TRUE)
    
    updateDateRangeInput(session, "fecha", start = d1, end = d2)
    updateSelectInput(session, "tema", selected = "Todos")
    updateSelectizeInput(session, "autor", selected = "Todos")
    updateTextInput(session, "doi", value = "")
    updateTextInput(session, "palabra", value = "")
    
    filtros_aplicados(list(
      fecha = c(d1, d2),
      tema = "Todos",
      autor = "Todos",
      doi = "",
      palabra = ""
    ))
  })
  
  # -------------------------------------------------------
  # Datos filtrados
  # -------------------------------------------------------
  
  papers_filtrados <- reactive({
    datos <- datos_reactivos()
    filtros <- filtros_aplicados()
    
    if (!is.null(filtros$fecha) && length(filtros$fecha) == 2) {
      datos <- datos %>%
        filter(
          publication_date >= filtros$fecha[1],
          publication_date <= filtros$fecha[2]
        )
    }
    
    if (!is.null(filtros$tema) && filtros$tema != "Todos") {
      datos <- datos %>%
        filter(topic_label == filtros$tema)
    }
    
    if (!is.null(filtros$autor) && filtros$autor != "" && filtros$autor != "Todos") {
      datos <- datos %>%
        filter(str_detect(str_to_lower(authors_raw), str_to_lower(filtros$autor)))
    }
    
    if (!is.null(filtros$doi) && filtros$doi != "") {
      datos <- datos %>%
        filter(str_detect(str_to_lower(doi), str_to_lower(filtros$doi)))
    }
    
    if (!is.null(filtros$palabra) && filtros$palabra != "") {
      p <- str_to_lower(filtros$palabra)
      
      datos <- datos %>%
        filter(
          str_detect(str_to_lower(title), p) |
            str_detect(str_to_lower(abstract), p)
        )
    }
    
    datos
  })
  
  # -------------------------------------------------------
  # Indicadores
  # -------------------------------------------------------
  
  output$total_articulos <- renderText({
    nrow(papers_filtrados())
  })
  
  output$prom_autores <- renderText({
    v <- mean(papers_filtrados()$n_authors, na.rm = TRUE)
    ifelse(is.nan(v), "0", round(v, 1))
  })
  
  output$prom_citas <- renderText({
    v <- mean(papers_filtrados()$citations, na.rm = TRUE)
    ifelse(is.nan(v), "0", round(v, 1))
  })
  
  output$prom_referencias <- renderText({
    v <- mean(papers_filtrados()$n_references, na.rm = TRUE)
    ifelse(is.nan(v), "0", round(v, 1))
  })
  
  output$articulo_mas_citado <- renderText({
    d <- papers_filtrados()
    
    if (nrow(d) == 0) {
      return("Sin datos")
    }
    
    d %>%
      arrange(desc(citations)) %>%
      slice(1) %>%
      pull(title)
  })
  
  output$meta_mas_citado <- renderText({
    d <- papers_filtrados()
    
    if (nrow(d) == 0) {
      return("")
    }
    
    a <- d %>%
      arrange(desc(citations)) %>%
      slice(1)
    
    paste0(
      "Citas: ", a$citations,
      " · Tema: ", a$topic_label,
      " · DOI: ", a$doi
    )
  })
  
  output$categoria_mayor <- renderText({
    d <- papers_filtrados()
    
    if (nrow(d) == 0) {
      return("Sin datos")
    }
    
    d %>%
      count(topic_label) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(topic_label)
  })
  
  output$meta_categoria_mayor <- renderText({
    d <- papers_filtrados()
    
    if (nrow(d) == 0) {
      return("")
    }
    
    f <- d %>%
      count(topic_label) %>%
      arrange(desc(n)) %>%
      slice(1)
    
    paste0("Artículos: ", f$n)
  })
  
  # =======================================================
  # Estilo global para gráficas — Urban editorial
  # =======================================================
  
  hc_theme_urban <- function() {
    hc_theme(
      chart = list(
        backgroundColor = "#ffffff",
        style = list(
          fontFamily = "DM Sans, Inter, Arial, sans-serif"
        )
      ),
      title = list(
        style = list(
          color = "#0a0a0a",
          fontFamily = "DM Sans, Inter, Arial, sans-serif",
          fontWeight = "900",
          fontSize = "19px"
        )
      ),
      subtitle = list(
        style = list(
          color = "#606060",
          fontSize = "12px"
        )
      ),
      xAxis = list(
        gridLineColor = "#eeeeee",
        lineColor = "#0a0a0a",
        tickColor = "#0a0a0a",
        labels = list(style = list(color = "#333333", fontSize = "11px")),
        title = list(style = list(color = "#333333", fontSize = "12px"))
      ),
      yAxis = list(
        gridLineColor = "#eeeeee",
        lineColor = "#0a0a0a",
        tickColor = "#0a0a0a",
        labels = list(style = list(color = "#333333", fontSize = "11px")),
        title = list(style = list(color = "#333333", fontSize = "12px"))
      ),
      legend = list(
        itemStyle = list(color = "#0a0a0a", fontWeight = "700"),
        itemHoverStyle = list(color = "#e10600")
      ),
      tooltip = list(
        backgroundColor = "#0a0a0a",
        borderColor = "#e10600",
        style = list(color = "#ffffff")
      )
    )
  }
  
  urban_palette <- c("#0a0a0a", "#e10600", "#6f6f6f", "#b8b8b8", "#2b2b2b")
  
  # -------------------------------------------------------
  # Gráfica 1: artículos por tema
  # -------------------------------------------------------
  
  output$grafico_temas <- renderHighchart({
    datos <- papers_filtrados() %>%
      count(topic_label, name = "cantidad") %>%
      arrange(desc(cantidad))
    
    if (nrow(datos) == 0) {
      return(
        highchart() %>%
          hc_title(text = "Artículos por categoría") %>%
          hc_subtitle(text = "Sin datos para los filtros seleccionados") %>%
          hc_add_theme(hc_theme_urban())
      )
    }
    
    hchart(datos, "column", hcaes(x = topic_label, y = cantidad), name = "Artículos") %>%
      hc_title(text = "Artículos por categoría") %>%
      hc_subtitle(text = "Distribución de publicaciones según clasificación temática") %>%
      hc_xAxis(title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Número de artículos")) %>%
      hc_tooltip(pointFormat = "<b>{point.y}</b> artículos") %>%
      hc_colors(urban_palette) %>%
      hc_plotOptions(
        column = list(
          colorByPoint = TRUE,
          borderRadius = 0,
          pointPadding = 0.12,
          groupPadding = 0.10,
          dataLabels = list(
            enabled = TRUE,
            color = "#0a0a0a",
            style = list(textOutline = "none", fontWeight = "900")
          )
        ),
        series = list(
          cursor = "pointer",
          states = list(
            hover = list(brightness = -0.10)
          )
        )
      ) %>%
      hc_add_theme(hc_theme_urban()) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # -------------------------------------------------------
  # Gráfica 2: evolución temporal
  # -------------------------------------------------------
  
  output$grafico_tiempo <- renderHighchart({
    datos <- papers_filtrados() %>%
      mutate(mes = floor_date(publication_date, unit = "month")) %>%
      count(mes, name = "cantidad") %>%
      arrange(mes)
    
    if (nrow(datos) == 0) {
      return(
        highchart() %>%
          hc_title(text = "Evolución temporal") %>%
          hc_subtitle(text = "Sin datos para los filtros seleccionados") %>%
          hc_add_theme(hc_theme_urban())
      )
    }
    
    highchart() %>%
      hc_chart(type = "areaspline", zoomType = "x") %>%
      hc_title(text = "Evolución temporal") %>%
      hc_subtitle(text = "Publicaciones por mes") %>%
      hc_xAxis(type = "datetime", title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Artículos")) %>%
      hc_add_series(
        data = list_parse2(
          datos %>%
            transmute(
              x = datetime_to_timestamp(mes),
              y = cantidad
            )
        ),
        name = "Publicaciones",
        color = "#e10600",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 0, x2 = 0, y2 = 1),
          stops = list(
            list(0, "rgba(225,6,0,0.35)"),
            list(1, "rgba(225,6,0,0.02)")
          )
        ),
        lineWidth = 4,
        marker = list(
          radius = 5,
          fillColor = "#ffffff",
          lineColor = "#e10600",
          lineWidth = 2
        )
      ) %>%
      hc_tooltip(pointFormat = "<b>{point.y}</b> publicaciones") %>%
      hc_add_theme(hc_theme_urban()) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # -------------------------------------------------------
  # Gráfica 3: distribución de citas
  # -------------------------------------------------------
  
  output$grafico_citas <- renderHighchart({
    datos <- papers_filtrados() %>%
      filter(!is.na(citations)) %>%
      mutate(citations = as.numeric(citations))
    
    if (nrow(datos) == 0) {
      return(
        highchart() %>%
          hc_title(text = "Distribución de citas") %>%
          hc_subtitle(text = "Sin datos para los filtros seleccionados") %>%
          hc_add_theme(hc_theme_urban())
      )
    }
    
    hchart(datos$citations, breaks = 14, name = "Artículos") %>%
      hc_title(text = "Distribución de citas") %>%
      hc_subtitle(text = "Frecuencia de artículos según número de citas") %>%
      hc_xAxis(title = list(text = "Citas")) %>%
      hc_yAxis(title = list(text = "Frecuencia")) %>%
      hc_colors(c("#0a0a0a")) %>%
      hc_plotOptions(
        column = list(
          borderRadius = 0,
          pointPadding = 0.04,
          groupPadding = 0.02
        )
      ) %>%
      hc_add_theme(hc_theme_urban()) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # -------------------------------------------------------
  # Gráfica 4: top autores
  # -------------------------------------------------------
  
  output$grafico_top_autores <- renderHighchart({
    datos <- papers_filtrados() %>%
      select(authors_raw) %>%
      filter(!is.na(authors_raw), authors_raw != "") %>%
      mutate(author = str_split(authors_raw, ";")) %>%
      tidyr::unnest(author) %>%
      mutate(author = str_trim(author)) %>%
      filter(author != "") %>%
      count(author, name = "publicaciones") %>%
      arrange(desc(publicaciones)) %>%
      slice_head(n = 15)
    
    if (nrow(datos) == 0) {
      return(
        highchart() %>%
          hc_title(text = "Top autores") %>%
          hc_subtitle(text = "Sin datos para los filtros seleccionados") %>%
          hc_add_theme(hc_theme_urban())
      )
    }
    
    hchart(datos, "bar", hcaes(x = author, y = publicaciones), name = "Publicaciones") %>%
      hc_title(text = "Top 15 autores") %>%
      hc_subtitle(text = "Autores con mayor presencia en los artículos filtrados") %>%
      hc_xAxis(title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Publicaciones")) %>%
      hc_tooltip(pointFormat = "<b>{point.y}</b> publicaciones") %>%
      hc_colors(c("#e10600")) %>%
      hc_plotOptions(
        bar = list(
          borderRadius = 0,
          pointPadding = 0.08,
          groupPadding = 0.06,
          dataLabels = list(
            enabled = TRUE,
            color = "#0a0a0a",
            style = list(textOutline = "none", fontWeight = "900")
          )
        )
      ) %>%
      hc_add_theme(hc_theme_urban()) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # -------------------------------------------------------
  # Gráfica 5: citas por tema
  # -------------------------------------------------------
  
  output$grafico_citas_tema <- renderHighchart({
    datos <- papers_filtrados() %>%
      group_by(topic_label) %>%
      summarise(
        promedio_citas = round(mean(citations, na.rm = TRUE), 2),
        max_citas = max(citations, na.rm = TRUE),
        total_articulos = n(),
        .groups = "drop"
      ) %>%
      filter(!is.nan(promedio_citas), is.finite(max_citas)) %>%
      arrange(desc(promedio_citas))
    
    if (nrow(datos) == 0) {
      return(
        highchart() %>%
          hc_title(text = "Citas por tema") %>%
          hc_subtitle(text = "Sin datos para los filtros seleccionados") %>%
          hc_add_theme(hc_theme_urban())
      )
    }
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Citas por categoría") %>%
      hc_subtitle(text = "Promedio y máximo de citas por tema") %>%
      hc_xAxis(categories = datos$topic_label, title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Citas")) %>%
      hc_add_series(
        name = "Promedio",
        data = datos$promedio_citas,
        color = "#0a0a0a",
        type = "column"
      ) %>%
      hc_add_series(
        name = "Máximo",
        data = datos$max_citas,
        color = "#e10600",
        type = "spline",
        lineWidth = 4,
        marker = list(radius = 5)
      ) %>%
      hc_tooltip(shared = TRUE) %>%
      hc_plotOptions(column = list(borderRadius = 0)) %>%
      hc_add_theme(hc_theme_urban()) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # -------------------------------------------------------
  # Gráfica 6: referencias promedio por tema
  # -------------------------------------------------------
  
  output$grafico_referencias_tema <- renderHighchart({
    datos <- papers_filtrados() %>%
      group_by(topic_label) %>%
      summarise(
        promedio_referencias = round(mean(n_references, na.rm = TRUE), 2),
        total_articulos = n(),
        .groups = "drop"
      ) %>%
      filter(!is.nan(promedio_referencias)) %>%
      arrange(desc(promedio_referencias))
    
    if (nrow(datos) == 0) {
      return(
        highchart() %>%
          hc_title(text = "Referencias promedio") %>%
          hc_subtitle(text = "Sin datos para los filtros seleccionados") %>%
          hc_add_theme(hc_theme_urban())
      )
    }
    
    hchart(
      datos,
      "column",
      hcaes(x = topic_label, y = promedio_referencias),
      name = "Referencias promedio"
    ) %>%
      hc_title(text = "Referencias promedio por categoría") %>%
      hc_subtitle(text = "Densidad bibliográfica por tema") %>%
      hc_xAxis(title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Referencias promedio")) %>%
      hc_tooltip(pointFormat = "<b>{point.y}</b> referencias promedio") %>%
      hc_colors(c("#0a0a0a", "#e10600", "#6f6f6f", "#b8b8b8")) %>%
      hc_plotOptions(
        column = list(
          colorByPoint = TRUE,
          borderRadius = 0,
          dataLabels = list(
            enabled = TRUE,
            style = list(textOutline = "none", fontWeight = "900")
          )
        )
      ) %>%
      hc_add_theme(hc_theme_urban()) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # -------------------------------------------------------
  # Gráfica 7: autores promedio por tema
  # -------------------------------------------------------
  
  output$grafico_autores_tema <- renderHighchart({
    datos <- papers_filtrados() %>%
      group_by(topic_label) %>%
      summarise(
        promedio_autores = round(mean(n_authors, na.rm = TRUE), 2),
        total_articulos = n(),
        .groups = "drop"
      ) %>%
      filter(!is.nan(promedio_autores)) %>%
      arrange(desc(promedio_autores))
    
    if (nrow(datos) == 0) {
      return(
        highchart() %>%
          hc_title(text = "Autores promedio") %>%
          hc_subtitle(text = "Sin datos para los filtros seleccionados") %>%
          hc_add_theme(hc_theme_urban())
      )
    }
    
    hchart(
      datos,
      "bar",
      hcaes(x = topic_label, y = promedio_autores),
      name = "Autores promedio"
    ) %>%
      hc_title(text = "Colaboración por categoría") %>%
      hc_subtitle(text = "Promedio de autores por artículo") %>%
      hc_xAxis(title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Autores promedio")) %>%
      hc_tooltip(pointFormat = "<b>{point.y}</b> autores promedio") %>%
      hc_colors(c("#e10600")) %>%
      hc_plotOptions(
        bar = list(
          borderRadius = 0,
          dataLabels = list(enabled = TRUE)
        )
      ) %>%
      hc_add_theme(hc_theme_urban()) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # -------------------------------------------------------
  # Gráfica 8: citas vs referencias
  # -------------------------------------------------------
  
  output$grafico_scatter_citas_refs <- renderHighchart({
    datos <- papers_filtrados() %>%
      filter(!is.na(citations), !is.na(n_references)) %>%
      mutate(
        title_short = str_trunc(title, 75),
        citations = as.numeric(citations),
        n_references = as.numeric(n_references)
      )
    
    if (nrow(datos) == 0) {
      return(
        highchart() %>%
          hc_title(text = "Citas vs referencias") %>%
          hc_subtitle(text = "Sin datos para los filtros seleccionados") %>%
          hc_add_theme(hc_theme_urban())
      )
    }
    
    highchart() %>%
      hc_chart(type = "scatter", zoomType = "xy") %>%
      hc_title(text = "Citas vs referencias") %>%
      hc_subtitle(text = "Cada punto representa un artículo") %>%
      hc_xAxis(title = list(text = "Referencias")) %>%
      hc_yAxis(title = list(text = "Citas")) %>%
      hc_add_series(
        data = list_parse2(
          datos %>%
            transmute(
              x = n_references,
              y = citations,
              name = title_short,
              tema = topic_label
            )
        ),
        name = "Artículos",
        color = "rgba(225,6,0,0.62)",
        marker = list(
          radius = 5,
          symbol = "circle",
          lineWidth = 1,
          lineColor = "#0a0a0a"
        )
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        pointFormat = paste0(
          "<b>{point.name}</b><br>",
          "Referencias: {point.x}<br>",
          "Citas: {point.y}<br>",
          "Tema: {point.tema}"
        )
      ) %>%
      hc_add_theme(hc_theme_urban()) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # -------------------------------------------------------
  # Gráfica 9: impacto de citas por categoría
  # -------------------------------------------------------
  
  output$grafico_boxplot_citas <- renderHighchart({
    datos <- papers_filtrados() %>%
      group_by(topic_label) %>%
      summarise(
        promedio_citas = round(mean(citations, na.rm = TRUE), 2),
        mediana_citas = round(median(citations, na.rm = TRUE), 2),
        max_citas = max(citations, na.rm = TRUE),
        articulos = n(),
        .groups = "drop"
      ) %>%
      filter(!is.nan(promedio_citas), is.finite(max_citas)) %>%
      arrange(desc(max_citas))
    
    if (nrow(datos) == 0) {
      return(
        highchart() %>%
          hc_title(text = "Impacto de citas") %>%
          hc_subtitle(text = "Sin datos para los filtros seleccionados") %>%
          hc_add_theme(hc_theme_urban())
      )
    }
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Impacto de citas por categoría") %>%
      hc_subtitle(text = "Comparación entre mediana, promedio y máximo") %>%
      hc_xAxis(categories = datos$topic_label, title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Citas")) %>%
      hc_add_series(
        name = "Mediana",
        data = datos$mediana_citas,
        color = "#b8b8b8",
        type = "column"
      ) %>%
      hc_add_series(
        name = "Promedio",
        data = datos$promedio_citas,
        color = "#0a0a0a",
        type = "column"
      ) %>%
      hc_add_series(
        name = "Máximo",
        data = datos$max_citas,
        color = "#e10600",
        type = "spline",
        lineWidth = 4
      ) %>%
      hc_tooltip(shared = TRUE) %>%
      hc_plotOptions(
        column = list(
          borderRadius = 0,
          pointPadding = 0.12
        )
      ) %>%
      hc_add_theme(hc_theme_urban()) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # -------------------------------------------------------
  # Gráfica 10: mapa de calor mes vs tema
  # -------------------------------------------------------
  
  output$grafico_heatmap_mes_tema <- renderHighchart({
    datos <- papers_filtrados() %>%
      filter(!is.na(publication_date), !is.na(topic_label)) %>%
      mutate(mes = month(publication_date, label = TRUE, abbr = TRUE)) %>%
      count(topic_label, mes, name = "cantidad")
    
    if (nrow(datos) == 0) {
      return(
        highchart() %>%
          hc_title(text = "Mapa de calor") %>%
          hc_subtitle(text = "Sin datos para los filtros seleccionados") %>%
          hc_add_theme(hc_theme_urban())
      )
    }
    
    temas <- sort(unique(as.character(datos$topic_label)))
    
    meses <- levels(
      month(
        seq(as.Date("2025-01-01"), as.Date("2025-12-01"), by = "month"),
        label = TRUE,
        abbr = TRUE
      )
    )
    
    datos <- datos %>%
      mutate(
        x = match(as.character(mes), meses) - 1,
        y = match(as.character(topic_label), temas) - 1
      )
    
    highchart() %>%
      hc_chart(type = "heatmap") %>%
      hc_title(text = "Mapa de calor") %>%
      hc_subtitle(text = "Publicaciones por mes y categoría") %>%
      hc_xAxis(categories = meses, title = list(text = NULL)) %>%
      hc_yAxis(categories = temas, title = list(text = NULL), reversed = TRUE) %>%
      hc_colorAxis(
        min = 0,
        minColor = "#f2f2f0",
        maxColor = "#e10600"
      ) %>%
      hc_add_series(
        name = "Artículos",
        borderWidth = 2,
        borderColor = "#ffffff",
        data = list_parse2(
          datos %>%
            transmute(
              x = x,
              y = y,
              value = cantidad
            )
        ),
        dataLabels = list(enabled = TRUE, color = "#0a0a0a")
      ) %>%
      hc_tooltip(pointFormat = "<b>{point.value}</b> artículos") %>%
      hc_add_theme(hc_theme_urban()) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # -------------------------------------------------------
  # Tabla principal
  # -------------------------------------------------------
  
  output$tabla_articulos <- renderDT({
    datos <- papers_filtrados() %>%
      select(
        title,
        authors_raw,
        publication_date,
        topic_label,
        doi,
        citations,
        n_references
      ) %>%
      rename(
        "Título" = title,
        "Autores" = authors_raw,
        "Fecha" = publication_date,
        "Tema" = topic_label,
        "DOI" = doi,
        "Citas" = citations,
        "Referencias" = n_references
      )
    
    datatable(
      datos,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        language = list(
          url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"
        )
      )
    )
  })
  
  # -------------------------------------------------------
  # Descargar datos filtrados
  # -------------------------------------------------------
  
  output$descargar_filtrados <- downloadHandler(
    filename = function() {
      paste0("bdcc_articulos_filtrados_", Sys.Date(), ".csv")
    },
    content = function(file) {
      datos <- papers_filtrados() %>%
        select(
          title,
          authors_raw,
          publication_date,
          topic_label,
          doi,
          citations,
          downloads,
          n_references,
          abstract
        )
      
      write.csv(datos, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # -------------------------------------------------------
  # Scraping: buscar nuevos artículos
  # -------------------------------------------------------
  
  observeEvent(input$actualizar, {
    
    anio <- as.integer(input$anio_scraping)
    
    output$estado_actualizacion <- renderText({
      paste0(
        "Consultando CrossRef para el año ", anio, "…\n",
        "(esto puede tardar hasta 20 segundos)"
      )
    })
    
    tryCatch({
      
      res <- scraping_crossref(anio)
      
      if (!res$ok) {
        output$estado_actualizacion <- renderText({
          res$msg
        })
        return()
      }
      
      nuevos_df <- res$data
      
      if (nrow(nuevos_df) == 0) {
        output$estado_actualizacion <- renderText({
          paste0(
            "CrossRef no encontró artículos para ", anio, ".\n",
            "La base de datos no fue modificada."
          )
        })
        return()
      }
      
      con <- dbConnect(SQLite(), db_path)
      dois_existentes <- dbGetQuery(con, "SELECT doi FROM papers")$doi
      dbDisconnect(con)
      
      nuevos_df <- nuevos_df %>%
        filter(!is.na(doi), doi != "", !doi %in% dois_existentes)
      
      n_nuevos <- nrow(nuevos_df)
      
      if (n_nuevos > 0) {
        
        con <- dbConnect(SQLite(), db_path)
        dbWriteTable(con, "papers", nuevos_df, append = TRUE, row.names = FALSE)
        dbDisconnect(con)
        
        datos_reactivos(cargar_papers())
        
        d_actualizados <- datos_reactivos()
        fecha_inicio <- min(d_actualizados$publication_date, na.rm = TRUE)
        fecha_fin <- max(d_actualizados$publication_date, na.rm = TRUE)
        
        updateDateRangeInput(
          session,
          "fecha",
          start = fecha_inicio,
          end = fecha_fin,
          min = fecha_inicio,
          max = fecha_fin
        )
        
        filtros_aplicados(list(
          fecha = c(fecha_inicio, fecha_fin),
          tema = "Todos",
          autor = "Todos",
          doi = "",
          palabra = ""
        ))
        
        nuevos_autores <- datos_reactivos()$authors_raw %>%
          str_split(";") %>%
          unlist() %>%
          str_trim() %>%
          discard(~ .x == "" | is.na(.x)) %>%
          unique() %>%
          sort()
        
        updateSelectizeInput(
          session,
          "autor",
          choices = c("Todos" = "Todos", setNames(nuevos_autores, nuevos_autores)),
          selected = "Todos"
        )
        
        msg <- paste0(
          "Se encontraron ", n_nuevos, " artículo(s) nuevo(s) para ", anio, ".\n",
          "Los registros han sido guardados en SQLite.\n\n",
          "DOIs insertados:\n",
          paste0("  · ", head(nuevos_df$doi, 10), collapse = "\n"),
          if (n_nuevos > 10) paste0("\n  … y ", n_nuevos - 10, " más.") else ""
        )
        
        resultados_actualizacion(
          nuevos_df %>%
            select(title, doi, publication_date, topic_label, citations) %>%
            rename(
              Título = title,
              DOI = doi,
              Fecha = publication_date,
              Tema = topic_label,
              Citas = citations
            )
        )
        
      } else {
        
        msg <- paste0(
          "CrossRef devolvió ", nrow(res$data), " artículo(s) para ", anio,
          ", pero todos los DOI ya están almacenados en la base de datos.\n",
          "No hubo cambios."
        )
        
        resultados_actualizacion(
          data.frame(
            Estado = "Sin nuevos artículos",
            Consultados_CrossRef = nrow(res$data),
            Insertados_SQLite = 0,
            Resultado = "Todos los DOI ya estaban almacenados en SQLite.",
            stringsAsFactors = FALSE
          )
        )
      }
      
      output$estado_actualizacion <- renderText({
        msg
      })
      
    }, error = function(e) {
      output$estado_actualizacion <- renderText({
        paste0("Error inesperado: ", conditionMessage(e))
      })
    })
  })
  
  # -------------------------------------------------------
  # Verificar últimos 5
  # -------------------------------------------------------
  
  observeEvent(input$verificar_ultimos, {
    ultimos <- datos_reactivos() %>%
      arrange(desc(publication_date)) %>%
      slice(1:5) %>%
      select(
        title,
        doi,
        publication_date,
        topic_label,
        citations,
        n_references
      ) %>%
      rename(
        Título = title,
        DOI = doi,
        Fecha = publication_date,
        Tema = topic_label,
        Citas = citations,
        Referencias = n_references
      )
    
    resultados_actualizacion(ultimos)
    
    output$estado_actualizacion <- renderText({
      paste0(
        "Verificación local completada.\n",
        "Se muestran los últimos 5 artículos almacenados en SQLite.\n",
        "Presione 'Buscar nuevos artículos' para contrastar con CrossRef."
      )
    })
  })
  
  # -------------------------------------------------------
  # Estado inicial de actualización
  # -------------------------------------------------------
  
  output$estado_actualizacion <- renderText({
    "Presione 'Buscar nuevos artículos' o 'Verificar últimos 5' para iniciar."
  })
  
  output$tabla_actualizacion <- renderDT({
    datos <- resultados_actualizacion()
    
    if (nrow(datos) == 0) {
      datos <- data.frame(
        Mensaje = "Aún no se ha ejecutado ningún proceso."
      )
    }
    
    datatable(
      datos,
      rownames = FALSE,
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        language = list(
          url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"
        )
      )
    )
  })
}