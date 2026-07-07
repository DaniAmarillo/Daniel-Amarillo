# server.R - Backend y Lógica Reactiva Estabilizada (Real Data Only)

function(input, output, session) {
  
  # Variable reactiva que notifica cambios si el scraper escribe o actualiza registros
  refresh <- reactiveVal(0)
  
  # --- ORQUESTADOR CENTRAL DE CONSULTAS SQL ---
  datos <- reactive({
    refresh() # Se recalcula si esta variable cambia
    
    con <- get_con()
    df  <- dbReadTable(con, "papers")
    dbDisconnect(con)
    
    # Casteo seguro de formatos
    df <- df |> mutate(
      publication_date = as.Date(publication_date),
      citations = as.integer(citations),
      downloads = as.integer(downloads),
      n_authors = as.integer(n_authors),
      n_references = as.integer(n_references)
    )
    
    # Filtrado reactivo en cascada basado en la UI
    df <- df |> filter(is.na(publication_date) |
                         (publication_date >= input$fechas[1] & publication_date <= input$fechas[2]))
    
    if (input$tema != "Todos")       df <- df |> filter(topic_label == input$tema)
    if (nchar(input$autor)   > 0)    df <- df |> filter(str_detect(tolower(authors_raw), tolower(input$autor)))
    if (nchar(input$titulo)  > 0)    df <- df |> filter(str_detect(tolower(title), tolower(input$titulo)))
    if (nchar(input$doi_fil) > 0)    df <- df |> filter(str_detect(doi, input$doi_fil))
    
    return(df)
  })
  
  # --- RENDERIZADO DE KPIs ---
  output$kpi_total   <- renderText({ nrow(datos()) })
  output$kpi_autores <- renderText({ round(mean(datos()$n_authors, na.rm=TRUE), 1) })
  output$kpi_citas   <- renderText({ round(mean(datos()$citations, na.rm=TRUE), 1) })
  output$kpi_refs    <- renderText({ round(mean(datos()$n_references, na.rm=TRUE), 1) })
  output$kpi_vistas  <- renderText({ format(sum(datos()$downloads, na.rm=TRUE), big.mark=",") })
  output$kpi_temas   <- renderText({ n_distinct(datos()$topic_label) })
  
  # --- GRÁFICO 1: EVOLUCIÓN MENSUAL (Hchart Estable) ---
  output$chart_mes <- renderHighchart({
    df_chart <- datos() |> filter(!is.na(publication_date)) |>
      mutate(mes = format(publication_date, "%Y-%m")) |>
      count(mes) |> arrange(mes) |>
      as.data.frame() # Previene el error 'is.character(txt)' forzando df nativo
    
    if(nrow(df_chart) == 0) return(NULL)
    
    hchart(df_chart, "areaspline", hcaes(x = mes, y = n), name = "Papers Indexados", color = "#2d6a4f") |>
      hc_title(text = "Evolución Mensual de Publicaciones", align = "left", style = list(color = "#1b4332", fontWeight = "bold")) |>
      hc_xAxis(title = list(text = "Periodo")) |>
      hc_yAxis(title = list(text = "Volumen de Artículos")) |>
      hc_credits(enabled = FALSE)
  })
  
  # --- GRÁFICO 2: COMPOSICIÓN DE TEMAS (Hchart Donut Estable) ---
  output$chart_tema <- renderHighchart({
    df_chart <- datos() |> count(topic_label) |> arrange(desc(n)) |>
      as.data.frame()
    
    if(nrow(df_chart) == 0) return(NULL)
    
    hchart(df_chart, "pie", hcaes(x = topic_label, y = n), name = "Artículos") |>
      hc_title(text = "Proporción Temática Registrada (NLP)", align = "left", style = list(color = "#1b4332", fontWeight = "bold")) |>
      hc_plotOptions(pie = list(innerSize = "60%", dataLabels = list(enabled = TRUE, format = "{point.name}: {point.y} papers"))) |>
      hc_colors(c("#1b4332", "#40916c", "#74c69d", "#b7e4c7")) |>
      hc_credits(enabled = FALSE)
  })
  
  # --- GRÁFICO 3: CITAS VS VISTAS (Scatterplot con Tooltip Seguro sin JS) ---
  output$chart_scatter <- renderHighchart({
    df_scatter <- datos() |> select(title, downloads, citations, topic_label) |> 
      filter(!is.na(downloads) & !is.na(citations)) |>
      as.data.frame()
    
    if(nrow(df_scatter) == 0) return(NULL)
    
    hchart(df_scatter, "scatter", hcaes(x = downloads, y = citations, group = topic_label)) |> 
      hc_title(text = "Análisis de Impacto: Correlación Citas vs. Vistas", align = "left", style = list(color = "#1b4332", fontWeight = "bold")) |> 
      hc_xAxis(title = list(text = "Métrica de Uso (Views)")) |> 
      hc_yAxis(title = list(text = "Conteo de Citas Acumuladas")) |> 
      # Formato nativo en cadena de texto: elimina fallos de ejecución JS() de raíz
      hc_tooltip(pointFormat = "<b>Tema:</b> {point.topic_label}<br><b>Título:</b> {point.title}<br><b>Vistas:</b> {point.x}<br><b>Citas:</b> {point.y}") |>
      hc_credits(enabled = FALSE)
  })
  
  # --- GRÁFICO 4: TOP AUTORES INFLUYENTES ---
  output$chart_top_autores <- renderHighchart({
    df_autores <- datos() |> 
      filter(!is.na(authors_raw) & authors_raw != "Anonymous") |> 
      group_by(authors_raw) |> 
      summarise(CitasTotales = sum(citations, na.rm=TRUE)) |> 
      arrange(desc(CitasTotales)) |> head(8) |>
      as.data.frame()
    
    if(nrow(df_autores) == 0) return(NULL)
    
    hchart(df_autores, "bar", hcaes(x = authors_raw, y = CitasTotales), name = "Suma de Citas", color = "#40916c") |> 
      hc_title(text = "Líderes de Citación (Top Autores)", align = "left", style = list(color = "#1b4332", fontWeight = "bold")) |>
      hc_credits(enabled = FALSE)
  })
  
  # --- REGISTROS DE LA TABLA DINÁMICA (`DT`) ---
  output$tabla <- renderDT({
    datos() |>
      select(title, authors_raw, publication_date, topic_label, doi, citations, downloads) |>
      rename(Título=title, Autores=authors_raw, Fecha=publication_date,
             Tema=topic_label, DOI=doi, Citas=citations, Vistas=downloads) |>
      datatable(
        extensions = 'Responsive',
        options = list(pageLength = 7, autoWidth = TRUE, scrollX = TRUE,
                       language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
        rownames = FALSE
      )
  })
  
  # --- BOTÓN DE WEB SCRAPING EN VIVO (Con Regla de Reconsulta de Últimos 5 Obligatoria) ---
  observeEvent(input$btn_scrape, {
    output$scrape_msg <- renderUI(
      div(class="alert alert-warning", "\u23f3 Inicializando automatización Headless Chrome...")
    )
    
    con <- get_con()
    urls_existentes <- dbGetQuery(con, "SELECT url FROM papers")$url
    
    todos_links <- character(0)
    scrape_ok   <- TRUE
    
    withProgress(message = 'Extrayendo índices de Frontiers...', value = 0, {
      tryCatch({
        b <- ChromoteSession$new()
        for (i in seq_along(PAGINAS_NUEVOS)) {
          pg <- PAGINAS_NUEVOS[i]
          setProgress(value = i / length(PAGINAS_NUEVOS), detail = paste("Escaneando página", pg))
          
          url_pag <- paste0(
            "https://www.frontiersin.org/journals/plant-science/articles",
            "?publication-date=01%2F01%2F2026-31%2F12%2F2026",
            if (pg > 1) paste0("&page=", pg) else ""
          )
          b$Page$navigate(url_pag)
          Sys.sleep(4)
          html  <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
          pag   <- read_html(html)
          links <- pag |> html_elements("a") |> html_attr("href") |>
            grep("/articles/fpls", x=_, value=TRUE) |> unique()
          todos_links <- c(todos_links, links[grepl("fpls\\.2026\\.", links)])
        }
        b$close()
      }, error = function(e) { scrape_ok <<- FALSE })
    })
    
    if (!scrape_ok) {
      dbDisconnect(con)
      output$scrape_msg <- renderUI(
        div(class="alert alert-danger", "\u26a0\ufe0f Error de Chromote. Verifica que Google Chrome esté instalado localments.")
      )
      return()
    }
    
    todos_links  <- unique(todos_links)
    links_nuevos <- todos_links[!todos_links %in% urls_existentes]
    
    # --- CASO A: SI ENCUENTRA ARTÍCULOS NUEVOS (2026) ---
    if (length(links_nuevos) > 0) {
      resultados <- list()
      withProgress(message = 'Minando metadatos de artículos...', value = 0, {
        b2 := ChromoteSession$new()
        for (k in seq_along(links_nuevos)) {
          url <- links_nuevos[k]
          setProgress(value = k / length(links_nuevos), detail = paste("Artículo", k, "de", length(links_nuevos)))
          r <- tryCatch(extraer_articulo(b2, url), error=function(e) NULL)
          if (!is.null(r)) resultados[[length(resultados) + 1]] <- r
        }
        b2$close()
      })
      
      n_nuevos <- length(resultados)
      if (n_nuevos > 0) {
        df_nuevos <- bind_rows(resultados)
        max_id    <- dbGetQuery(con, "SELECT MAX(paper_id) as m FROM papers")$m
        if(is.na(max_id)) max_id <- 0
        
        df_nuevos$paper_id <- seq(max_id + 1, max_id + n_nuevos)
        dbWriteTable(con, "papers", df_nuevos, append=TRUE)
        
        output$scrape_msg <- renderUI(
          div(class="alert alert-success", paste0("\u2705 ¡Éxito! Se detectaron e inyectaron ", n_nuevos, " artículos nuevos del 2026."))
        )
        refresh(refresh() + 1)
      } else {
        output$scrape_msg <- renderUI(div(class="alert alert-danger", "\u26a0\ufe0f No se pudieron extraer estructuras válidas."))
      }
      
      # --- CASO B: CUMPLIMIENTO REQUERIMIENTO RECONSULTA DE ÚLTIMOS 5 PAPERS ---
    } else {
      output$scrape_msg <- renderUI(
        div(class="alert alert-info", "\u2139\ufe0f Sin artículos nuevos. Reconsultando últimas 5 entradas para verificar actualizaciones...")
      )
      
      # Traer los últimos 5 guardados de tu BD real
      ultimos_5 <- dbGetQuery(con, "SELECT paper_id, url FROM papers ORDER BY paper_id DESC LIMIT 5")
      
      if(nrow(ultimos_5) > 0) {
        withProgress(message = 'Actualizando métricas desde Frontiers...', value = 0, {
          b3 <- ChromoteSession$new()
          for (m in 1:nrow(ultimos_5)) {
            setProgress(value = m / nrow(ultimos_5), detail = paste("Verificando artículo", m))
            r_upd <- tryCatch(extraer_articulo(b3, ultimos_5$url[m]), error=function(e) NULL)
            
            if(!is.null(r_upd)) {
              # Actualiza dinámicamente las citas y descargas acumuladas en tu archivo .sqlite
              stmt <- dbSendStatement(con, "UPDATE papers SET citations = ?, downloads = ? WHERE paper_id = ?")
              dbBind(stmt, list(r_upd$citations, r_upd$downloads, ultimos_5$paper_id[m]))
              dbClearResult(stmt)
            }
          }
          b3$close()
        })
        output$scrape_msg <- renderUI(
          div(class="alert alert-success", "\u2705 ¡Reconsulta completada! Métricas e impactos de los últimos 5 artículos actualizados en SQLite.")
        )
        refresh(refresh() + 1) # Actualiza el dashboard automáticamente
      }
    }
    dbDisconnect(con)
  })
}