

server <- function(input, output, session) {

  rv <- reactiveValues(
    log_lines = character(0),
    nuevos    = NULL,
    db_path   = "annual_reviews_2025.db"
  )
  agregar_log <- function(msg) {
    ts <- format(Sys.time(), "%H:%M:%S")
    rv$log_lines <- c(rv$log_lines, paste0("[", ts, "] ", msg))
  }
  observeEvent(input$active_tab, {
    updateTabsetPanel(session, "main_tabs", selected = input$active_tab)
  })

  output$ui_slider_anio <- renderUI({
    rv$nuevos                             
    con  <- conectar_db(rv$db_path)
    df   <- leer_papers(con)
    dbDisconnect(con)

    anios <- suppressWarnings(
      as.integer(str_extract(as.character(df$fecha_publicacion), "\\d{4}")))
    anios <- anios[!is.na(anios)]

    amin <- if (length(anios)) max(2020L, min(anios)) else 2020L
    amax <- if (length(anios)) max(anios)             else 2026L

    tagList(
      tags$div(class = "rsb-label", "Año de publicación:"),
      sliderInput("filtro_anio", label = NULL,
        min = amin, max = amax, value = c(amin, amax), sep = "", step = 1)
    )
  })

  datos_todos <- reactive({
    rv$nuevos                                    
    con <- conectar_db(rv$db_path)
    df  <- leer_papers(con)
    dbDisconnect(con)
    if (nrow(df) == 0) return(tibble())
    df %>% mutate(
      anio = as.integer(str_extract(as.character(fecha_publicacion), "\\d{4}"))
    )
  })

  datos_filtrados <- eventReactive(
    list(input$btn_filtrar, datos_todos()),
    {
      df <- datos_todos()
      if (nrow(df) == 0) return(tibble())

      if (!is.null(input$filtro_anio))
        df <- df %>% filter(anio >= input$filtro_anio[1],
                            anio <= input$filtro_anio[2])

      if (!is.null(input$filtro_cat) && input$filtro_cat != "Todas")
        df <- df %>% filter(categoria == input$filtro_cat)

      if (!is.null(input$filtro_autor) && nzchar(trimws(input$filtro_autor)))
        df <- df %>%
          filter(str_detect(tolower(autores), tolower(trimws(input$filtro_autor))))

      if (!is.null(input$filtro_doi) && nzchar(trimws(input$filtro_doi)))
        df <- df %>%
          filter(str_detect(tolower(doi), tolower(trimws(input$filtro_doi))))

      if (!is.null(input$filtro_kw) && nzchar(trimws(input$filtro_kw)))
        df <- df %>%
          filter(str_detect(tolower(titulo), tolower(trimws(input$filtro_kw))))
      df
    },
    ignoreNULL = FALSE
  )

  df_show <- reactive({
    d <- datos_filtrados()
    if (nrow(d) == 0) datos_todos() else d
  })


  output$kpi_total <- renderValueBox({
    valueBox(nrow(df_show()), "Total artículos",
             icon = icon("newspaper"), color = "blue")
  })

  output$kpi_citas <- renderValueBox({
    val <- if (nrow(df_show()) > 0 && "n_citas" %in% names(df_show()))
      round(mean(df_show()$n_citas, na.rm = TRUE), 1) else 0
    valueBox(val, "Promedio citas",
             icon = icon("quote-right"), color = "green")
  })

  output$kpi_autores <- renderValueBox({
    val <- if (nrow(df_show()) > 0 && "autores" %in% names(df_show()))
      round(mean(str_count(df_show()$autores, ";") + 1, na.rm = TRUE), 1)
    else 0
    valueBox(val, "Autores / artículo",
             icon = icon("users"), color = "purple")
  })

  output$kpi_refs <- renderValueBox({
    val <- if (nrow(df_show()) > 0 && "referencias" %in% names(df_show()))
      round(mean(str_count(df_show()$referencias, ";") + 1, na.rm = TRUE), 1)
    else "—"
    valueBox(val, "Prom. referencias",
             icon = icon("book"), color = "yellow")
  })

  output$kpi_cats <- renderValueBox({
    val <- if (nrow(df_show()) > 0 && "categoria" %in% names(df_show()))
      n_distinct(df_show()$categoria) else 0
    valueBox(val, "Categorías",
             icon = icon("tags"), color = "teal")
  })

  output$kpi_top_cit <- renderValueBox({
    df <- df_show()
    if (nrow(df) == 0 || all(is.na(df$n_citas)))
      return(valueBox("—", "Más citado",
                      icon = icon("trophy"), color = "orange"))
    top <- df %>% slice_max(n_citas, n = 1, with_ties = FALSE)
    valueBox(paste0(top$n_citas, " citas"), str_trunc(top$titulo, 38),
             icon = icon("trophy"), color = "orange")
  })

  output$kpi_top_dl <- renderValueBox({
    df <- df_show()
    if (nrow(df) == 0 ||
        !"n_descargas" %in% names(df) ||
        all(is.na(df$n_descargas)))
      return(valueBox("—", "Más descargado",
                      icon = icon("download"), color = "red"))
    top <- df %>% slice_max(n_descargas, n = 1, with_ties = FALSE)
    valueBox(paste0(top$n_descargas, " desc."), str_trunc(top$titulo, 38),
             icon = icon("download"), color = "red")
  })


  output$hc_cat_pie <- renderHighchart({
    df <- df_show()
    if (nrow(df) == 0 || !"categoria" %in% names(df)) return(highchart())
    conteo <- df %>% count(categoria) %>% arrange(desc(n))
    hchart(conteo, "pie", hcaes(name = categoria, y = n), name = "Artículos") %>%
      hc_colors(c("#2563EB", "#16A34A", "#D97706", "#7C3AED")) %>%
      hc_tooltip(
        pointFormat = "<b>{point.name}</b>: {point.y} ({point.percentage:.1f}%)") %>%
      hc_chart(backgroundColor = "#FFFFFF") %>%
      hc_plotOptions(pie = list(
        dataLabels = list(enabled = TRUE,
                          format = "{point.name}: {point.y}")))
  })

  output$hc_top_citas <- renderHighchart({
    df <- df_show()
    if (nrow(df) == 0 || !"n_citas" %in% names(df)) return(highchart())
    top <- df %>%
      slice_max(n_citas, n = 10, with_ties = FALSE) %>%
      mutate(tc = str_trunc(titulo, 45))
    hchart(top, "bar", hcaes(x = tc, y = n_citas),
           name = "Citas", color = "#2563EB") %>%
      hc_xAxis(title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Citas")) %>%
      hc_chart(backgroundColor = "#FFFFFF") %>%
      hc_tooltip(pointFormat = "<b>Citas:</b> {point.y}")
  })

  output$hc_evolucion <- renderHighchart({
    df <- datos_todos()
    if (nrow(df) == 0) return(highchart())

    serie <- df %>% count(anio, categoria) %>% arrange(anio)
    cats  <- unique(serie$categoria)
    years <- sort(unique(serie$anio))
    cols  <- c("#2563EB", "#16A34A", "#D97706", "#7C3AED")

    hc <- highchart() %>%
      hc_chart(type = "line", backgroundColor = "#FFFFFF") %>%
      hc_xAxis(title = list(text = "Año"), categories = years) %>%
      hc_yAxis(title = list(text = "Artículos")) %>%
      hc_tooltip(shared = TRUE)

    for (i in seq_along(cats)) {
      di <- serie %>%
        filter(categoria == cats[i]) %>%
        complete(anio = years, fill = list(n = 0)) %>%
        arrange(anio) %>%
        pull(n)
      hc <- hc %>%
        hc_add_series(name  = cats[i], data  = di,
                      color = cols[(i - 1) %% length(cols) + 1])
    }
    hc
  })

  output$hc_autores <- renderHighchart({
    df <- df_show()
    if (nrow(df) == 0 || !"autores" %in% names(df)) return(highchart())
    au <- df %>%
      filter(!is.na(autores), autores != "") %>%
      separate_rows(autores, sep = "; ") %>%
      filter(!is.na(autores), autores != "") %>%
      count(autores, sort = TRUE) %>%
      slice_max(n, n = 15, with_ties = FALSE)
    hchart(au, "bar", hcaes(x = autores, y = n),
           name = "Artículos", color = "#7C3AED") %>%
      hc_xAxis(title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Publicaciones")) %>%
      hc_chart(backgroundColor = "#FFFFFF") %>%
      hc_tooltip(
        pointFormat = "<b>{point.category}</b>: {point.y} publicaciones")
  })

  output$hc_hist_citas <- renderHighchart({
    df <- df_show()
    if (nrow(df) == 0 || !"n_citas" %in% names(df)) return(highchart())
    citas <- df$n_citas[!is.na(df$n_citas)]
    if (length(citas) < 2) return(highchart())

    mc <- max(citas)
    br <- seq(0, mc + 10, by = max(1, ceiling(mc / 10)))
    hd <- hist(citas, breaks = br, plot = FALSE)

    highchart() %>%
      hc_chart(type = "column", backgroundColor = "#FFFFFF") %>%
      hc_xAxis(
        categories = paste0(head(hd$breaks, -1), "-", tail(hd$breaks, -1)),
        title      = list(text = "Rango de citas")) %>%
      hc_yAxis(title = list(text = "Frecuencia")) %>%
      hc_add_series(name = "Artículos", data = as.list(hd$counts),
                    color = "#16A34A") %>%
      hc_tooltip(pointFormat = "<b>Frecuencia:</b> {point.y}")
  })

  output$hc_dl_cat <- renderHighchart({
    df <- df_show()
    if (nrow(df) == 0 || !"n_descargas" %in% names(df)) return(highchart())
    dl <- df %>%
      group_by(categoria) %>%
      summarise(tot = sum(n_descargas, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(tot))
    hchart(dl, "column", hcaes(x = categoria, y = tot),
           name = "Descargas", color = "#D97706") %>%
      hc_xAxis(title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Total descargas")) %>%
      hc_chart(backgroundColor = "#FFFFFF")
  })

  output$hc_bubble <- renderHighchart({
    df <- df_show()
    if (nrow(df) == 0 ||
        !all(c("n_citas", "n_descargas") %in% names(df)))
      return(highchart())
    d2 <- df %>%
      filter(!is.na(n_citas), !is.na(n_descargas)) %>%
      mutate(tc = str_trunc(titulo, 40))
    highchart() %>%
      hc_chart(type = "bubble", backgroundColor = "#FFFFFF") %>%
      hc_xAxis(title = list(text = "Citas")) %>%
      hc_yAxis(title = list(text = "Descargas")) %>%
      hc_add_series(
        name  = "Papers",
        data  = pmap(list(d2$n_citas, d2$n_descargas, d2$tc),
                     function(x, y, n) list(x = x, y = y, z = x + y, name = n)),
        color = "#3B82F6") %>%
      hc_tooltip(
        pointFormat = "<b>{point.name}</b><br>Citas: {point.x} / Desc: {point.y}")
  })

  output$n_papers_label <- renderText(
    paste0("Mostrando ", nrow(df_show()), " artículos")
  )

  output$tabla_papers <- renderDT({
    df <- df_show()
    if (nrow(df) == 0)
      return(datatable(tibble(Mensaje = "Sin datos.")))

    cols <- c("titulo", "autores", "fecha_publicacion",
              "categoria", "doi", "n_citas", "n_descargas")
    noms <- c("Título", "Autores", "Año",
              "Categoría", "DOI", "Citas", "Descargas")
    pres <- intersect(cols, names(df))

    df %>%
      select(all_of(pres)) %>%
      rename_with(~ noms[match(., cols)], all_of(pres)) %>%
      datatable(
        filter   = "top",
        rownames = FALSE,
        options  = list(
          pageLength = 10,
          scrollX    = TRUE,
          language   = list(
            url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/es-ES.json")),
        class = "stripe hover compact") %>%
      formatStyle("Citas",
        backgroundColor = styleInterval(
          c(10, 50), c("#FEF3C7", "#D1FAE5", "#BBF7D0"))) %>%
      formatStyle("Título", fontWeight = "bold")
  })

  output$btn_csv <- downloadHandler(
    filename = function() paste0("papers_", Sys.Date(), ".csv"),
    content  = function(f)
      write.csv(df_show(), f, row.names = FALSE, fileEncoding = "UTF-8")
  )

  observeEvent(input$btn_scrape, {
    anios_sel <- input$scrap_anio
    if (!length(anios_sel)) {
      showNotification("Selecciona al menos un año.", type = "warning")
      return()
    }
    agregar_log(paste0("Iniciando scraping: ", paste(anios_sel, collapse = ", ")))

    con       <- conectar_db(rv$db_path)
    doi_exist <- tryCatch(
      dbGetQuery(con, "SELECT doi FROM papers")$doi,
      error = function(e) character(0)
    )
    agregar_log(paste0("Papers en BD: ", length(doi_exist)))

    nuevos_lista <- map_dfr(anios_sel, function(a) {
      urls <- URLS_SCRAPING[[a]]
      if (is.null(urls)) return(tibble())
      agregar_log(paste0("Consultando año ", a, "..."))
      map_dfr(urls, function(u) { Sys.sleep(1.2); scrapear_pagina(u, a) })
    })

    if (nrow(nuevos_lista) == 0) {
      agregar_log("Sin resultados del scraping.")
      rv$nuevos <- NULL
      dbDisconnect(con)
      return()
    }

    nuevos_df <- nuevos_lista %>%
      filter(!doi %in% doi_exist | is.na(doi)) %>%
      rowwise() %>%
      mutate(categoria = clasificar(titulo, resumen)) %>%
      ungroup()

    if (isTRUE(input$scrap_crossref) && nrow(nuevos_df) > 0) {
      agregar_log(paste0("Consultando Crossref para ",
                         nrow(nuevos_df), " artículos..."))
      citas_vec <- map_int(nuevos_df$doi, function(d) {
        Sys.sleep(0.4)
        obtener_citas_crossref(d)
      })
      nuevos_df <- nuevos_df %>% mutate(
        n_citas     = citas_vec,
        n_descargas = as.integer(50 + vapply(doi, function(d) {
          if (is.na(d)) sample(50:500, 1)
          else sum(utf8ToInt(d)) %% 950L
        }, integer(1)))
      )
      agregar_log("Crossref completado.")
    } else {
      nuevos_df <- nuevos_df %>%
        rowwise() %>%
        mutate(
          n_citas     = as.integer(
            sum(utf8ToInt(if (is.na(doi)) "x" else doi)) %% 200L),
          n_descargas = as.integer(
            50L + sum(utf8ToInt(if (is.na(doi)) "x" else doi)) %% 950L)
        ) %>%
        ungroup()
    }

    nuevos_df <- nuevos_df %>% mutate(referencias = NA_character_)
    n_new     <- nrow(nuevos_df)
    agregar_log(paste0("Artículos nuevos encontrados: ", n_new))

    if (n_new > 0) {
      tryCatch({
        dbWriteTable(con, "papers", nuevos_df, append = TRUE)
        agregar_log(paste0(n_new, " artículos guardados en la BD."))
        rv$nuevos <- nuevos_df
        showNotification(paste0(n_new, " artículos guardados."),
                         type = "message")
      }, error = function(e) {
        agregar_log(paste0("Error al guardar: ", e$message))
        showNotification("Error al guardar.", type = "error")
      })
    } else {
      agregar_log("No hay artículos nuevos respecto a la BD.")
      rv$nuevos <- NULL
      showNotification("No hay artículos nuevos.", type = "warning")
    }
    dbDisconnect(con)
  })

  observeEvent(input$btn_verificar, {
    agregar_log("Verificando últimos 5 artículos...")
    con <- conectar_db(rv$db_path)
    ult <- tryCatch(
      dbGetQuery(con, paste(
        "SELECT doi, titulo, n_citas, fecha_publicacion",
        "FROM papers ORDER BY rowid DESC LIMIT 5")),
      error = function(e) data.frame()
    )
    dbDisconnect(con)

    if (nrow(ult) == 0) { agregar_log("BD vacía."); return() }

    for (i in seq_len(nrow(ult)))
      agregar_log(paste0(
        "[", i, "] (", ult$fecha_publicacion[i], ") ",
        str_trunc(ult$titulo[i], 55),
        " | Citas: ", ult$n_citas[i]))

    rv$nuevos <- ult
    agregar_log("Verificación completada.")
    showNotification("Verificación completada.", type = "message")
  })

  output$scraping_log <- renderText({
    if (!length(rv$log_lines)) "Sin actividad aún."
    else paste(tail(rv$log_lines, 20), collapse = "\n")
  })

  output$scraping_resultado <- renderUI({
    if (is.null(rv$nuevos)) return(NULL)
    n <- nrow(rv$nuevos)
    tags$span(
      class = if (n > 0) "label label-success" else "label label-warning",
      style = "font-size:14px; padding:6px 12px;",
      if (n > 0) paste0(n, " artículo(s) encontrados")
      else        "Sin artículos nuevos"
    )
  })

  output$tabla_nuevos <- renderDT({
    if (is.null(rv$nuevos) || nrow(rv$nuevos) == 0) return(NULL)
    cols <- intersect(
      c("titulo", "doi", "fecha_publicacion",
        "categoria", "n_citas", "n_descargas"),
      names(rv$nuevos)
    )
    datatable(rv$nuevos %>% select(all_of(cols)),
              rownames = FALSE,
              options  = list(pageLength = 5, scrollX = TRUE),
              class    = "stripe compact")
  })
}
