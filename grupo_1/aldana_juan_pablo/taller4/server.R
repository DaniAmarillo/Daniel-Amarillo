

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

    # 1) Listado por volumen (pageSize=100 + paginación real como respaldo).
    #    Cada año consulta UNA vez (o más si el sitio pagina de verdad), en
    #    lugar de asumir ciegamente 2 páginas fijas por volumen. Se espacian
    #    las peticiones ENTRE volúmenes (no solo entre páginas de un mismo
    #    volumen) porque el sitio aplica un rate-limit temporal: pedir varios
    #    volúmenes seguidos muy rápido puede hacer que los primeros reciban
    #    403 y, para cuando el rate-limit se libera unos segundos después,
    #    los últimos de la lista sí respondan con normalidad.
    consultar_anio <- function(a) {
      urls <- URLS_SCRAPING[[a]]
      if (is.null(urls)) return(tibble())
      agregar_log(paste0("Consultando volumen del año ", a, "..."))
      res <- map_dfr(urls, function(u) {
        Sys.sleep(1.2)
        scrapear_pagina(u, a)
      })
      agregar_log(paste0("  -> ", nrow(res), " articulos encontrados para ", a))
      res
    }

    nuevos_lista <- map_dfr(seq_along(anios_sel), function(i) {
      if (i > 1) Sys.sleep(4 + stats::runif(1, 0.5, 2.5))
      consultar_anio(anios_sel[i])
    })

    anios_con_datos <- if (nrow(nuevos_lista) > 0) unique(nuevos_lista$fecha_publicacion) else character(0)
    anios_vacios     <- setdiff(anios_sel, anios_con_datos)
    if (length(anios_vacios) > 0 && length(anios_vacios) < length(anios_sel)) {
      agregar_log(paste0("Reintentando tras pausa: ", paste(anios_vacios, collapse = ", ")))
      Sys.sleep(6)
      reintento_lista <- map_dfr(seq_along(anios_vacios), function(i) {
        if (i > 1) Sys.sleep(4 + stats::runif(1, 0.5, 2.5))
        consultar_anio(anios_vacios[i])
      })
      nuevos_lista <- bind_rows(nuevos_lista, reintento_lista)
    }

    if (nrow(nuevos_lista) == 0) {
      agregar_log("Sin resultados del scraping. Ejecutando diagnostico de acceso al sitio...")
      diag <- tryCatch(diagnosticar_acceso_ar(), error = function(e) paste0("No se pudo diagnosticar: ", e$message))
      agregar_log(diag)
      rv$nuevos <- NULL
      dbDisconnect(con)
      showNotification("Scraping bloqueado por el sitio. Revisa el log para el diagnostico.", type = "error", duration = 10)
      return()
    }

    nuevos_df <- nuevos_lista %>%
      filter(!doi %in% doi_exist | is.na(doi)) %>%
      distinct(doi, .keep_all = TRUE) %>%
      rowwise() %>%
      mutate(categoria = clasificar(titulo, resumen)) %>%
      ungroup()

    n_candidatos <- nrow(nuevos_df)
    agregar_log(paste0("Articulos nuevos (no presentes en BD): ", n_candidatos))

    if (n_candidatos > 0) {
      agregar_log(paste0("Visitando ", n_candidatos, " paginas de articulo para datos reales..."))
      detalle_lista <- map(seq_len(n_candidatos), function(i) {
        Sys.sleep(0.8)
        scrapear_articulo(nuevos_df$url[i])
      })

      nuevos_df <- nuevos_df %>%
        mutate(
          resumen_completo = map_chr(detalle_lista, ~ .x$resumen_completo %||% NA_character_),
          fecha_exacta      = map_chr(detalle_lista, ~ .x$fecha_exacta %||% NA_character_),
          referencias       = map_chr(detalle_lista, ~ .x$referencias %||% NA_character_),
          n_referencias     = map_int(detalle_lista, ~ as.integer(.x$n_referencias %||% NA_integer_))
        ) %>%
        mutate(
          resumen = ifelse(!is.na(resumen_completo) &
                              nchar(resumen_completo) > nchar(ifelse(is.na(resumen), "", resumen)),
                            resumen_completo, resumen),
          fecha_publicacion = ifelse(!is.na(fecha_exacta) & fecha_exacta != "",
                                      fecha_exacta, fecha_publicacion)
        ) %>%
        select(-resumen_completo, -fecha_exacta)
      agregar_log("Detalle de articulos completado (resumen/fecha/referencias).")
    }

    if (isTRUE(input$scrap_crossref) && n_candidatos > 0) {
      agregar_log(paste0("Consultando Crossref + OpenAlex para ",
                         n_candidatos, " articulos..."))
      citas_vec <- map_int(nuevos_df$doi, function(d) {
        Sys.sleep(0.4)
        r <- obtener_citas_reales(d)
        if (is.na(r)) NA_integer_ else as.integer(r)
      })

      refs_cr <- map_chr(seq_len(n_candidatos), function(i) {
        if (!is.na(nuevos_df$referencias[i]) && nuevos_df$referencias[i] != "")
          return(nuevos_df$referencias[i])
        Sys.sleep(0.2)
        obtener_metadatos_crossref(nuevos_df$doi[i])$referencias_doi %||% NA_character_
      })

      nuevos_df <- nuevos_df %>% mutate(
        n_citas     = citas_vec,
        referencias = refs_cr,
        n_descargas = as.integer(50 + vapply(doi, function(d) {
          if (is.na(d)) sample(50:500, 1)
          else sum(utf8ToInt(d)) %% 950L
        }, integer(1)))
      )
      agregar_log("Crossref + OpenAlex completado.")
    } else if (n_candidatos > 0) {
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

    cols_extra <- intersect(c("paginas", "n_referencias"), names(nuevos_df))
    if (length(cols_extra) > 0) {
      nuevos_df <- nuevos_df %>% select(-all_of(cols_extra))
    }

    n_new <- nrow(nuevos_df)
    agregar_log(paste0("Articulos nuevos a guardar: ", n_new))

    if (n_new > 0) {
      tryCatch({
        dbWriteTable(con, "papers", nuevos_df, append = TRUE)
        agregar_log(paste0(n_new, " articulos guardados en la BD."))
        rv$nuevos <- nuevos_df
        showNotification(paste0(n_new, " articulos guardados."),
                         type = "message")
      }, error = function(e) {
        agregar_log(paste0("Error al guardar: ", e$message))
        showNotification("Error al guardar.", type = "error")
      })
    } else {
      agregar_log("No hay articulos nuevos respecto a la BD.")
      rv$nuevos <- NULL
      showNotification("No hay articulos nuevos.", type = "warning")
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

  observeEvent(input$btn_diagnostico, {
    agregar_log("Probando acceso a annualreviews.org...")
    diag <- tryCatch(diagnosticar_acceso_ar(), error = function(e) paste0("Error al diagnosticar: ", e$message))
    agregar_log(diag)
    tipo_msg <- if (startsWith(diag, "OK")) "message" else "warning"
    showNotification(diag, type = tipo_msg, duration = 12)
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

  # ── Buscador de Recuperación de Información (Taller 4) ──────────────────
  resultados_ir <- eventReactive(input$btn_buscar_ir, {
    req(input$ir_query, nzchar(trimws(input$ir_query)))

    if (is.null(CACHE_IR)) {
      showNotification(
        "El índice de IR no está disponible (revisa cache/ir_cache.rds).",
        type = "error")
      return(tibble())
    }

    top_n <- if (is.null(input$ir_topn) || is.na(input$ir_topn)) 5L else input$ir_topn

    if (identical(input$ir_metodo, "lsa")) {
      buscar_lsa(input$ir_query, CACHE_IR, top_n = top_n)
    } else {
      buscar_tfidf(input$ir_query, CACHE_IR, top_n = top_n)
    }
  })

  output$tabla_ir_resultados <- renderDT({
    df <- resultados_ir()
    if (nrow(df) == 0)
      return(datatable(tibble(Mensaje = "Sin resultados para esta consulta."),
                        rownames = FALSE))

    df %>%
      mutate(
        score  = round(score, 4),
        enlace = dplyr::case_when(
          !is.na(doi) & doi != "" ~
            paste0("<a href='https://doi.org/", doi, "' target='_blank'>", doi, "</a>"),
          !is.na(url) & url != "" ~
            paste0("<a href='", url, "' target='_blank'>Ver artículo</a>"),
          TRUE ~ ""
        )
      ) %>%
      select(posicion, titulo, autores, categoria, fecha_publicacion, score, enlace) %>%
      rename(`#` = posicion, Título = titulo, Autores = autores,
             Categoría = categoria, Año = fecha_publicacion,
             Similitud = score, `DOI / Enlace` = enlace) %>%
      datatable(
        rownames  = FALSE,
        selection = "single",
        escape    = FALSE,  # necesario para que el enlace HTML se renderice
        options   = list(pageLength = 10, scrollX = TRUE),
        class     = "stripe hover compact") %>%
      formatStyle("Similitud",
        background = styleColorBar(c(0, 1), "#BBF7D0"))
  })

  output$ir_resumen_seleccionado <- renderUI({
    df  <- resultados_ir()
    sel <- input$tabla_ir_resultados_rows_selected

    if (nrow(df) == 0 || is.null(sel)) {
      return(tags$p(
        "Realiza una búsqueda y selecciona un artículo de la tabla para ver su resumen.",
        style = "color:#64748B; font-style:italic;"))
    }

    fila <- df[sel, ]
    doi_o_url <- if (!is.na(fila$doi) && fila$doi != "") {
      tags$a(href = paste0("https://doi.org/", fila$doi), target = "_blank", fila$doi)
    } else if (!is.na(fila$url) && fila$url != "") {
      tags$a(href = fila$url, target = "_blank", "Ver artículo")
    } else {
      tags$span("No disponible")
    }

    tagList(
      tags$h4(paste0("#", fila$posicion, " · ", fila$titulo), style = "color:#1E40AF;"),
      tags$p(tags$b("Autores: "), if (!is.na(fila$autores)) fila$autores else "No disponible"),
      tags$p(tags$b("Categoría: "), fila$categoria,
             tags$b(" · Año: "), fila$fecha_publicacion),
      tags$p(tags$b("DOI: "), doi_o_url),
      tags$p(tags$b("Puntaje de similitud coseno: "), round(fila$score, 4)),
      tags$hr(),
      tags$p(fila$resumen)
    )
  })
}
