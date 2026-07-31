server <- function(input, output, session) {
  # ============================================================================
  # 1. DATOS REACTIVOS
  # ============================================================================
  datos_reactivos <- reactiveVal(data.frame())
  estado_scraping <- reactiveVal("online")
  estado_mensaje  <- reactiveVal("Sistema en línea y operando de forma óptima.")
  ultimo_reporte  <- reactiveVal(NULL)
  doi_consultado  <- reactiveVal(NULL)
  
  cargar_datos <- function() {
    con <- dbConnect(RSQLite::SQLite(), dbname = "Springer_Visual_Miner.sqlite")
    if (dbExistsTable(con, "papers")) {
      df <- dbReadTable(con, "papers") %>%
        mutate(publication_date_clean = as.Date(publication_date, format = "%Y-%m-%d"))
      auts <- if (dbExistsTable(con, "authors")) dbReadTable(con, "authors") else data.frame(author_name = character())
      
      datos_reactivos(df)
      
      updateSelectInput(session, "filtro_tema", choices = c("Todos", unique(df$topic_label)), selected = "Todos")
      updateSelectizeInput(session, "filtro_autor", choices = c("Todos", unique(auts$author_name)), selected = "Todos", server = TRUE)
    }
    dbDisconnect(con)
  }
  
  isolate(cargar_datos())

  # ----------------------------------------------------------------------------
  # MOTOR DE SINCRONIZACION 
  # ----------------------------------------------------------------------------
  correr_sincronizacion <- function(modo = "incremental") {
    
    estado_scraping(if (modo == "completo") "rebuilding" else "syncing")
    estado_mensaje("Conectando con el indice bibliografico...")
    
    reporte <- NULL
    
    withProgress(
      message = if (modo == "completo") "Reconstruyendo el corpus" else "Sincronizando con Springer",
      value = 0.05, {
        
        reporte <- tryCatch({
          
          r <- ejecutar_scraping(
            modo       = modo,
            desde      = "2025-01-01",
            enriquecer = TRUE,
            progreso   = function(frac, txt) setProgress(value = frac, detail = txt)
          )
          
          setProgress(value = 0.93, detail = "Normalizando tablas relacionales...")
          normalizar_tablas()
          
          setProgress(value = 0.98, detail = "Recargando datos...")
          cargar_datos()
          
          r
        }, error = function(e) {
          list(ok = FALSE, tipo = "error",
               titulo  = "Fallo la sincronizacion",
               mensaje = paste("Error:", conditionMessage(e)),
               detalle = character(0),
               n_nuevos = 0, n_actualizados = 0, n_metricas_cambiadas = 0,
               dois_nuevos = character(0), fallidos = character(0), segundos = 0)
        })
      })
    
    if (isTRUE(reporte$ok) && reporte$n_nuevos > 0) {
      reporte$mensaje <- paste0(
        reporte$mensaje,
        " Nota: el indice de busqueda no se reconstruye automaticamente;",
        " estos articulos aun no son buscables.")
    }
    
    ultimo_reporte(reporte)
    estado_scraping("online")
    estado_mensaje(reporte$mensaje)
    
    showNotification(
      ui = tagList(tags$strong(reporte$titulo), tags$br(), reporte$mensaje),
      type     = if (isTRUE(reporte$ok)) "message" else "error",
      duration = 12
    )
    
    shinyjs::runjs("setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 200);")
    invisible(reporte)
  }
  
  # ============================================================================
  # 2. FILTRADO INTELIGENTE
  # ============================================================================
  datos_filtrados <- reactive({
    df <- datos_reactivos()
    if (nrow(df) == 0) return(df)
    
    tema_sel  <- input$filtro_tema
    autor_sel <- input$filtro_autor
    busqueda  <- input$busqueda_global
    rango_f   <- input$filtro_fecha
    
    if (!is.null(tema_sel) && length(tema_sel) > 0 && isTRUE(all(tema_sel != "Todos"))) {
      df <- df %>% filter(topic_label %in% tema_sel)
    }
    
    if (!is.null(autor_sel) && length(autor_sel) > 0 && isTRUE(all(autor_sel != "Todos"))) {
      con <- dbConnect(RSQLite::SQLite(), dbname = "Springer_Visual_Miner.sqlite")
      rel <- if (dbExistsTable(con, "paper_authors")) dbReadTable(con, "paper_authors") else data.frame()
      aut <- if (dbExistsTable(con, "authors")) dbReadTable(con, "authors") else data.frame()
      dbDisconnect(con)
      
      if (nrow(rel) > 0 && nrow(aut) > 0) {
        ids_autor <- aut %>% filter(author_name %in% autor_sel) %>% pull(author_id)
        pids_autor <- rel %>% filter(author_id %in% ids_autor) %>% pull(paper_id)
        df <- df %>% filter(paper_id %in% pids_autor)
      }
    }
    
    if (!is.null(rango_f) && length(rango_f) == 2) {
      df <- df %>% filter(publication_date_clean >= rango_f[1] & publication_date_clean <= rango_f[2])
    }
    
    if (!is.null(busqueda) && busqueda != "") {
      b_term <- tolower(busqueda)
      df <- df %>% filter(
        str_detect(tolower(title), b_term) | str_detect(tolower(abstract), b_term) | str_detect(tolower(doi), b_term)
      )
    }
    return(df)
  })
  
  # ============================================================================
  # 3. COMPONENTES GLOBALES
  # ============================================================================
  output$status_scraping <- renderUI({
    col <- if(estado_scraping() == "online") "#10b981" else "#f59e0b"
    div(style = "display:flex; align-items:center; gap:8px; font-weight:600; color:var(--text-primary);",
        tags$i(class = "fa-solid fa-circle", style = paste("font-size:8px; color:", col)),
        paste("Estado:", toupper(estado_scraping()))
    )
  })
  
  output$topbar_title <- renderUI({
    tagList(
      div(class = "section-title", id="topbar-title-text", "Dashboard - Artificial Intelligence Review"),
      div(class = "section-sub", id="topbar-sub-text", "Resumen general del corpus")
    )
  })
  
  output$status_badge <- renderUI({ div(class = "topbar-badge", div(class = "dot"), "LIVE SYNC") })
  output$topbar_count <- renderUI({ paste(nrow(datos_filtrados()), "Artículos") })
  
  # ============================================================================
  # 4. PÁGINA 1: DASHBOARD EJECUTIVO
  # ============================================================================
  output$kpi_articulos <- renderUI({
    div(class = "kpi-card kpi-blue",
        div(class = "kpi-header", div(class = "kpi-icon", tags$i(class = "fa-solid fa-file-invoice"))),
        div(class = "kpi-value", nrow(datos_filtrados())), div(class = "kpi-label", "Artículos Filtrados")
    )
  })
  
  output$kpi_autores <- renderUI({
    div(class = "kpi-card kpi-emerald",
        div(class = "kpi-header", div(class = "kpi-icon", tags$i(class = "fa-solid fa-users"))),
        div(class = "kpi-value", sum(datos_filtrados()$n_authors, na.rm = TRUE)), div(class = "kpi-label", "Firmas de Autores")
    )
  })
  
  output$kpi_citas <- renderUI({
    div(class = "kpi-card kpi-purple",
        div(class = "kpi-header", div(class = "kpi-icon", tags$i(class = "fa-solid fa-quote-right"))),
        div(class = "kpi-value", sum(datos_filtrados()$citations, na.rm = TRUE)), div(class = "kpi-label", "Citas Globales")
    )
  })
  
  output$kpi_descargas <- renderUI({
    div(class = "kpi-card kpi-cyan",
        div(class = "kpi-header", div(class = "kpi-icon", tags$i(class = "fa-solid fa-cloud-arrow-down"))),
        div(class = "kpi-value", sum(datos_filtrados()$downloads, na.rm = TRUE)), div(class = "kpi-label", "Descargas Totales")
    )
  })
  
  output$plot_tendencia <- renderHighchart({
    df <- datos_filtrados()
    if(nrow(df) == 0) return(highchart())
    res <- df %>% count(year, topic_label) %>% complete(year, topic_label, fill = list(n = 0)) %>% arrange(year)
    hchart(res, "areaspline", hcaes(x = as.character(year), y = n, group = topic_label)) %>%
      hc_chart(backgroundColor = "transparent") %>% hc_colors(c("#00F2FE", "#4FACFE", "#00FF87", "#A18CD1")) %>%
      hc_xAxis(title = list(text=""), labels=list(style=list(color="#8E9AA8"))) %>% hc_yAxis(gridLineColor="#223143")
  })
  
  output$plot_categorias <- renderHighchart({
    df <- datos_filtrados()
    if(nrow(df) == 0) return(highchart())
    res <- df %>% group_by(topic_label) %>% summarise(Citas = sum(citations, na.rm=T), Pubs = n())
    highchart() %>% hc_chart(type = "column", backgroundColor = "transparent") %>%
      hc_xAxis(categories = as.list(res$topic_label), labels=list(style=list(color="#8E9AA8"))) %>%
      hc_yAxis(gridLineColor="#223143") %>%
      hc_add_series(name="Citas", data=as.list(res$Citas), color="#00F2FE") %>%
      hc_add_series(name="Artículos", data=as.list(res$Pubs), color="#00FF87")
  })
  
  output$plot_bubble <- renderHighchart({
    df <- datos_filtrados()
    if(nrow(df) == 0) return(highchart())
    hchart(df, "scatter", hcaes(x = downloads, y = citations, name = title)) %>%
      hc_chart(backgroundColor = "transparent") %>% hc_colors("#4FACFE") %>%
      hc_tooltip(pointFormat = "<b>{point.name}</b><br/>Descargas: {point.x}<br/>Citas: {point.y}")
  })
  
  output$plot_pareto <- renderHighchart({
    df <- datos_filtrados()
    if(nrow(df) == 0) return(highchart())
    con <- dbConnect(RSQLite::SQLite(), dbname = "Springer_Visual_Miner.sqlite")
    rel <- if(dbExistsTable(con, "paper_authors")) dbReadTable(con, "paper_authors") else data.frame()
    aut <- if(dbExistsTable(con, "authors")) dbReadTable(con, "authors") else data.frame()
    dbDisconnect(con)
    if(nrow(rel)==0) return(highchart())
    
    top <- rel %>% filter(paper_id %in% df$paper_id) %>% count(author_id) %>% 
      slice_max(n, n=10, with_ties=F) %>% left_join(aut, by="author_id")
    hchart(top, "column", hcaes(x = author_name, y = n)) %>% hc_colors("#A18CD1") %>%
      hc_chart(backgroundColor="transparent") %>% hc_xAxis(labels=list(rotation=-45, style=list(color="#8E9AA8")))
  })
  
  # ============================================================================
  # 5. PÁGINA 2: ANALYTICS
  # ============================================================================
  output$plot_citas_anio <- renderHighchart({
    df <- datos_filtrados()
    if(nrow(df) == 0) return(highchart())
    res <- df %>% group_by(year) %>% summarise(c = sum(citations, na.rm=T)) %>% arrange(year)
    hchart(res, "line", hcaes(x = as.character(year), y = c)) %>%
      hc_chart(backgroundColor="transparent") %>% hc_colors("#F59E0B") %>%
      hc_xAxis(title=list(text="Año"), labels=list(style=list(color="#8E9AA8")))
  })
  
  output$plot_downloads_tiempo <- renderHighchart({
    df <- datos_filtrados()
    if(nrow(df) == 0) return(highchart())
    res <- df %>% group_by(year) %>% summarise(d = sum(downloads, na.rm=T)) %>% arrange(year)
    hchart(res, "areaspline", hcaes(x = as.character(year), y = d)) %>%
      hc_chart(backgroundColor="transparent") %>% hc_colors("#10B981") %>%
      hc_xAxis(title=list(text="Año"), labels=list(style=list(color="#8E9AA8")))
  })
  
  output$plot_heatmap <- renderHighchart({
    df <- datos_filtrados()
    if(nrow(df) == 0) return(highchart())
    res <- df %>% count(year, topic_label)
    hchart(res, "heatmap", hcaes(x = as.character(year), y = topic_label, value = n)) %>%
      hc_colorAxis(stops = color_stops(10, c("#0f172a", "#6366f1", "#22d3ee"))) %>%
      hc_chart(backgroundColor="transparent") %>% hc_xAxis(labels=list(style=list(color="#8E9AA8")))
  })
  
  output$plot_treemap <- renderHighchart({
    df <- datos_filtrados()
    if(nrow(df) == 0) return(highchart())
    res <- df %>% count(topic_label)
    hchart(res, "pie", hcaes(name = topic_label, y = n), innerSize = "60%") %>%
      hc_chart(backgroundColor="transparent") %>%
      hc_plotOptions(pie = list(dataLabels = list(color="#fff", style=list(textOutline="none"))))
  })
  
  output$plot_top_papers_citas <- renderHighchart({
    df <- datos_filtrados() %>% slice_max(citations, n=10, with_ties=FALSE) %>% arrange(citations)
    if(nrow(df) == 0) return(highchart())
    hchart(df, "bar", hcaes(x = str_trunc(title, 40), y = citations)) %>%
      hc_chart(backgroundColor="transparent") %>% hc_colors("#A78BFA") %>%
      hc_xAxis(labels=list(style=list(color="#8E9AA8")))
  })
  
  output$plot_top_papers_downloads <- renderHighchart({
    df <- datos_filtrados() %>% slice_max(downloads, n=10, with_ties=FALSE) %>% arrange(downloads)
    if(nrow(df) == 0) return(highchart())
    hchart(df, "bar", hcaes(x = str_trunc(title, 40), y = downloads)) %>%
      hc_chart(backgroundColor="transparent") %>% hc_colors("#34D399") %>%
      hc_xAxis(labels=list(style=list(color="#8E9AA8")))
  })
  
  # ============================================================================
  # 6. PÁGINA 3: LITERATURE EXPLORER 
  # ============================================================================
  output$lit_count_label <- renderUI({ paste("Mostrando", nrow(datos_filtrados()), "documentos") })
  
  output$tabla_articulos <- renderDT({
    df <- datos_filtrados()
    if (nrow(df) == 0) return(datatable(data.frame(Mensaje="Sin resultados"), options=list(dom="t")))
    
    datatable(
      df %>% select(title, authors_raw, year, topic_label, citations, downloads),
      colnames = c("Título", "Autores", "Año", "Categoría", "Citas", "Descargas"),
      selection = "single", 
      rownames = FALSE, 
      class = "display nowrap stripe hover", 
      options = list(
        paging = TRUE,             
        pageLength = 10,           
        scrollX = TRUE,            
        autoWidth = FALSE,         
        dom = '<"top"f>rt<"bottom"lip><"clear">',
        columnDefs = list(
          list(width = '400px', targets = 0),
          list(width = '250px', targets = 1),
          list(width = '80px',  targets = 2),
          list(width = '150px', targets = 3),
          list(width = '80px',  targets = 4),
          list(width = '80px',  targets = 5)
        )
      )
    )
  }, server = FALSE)
  
  output$article_detail <- renderUI({
    req(input$tabla_articulos_rows_selected)
    
    # 1. Obtener la fila seleccionada de forma segura
    idx <- as.numeric(input$tabla_articulos_rows_selected[1])
    df <- datos_filtrados()
    
    if (is.na(idx) || idx < 1 || idx > nrow(df)) return(NULL)
    
    art <- df[idx, ]
    if (nrow(art) == 0 || is.na(art$paper_id[1])) return(NULL)
    
    title_val <- if (is.null(art$title[1]) || is.na(art$title[1])) "Sin título" else as.character(art$title[1])
    topic_val <- if (is.null(art$topic_label[1]) || is.na(art$topic_label[1])) "General" else as.character(art$topic_label[1])
    url_val   <- if (is.null(art$url[1]) || is.na(art$url[1])) "#" else as.character(art$url[1])
    
    # EXTRACCIÓN SEGURA DEL DOI
    doi_val <- if ("doi" %in% names(art) && !is.null(art$doi[1]) && !is.na(art$doi[1]) && nchar(trimws(as.character(art$doi[1]))) > 2) {
      as.character(art$doi[1])
    } else {
      "DOI no disponible"
    }
    
    abs_val <- art$abstract[1]
    abst <- if (is.null(abs_val) || is.na(abs_val) || as.character(abs_val) == "NA" || nchar(trimws(as.character(abs_val))) < 5) {
      "Abstract o resumen no disponible en la base de datos."
    } else {
      as.character(abs_val)
    }
    
    citas_num     <- if (is.null(art$citations[1]) || is.na(art$citations[1])) "0" else as.character(art$citations[1])
    descargas_num <- if (is.null(art$downloads[1]) || is.na(art$downloads[1])) "0" else as.character(art$downloads[1])
    
    # 3. Consulta de autores a prueba de fallos (tryCatch)
    lista_autores <- tryCatch({
      con <- dbConnect(RSQLite::SQLite(), dbname = "Springer_Visual_Miner.sqlite")
      res <- character()
      if (dbExistsTable(con, "paper_authors") && dbExistsTable(con, "authors")) {
        q_auth <- sprintf("SELECT a.author_name FROM authors a JOIN paper_authors pa ON a.author_id = pa.author_id WHERE pa.paper_id = '%s'", gsub("'", "''", art$paper_id[1]))
        res <- dbGetQuery(con, q_auth)$author_name
      }
      dbDisconnect(con)
      res
    }, error = function(e) {
      if (exists("con") && inherits(con, "SQLiteConnection")) dbDisconnect(con)
      character()
    })
    
    # Respaldo por si la tabla relacional está vacía
    if (length(lista_autores) == 0) {
      lista_autores <- if (!is.null(art$authors_raw[1]) && !is.na(art$authors_raw[1])) strsplit(as.character(art$authors_raw[1]), ",\\s*")[[1]] else "Autor no especificado"
    }
    
    # 4. Retorno de un ÚNICO contenedor div principal
    div(style = "max-width: 88vw; width: 100%; box-sizing: border-box; display: block; margin-top: 35px; clear: both; overflow: hidden; padding: 5px;",
        
        # TÍTULO
        div(class = "detail-header mb-4", style = "width: 100%; display: block; white-space: normal;",
            span(class = "badge mb-2 bg-primary", style = "padding: 6px 12px; font-size: 11px;", topic_val), 
            tags$h3(class = "text-white Syne fw-bold", 
                    style = "white-space: normal !important; word-break: break-word !important; line-height: 1.4; max-width: 100%; margin-top: 8px; font-size: 1.6rem; display: block;", 
                    title_val)
        ),
        
        # BLOQUES KPI
        div(class = "row g-3 mb-4", style = "max-width: 100%; margin: 0;",
            div(class = "col-6", style = "padding-left: 0;",
                div(style = "background: rgba(255,255,255,0.04); border: 1px solid rgba(255,255,255,0.09); padding: 16px; border-radius: 12px; text-align: center; box-shadow: inset 0 1px 0 rgba(255,255,255,0.02);",
                    div(style = "font-size: 26px; font-weight: 700; color: #6366f1; font-family: monospace; line-height: 1.2;", citas_num), 
                    div(style = "font-size: 11px; text-transform: uppercase; letter-spacing: 0.5px; color: rgba(255,255,255,0.4); margin-top: 5px; font-weight: 600;", "Citas")
                )
            ),
            div(class = "col-6", style = "padding-right: 0;",
                div(style = "background: rgba(255,255,255,0.04); border: 1px solid rgba(255,255,255,0.09); padding: 16px; border-radius: 12px; text-align: center; box-shadow: inset 0 1px 0 rgba(255,255,255,0.02);",
                    div(style = "font-size: 26px; font-weight: 700; color: #10b981; font-family: monospace; line-height: 1.2;", descargas_num), 
                    div(style = "font-size: 11px; text-transform: uppercase; letter-spacing: 0.5px; color: rgba(255,255,255,0.4); margin-top: 5px; font-weight: 600;", "Descargas")
                )
            )
        ),
        
        # RESUMEN (ABSTRACT)
        div(class = "mb-4", style = "width: 100%; display: block; white-space: normal;",
            tags$h5(class = "text-white small fw-bold", style = "text-transform: uppercase; letter-spacing: 0.5px; color: rgba(255,255,255,0.5); font-size: 12px; margin-bottom: 8px;", "Resumen"), 
            div(class = "abstract-box text-white-50 text-justify", style = "font-size: 14px; line-height: 1.6; color: rgba(255,255,255,0.7) !important; word-break: break-word;", abst)
        ),
        
        # AUTORES
        div(class = "mb-4", style = "width: 100%; display: block;",
            tags$h5(class = "text-white small fw-bold", style = "text-transform: uppercase; letter-spacing: 0.5px; color: rgba(255,255,255,0.5); font-size: 12px; margin-bottom: 8px;", "Autores"), 
            div(class = "d-flex flex-wrap gap-2 mt-2", 
                lapply(lista_autores, function(a) {
                  span(class = "author-pill meta-pill", style = "background: rgba(255,255,255,0.07); color: #fff; padding: 4px 12px; border-radius: 20px; font-size: 12px; border: 1px solid rgba(255,255,255,0.1); display: inline-block;", a)
                })
            )
        ),
        
        # NUEVO: CAJA DEL DOI 
        div(class = "mb-4", style = "width: 100%; display: block;",
            tags$h5(class = "text-white small fw-bold", style = "text-transform: uppercase; letter-spacing: 0.5px; color: rgba(255,255,255,0.5); font-size: 12px; margin-bottom: 8px;", "Identificador Digital (DOI)"),
            div(style = "background: rgba(255,255,255,0.02); border: 1px solid rgba(255,255,255,0.06); padding: 8px 14px; border-radius: 8px; display: inline-block;",
                tags$i(class = "fa-solid fa-fingerprint", style = "color: #a5b4fc; margin-right: 8px; opacity: 0.8;"),
                tags$span(style = "font-family: 'IBM Plex Mono', monospace; font-size: 13px; color: #a5b4fc; letter-spacing: 0.5px;", doi_val)
            )
        ),
        
        # BOTÓN DE ENLACE EXTERNO
        div(class = "mt-4 pt-3 border-top border-secondary",
            tags$a(href = url_val, target = "_blank", class = "btn btn-primary w-100 btn-custom", style = "padding: 10px; font-weight: 600;", "Ver en SpringerLink")
        )
    )
  })
  # ============================================================================
  # 7. PÁGINA 4: AUTHORS INTELLIGENCE
  # ============================================================================
  obtener_stats_autores <- reactive({
    df <- datos_filtrados()
    if(nrow(df)==0) return(NULL)
    con <- dbConnect(RSQLite::SQLite(), dbname = "Springer_Visual_Miner.sqlite")
    rel <- if(dbExistsTable(con, "paper_authors")) dbReadTable(con, "paper_authors") else data.frame()
    aut <- if(dbExistsTable(con, "authors")) dbReadTable(con, "authors") else data.frame()
    dbDisconnect(con)
    if(nrow(rel)==0 || nrow(aut)==0) return(NULL)
    
    rel %>% filter(paper_id %in% df$paper_id) %>% count(author_id) %>% left_join(aut, by="author_id") %>% arrange(desc(n))
  })
  
  output$author_kpi_total <- renderUI({
    st <- obtener_stats_autores()
    val <- if(is.null(st)) 0 else nrow(st)
    div(class="glass-card card-body text-center", tags$h2(val, class="text-white"), tags$p("Autores Únicos", class="text-muted m-0"))
  })
  
  output$author_kpi_top <- renderUI({
    st <- obtener_stats_autores()
    val <- if(is.null(st) || nrow(st)==0) "—" else st$author_name[1]
    div(class="glass-card card-body text-center", tags$h3(val, class="text-white text-truncate"), tags$p("Autor Más Frecuente", class="text-muted m-0"))
  })
  
  output$author_kpi_prolific <- renderUI({
    st <- obtener_stats_autores()
    val <- if(is.null(st) || nrow(st)==0) 0 else st$n[1]
    div(class="glass-card card-body text-center", tags$h2(val, class="text-white"), tags$p("Max. Papers / Autor", class="text-muted m-0"))
  })
  
  output$plot_top_autores <- renderHighchart({
    st <- obtener_stats_autores()
    if(is.null(st) || nrow(st)==0) return(highchart())
    hchart(head(st, 15), "bar", hcaes(x = author_name, y = n)) %>%
      hc_chart(backgroundColor="transparent") %>% hc_colors("#22D3EE") %>% hc_xAxis(labels=list(style=list(color="#8E9AA8")))
  })
  
  output$ranking_autores_html <- renderUI({
    st <- obtener_stats_autores()
    if(is.null(st) || nrow(st) == 0) return(tags$p(class="text-muted small", "No hay datos de autores disponibles para este filtro."))
    top <- head(st, 10)
    
    tags$ul(class="ranking-list",
            lapply(1:nrow(top), function(i) {
              cl_pos <- if(i==1) "gold" else if(i==2) "silver" else if(i==3) "bronze" else ""
              tags$li(class="ranking-item",
                      div(class=paste("ranking-pos", cl_pos), paste0("#", i)),
                      div(class="ranking-text", tags$span(class="r-name", top$author_name[i])),
                      div(class="ranking-val", paste(top$n[i], "papers"))
              )
            })
    )
  })
  
  # ============================================================================
  # 8. PÁGINA 5: RESEARCH INSIGHTS
  # ============================================================================
  output$insights_cards <- renderUI({
    df <- datos_filtrados()
    if(nrow(df)==0) return(NULL)
    
    top_cat <- df %>% count(topic_label) %>% slice_max(n, n=1, with_ties = FALSE) %>% pull(topic_label)
    avg_cit <- round(mean(df$citations, na.rm=T), 1)
    avg_dwn <- round(mean(df$downloads, na.rm=T), 1)
    
    div(class="insights-grid",
        div(class="insight-card", span(class="ic-badge dominant", "Dominante"),
            tags$h4("Categoría Principal"), tags$p(paste("El tema más frecuente es", top_cat))),
        div(class="insight-card", span(class="ic-badge trending", "Impacto"),
            tags$h4("Promedio de Citas"), tags$p(paste("Los artículos reciben una media de", avg_cit, "citas."))),
        div(class="insight-card", span(class="ic-badge rising", "Tracción"),
            tags$h4("Interés del Público"), tags$p(paste("Descargas promedio por documento:", avg_dwn)))
    )
  })
  
  # FIX
  output$top_impact_papers_html <- renderUI({
    df <- datos_filtrados()
    if(nrow(df)==0) return(NULL)
    
    df <- df %>% 
      mutate(score = (ifelse(is.na(citations), 0, citations) * 2) + ifelse(is.na(downloads), 0, downloads)) %>% 
      arrange(desc(score)) %>% 
      head(5)
    
    tags$ul(class="ranking-list",
            lapply(1:nrow(df), function(i) {
              tags$li(class="ranking-item",
                      div(class="ranking-pos", paste0(i)),
                      div(class="ranking-text", tags$span(class="r-name", df$title[i]), tags$span(class="r-sub text-muted", df$topic_label[i])),
                      div(class="ranking-val text-success", paste("Score:", round(df$score[i])))
              )
            })
    )
  })
  
  output$plot_comparativa_temas <- renderHighchart({
    df <- datos_filtrados()
    if(nrow(df)==0) return(highchart())
    res <- df %>% group_by(topic_label) %>% summarise(c = mean(citations,na.rm=T), d = mean(downloads,na.rm=T))
    
    highchart() %>% hc_chart(type = "column", backgroundColor="transparent") %>%
      hc_xAxis(categories = as.list(res$topic_label), labels=list(style=list(color="#8E9AA8"))) %>%
      hc_add_series(name="Promedio Citas", data=as.list(round(res$c,1)), color="#F43F5E") %>%
      hc_add_series(name="Promedio Descargas", data=as.list(round(res$d,1)), color="#10B981")
  })
  
  # ============================================================================
  # 9. PÁGINA 6: ADMINISTRATION
  # ============================================================================
  output$admin_status_detail <- renderUI({
    
    reporte <- ultimo_reporte()
    
    color_estado <- switch(estado_scraping(),
                           "online"     = "#10b981",
                           "syncing"    = "#f59e0b",
                           "rebuilding" = "#f43f5e",
                           "#64748b")
    
    cabecera <- tagList(
      tags$h4(class = "text-white mb-1", "Centro de Control de Scraping"),
      tags$p(style = paste0("color:", color_estado, "; font-weight:600; font-size:12px; margin-bottom:10px;"),
             paste("Estado actual:", toupper(estado_scraping())))
    )
    
    if (is.null(reporte)) {
      return(tagList(
        cabecera,
        tags$p(class = "text-muted small", estado_mensaje()),
        tags$p(style = "font-size:11px; color: rgba(255,255,255,0.3); margin-top:8px;",
               "Motor: Crossref API + HTTP directo (sin navegador headless).")
      ))
    }
    
    chip <- function(valor, etiqueta, color) {
      div(class = "sync-chip",
          tags$b(style = paste0("color:", color, ";"), valor),
          tags$span(etiqueta))
    }
    
    tagList(
      cabecera,
      
      div(style = paste0("border-left:3px solid ",
                         if (isTRUE(reporte$ok)) "#10b981" else "#f43f5e",
                         "; padding-left:12px; margin-bottom:14px;"),
          tags$div(style = "font-size:13px; font-weight:700; color: var(--text-primary);", reporte$titulo),
          tags$div(style = "font-size:12px; color: rgba(255,255,255,0.60); margin-top:4px; line-height:1.5;", reporte$mensaje)
      ),
      
      div(style = "display:flex; gap:8px; flex-wrap:wrap; margin-bottom:12px;",
          chip(reporte$n_nuevos,             "Nuevos",      "#10b981"),
          chip(reporte$n_actualizados,       "Refrescados", "#22d3ee"),
          chip(reporte$n_metricas_cambiadas, "Con cambio",  "#f59e0b"),
          chip(length(reporte$fallidos),     "Fallidos",    "#f43f5e")
      ),
      
      if (length(reporte$detalle) > 0) {
        tags$details(
          tags$summary(style = "font-size:11px; color:#a5b4fc; cursor:pointer; margin-bottom:6px;", "Ver detalle"),
          tags$ul(style = "margin:6px 0 0 0; padding-left:16px;",
                  lapply(reporte$detalle, function(x)
                    tags$li(style = "font-size:11px; color: rgba(255,255,255,0.5); line-height:1.6;", x)))
        )
      },
      
      tags$p(style = "font-size:10px; color: rgba(255,255,255,0.28); margin-top:12px;",
             sprintf("Duracion: %.1f s \u00b7 Motor: Crossref API + HTTP directo", reporte$segundos))
    )
  })
  
  output$db_stat_papers <- renderUI({ div(class="kpi-card", tags$h3(nrow(datos_reactivos())), tags$p("Documentos en DB")) })
  output$db_stat_authors <- renderUI({ 
    con<-dbConnect(RSQLite::SQLite(),"Springer_Visual_Miner.sqlite")
    n<-if(dbExistsTable(con,"authors")) dbGetQuery(con,"SELECT COUNT(*) as n FROM authors")$n else 0
    dbDisconnect(con)
    div(class="kpi-card", tags$h3(n), tags$p("Autores Indexados")) 
  })
  output$db_stat_refs <- renderUI({ 
    con <- dbConnect(RSQLite::SQLite(), "Springer_Visual_Miner.sqlite")
    n <- if(dbExistsTable(con, "references")) dbGetQuery(con, 'SELECT COUNT(*) as n FROM "references"')$n else 0
    dbDisconnect(con)
    div(class="kpi-card", tags$h3(n), tags$p("Referencias Mapeadas")) 
  })
  output$db_stat_topics <- renderUI({ div(class="kpi-card", tags$h3(length(unique(datos_reactivos()$topic_label))), tags$p("Clústers Temáticos")) })
  
  # ============================================================================
  # 10. EVENTOS DE OPERACIONES Y FORZADO DE RENDERIZADO
  # ============================================================================
  observeEvent(input$btn_actualizar, {
    correr_sincronizacion("incremental")
  }, ignoreInit = TRUE)
  
  elementos_ui_ocultos <- c("plot_tendencia", "plot_categorias", "plot_bubble", "plot_pareto",
                            "plot_citas_anio", "plot_downloads_tiempo", "plot_heatmap", "plot_treemap",
                            "plot_top_papers_citas", "plot_top_papers_downloads", "plot_top_autores",
                            "plot_comparativa_temas","tabla_articulos", "ranking_autores_html", "top_impact_papers_html")
  
  for (el in elementos_ui_ocultos) {
    outputOptions(output, el, suspendWhenHidden = FALSE)
  }
  
  # ============================================================================
  # 11. SCRAPING EVENTS (Administración y Zona de Seguridad)
  # ============================================================================

  observe({
    req(input$superuser_key)
    

    if (input$superuser_key == SUPERUSER_PASS) {

      shinyjs::enable("btn_emergencia")
      shinyjs::runjs("$('#btn_emergencia').css({'opacity': '1', 'cursor': 'pointer', 'box-shadow': '0 0 15px rgba(251,113,133,0.4)'});")
      shinyjs::runjs("$('#superuser_key').css({'border-color': '#34D399', 'box-shadow': '0 0 8px rgba(52,211,153,0.3)'});")
    } else {

      shinyjs::disable("btn_emergencia")
      shinyjs::runjs("$('#btn_emergencia').css({'opacity': '0.5', 'cursor': 'not-allowed', 'box-shadow': 'none'});")
      

      if (nchar(input$superuser_key) > 0) {
        shinyjs::runjs("$('#superuser_key').css({'border-color': '#fb7185', 'box-shadow': '0 0 8px rgba(251,113,133,0.3)'});")
      } else {

        shinyjs::runjs("$('#superuser_key').css({'border-color': '', 'box-shadow': 'none'});")
      }
    }
  })
  # ----------------------------------------------------------------------------
  

  observeEvent(input$btn_emergencia, {
    showModal(modalDialog(
      title = tags$h3(style = "color: #e11d48;", tags$i(class = "fa-solid fa-triangle-exclamation"), " ¡ADVERTENCIA CRÍTICA!"),
      tags$p("Estás a punto de iniciar la reconstrucción total de la base de datos."),
      tags$p("Esta acción purgará los datos actuales y forzará un nuevo scraping intensivo de 35 páginas. Este proceso tardará varios minutos y no se puede deshacer."),
      tags$strong("¿Estás absolutamente seguro de continuar?"),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("btn_confirmar_emergencia", "Sí, Reconstruir BD", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  

  observeEvent(input$btn_confirmar_emergencia, {
    removeModal()
    
    if (!identical(input$superuser_key, SUPERUSER_PASS)) {
      showNotification("PIN incorrecto. Operacion cancelada.", type = "error")
      return()
    }
    
    correr_sincronizacion("completo")
    
    updateTextInput(session, "superuser_key", value = "")
    shinyjs::disable("btn_emergencia")
    shinyjs::runjs("$('#btn_emergencia').css({'opacity': '0.5', 'cursor': 'not-allowed', 'box-shadow': 'none'});")
    shinyjs::runjs("$('#superuser_key').css({'border-color': '', 'box-shadow': 'none'});")
  }, ignoreInit = TRUE)
  
  # =====================================================================
  # FIX: Prevenir que Shiny suspenda elementos al estar en otras pestañas
  # =====================================================================
  
  # 1. Elementos de Literature Explorer 
  outputOptions(output, "tabla_articulos", suspendWhenHidden = FALSE)
  outputOptions(output, "article_detail", suspendWhenHidden = FALSE)
  
  # 2. Elementos del Dashboard 
  outputOptions(output, "kpi_articulos", suspendWhenHidden = FALSE)
  outputOptions(output, "kpi_autores", suspendWhenHidden = FALSE)
  outputOptions(output, "kpi_citas", suspendWhenHidden = FALSE)
  outputOptions(output, "kpi_descargas", suspendWhenHidden = FALSE)
  
  outputOptions(output, "plot_tendencia", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_categorias", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_bubble", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_pareto", suspendWhenHidden = FALSE)
  
  # ============================================================================
  # 12. BUSQUEDA POR DOI
  # ============================================================================
  observeEvent(input$btn_buscar_doi, {
    d <- normalizar_doi(input$busqueda_doi)
    if (is.na(d)) {
      doi_consultado(list(estado = "invalido", texto = input$busqueda_doi))
    } else {
      doi_consultado(list(estado = "ok", doi = d))
    }
    shinyjs::runjs("switchPage('literature');")
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_limpiar_doi, {
    updateTextInput(session, "busqueda_doi", value = "")
    doi_consultado(NULL)
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_importar_doi, {
    st <- doi_consultado()
    req(st, st$doi)
    
    res <- NULL
    withProgress(message = "Importando DOI desde Crossref", value = 0.4, {
      res <- tryCatch(importar_doi(st$doi), error = function(e)
        list(ok = FALSE, mensaje = paste("Error:", conditionMessage(e))))
      
      if (isTRUE(res$ok)) {
        setProgress(0.8, detail = "Normalizando tablas...")
        normalizar_tablas()
        cargar_datos()
      }
    })
    
    showNotification(res$mensaje, type = if (isTRUE(res$ok)) "message" else "warning", duration = 10)
    doi_consultado(list(estado = "ok", doi = st$doi))
  }, ignoreInit = TRUE)
  
  output$resultado_doi <- renderUI({
    
    st <- doi_consultado()
    if (is.null(st)) return(NULL)
    
    if (identical(st$estado, "invalido")) {
      return(div(class = "doi-result error",
                 tags$span(class = "doi-code", "formato invalido"),
                 tags$h4("Eso no parece un DOI"),
                 tags$p(style = "font-size:12px; color: rgba(255,255,255,0.55); margin:0; line-height:1.6;",
                        "Un DOI empieza por 10. seguido de 4 a 9 digitos y una barra. Ejemplo: 10.1007/s10462-024-10731-4")
      ))
    }
    
    d    <- st$doi
    fila <- buscar_doi_local(d)
    
    # --- Encontrado en el corpus ---
    if (!is.null(fila) && nrow(fila) > 0) {
      a <- fila[1, ]
      return(div(class = "doi-result",
                 tags$span(class = "doi-code", a$doi),
                 tags$h4(a$title),
                 tags$p(style = "font-size:12px; color: rgba(255,255,255,0.55); margin:0; line-height:1.6;",
                        str_trunc(ifelse(is.na(a$abstract) | a$abstract == "NA",
                                         "Sin abstract registrado.", a$abstract), 340)),
                 div(class = "doi-meta",
                     div(tags$strong(ifelse(is.na(a$year), "\u2014", a$year)), "A\u00f1o"),
                     div(tags$strong(ifelse(is.na(a$citations), 0, a$citations)), "Citas"),
                     div(tags$strong(ifelse(is.na(a$downloads), 0, a$downloads)), "Accesos"),
                     div(tags$strong(ifelse(is.na(a$n_authors), 0, a$n_authors)), "Autores"),
                     div(tags$strong(style = "font-size:13px;", a$topic_label), "Categor\u00eda")
                 ),
                 div(style = "margin-top:16px;",
                     tags$a(href = a$url, target = "_blank", class = "doi-btn",
                            tagList(tags$i(class = "fa-solid fa-arrow-up-right-from-square"), " Ver en SpringerLink"))
                 )
      ))
    }
    
    # --- No esta en el corpus ---
    sug <- sugerir_dois(d, n = 5)
    
    div(class = "doi-result miss",
        tags$span(class = "doi-code", d),
        tags$h4("Ese DOI no esta en el corpus indexado"),
        tags$p(style = "font-size:12px; color: rgba(255,255,255,0.55); margin:0 0 14px 0; line-height:1.6;",
               "Puede ser anterior al rango de fechas cargado, o pertenecer a otra revista. Puedes traerlo directo desde Crossref."),
        
        if (nrow(sug) > 0) {
          tagList(
            tags$div(style = "font-size:11px; text-transform:uppercase; letter-spacing:0.5px; color: rgba(255,255,255,0.35); margin-bottom:6px;",
                     "DOIs parecidos en tu base"),
            tags$ul(style = "margin:0 0 14px 0; padding-left:16px;",
                    lapply(seq_len(nrow(sug)), function(i)
                      tags$li(style = "font-size:11px; color: rgba(255,255,255,0.5); line-height:1.7;",
                              tags$code(style = "color:#a5b4fc;", sug$doi[i]), " - ", str_trunc(sug$title[i], 60))))
          )
        },
        
        actionButton("btn_importar_doi", class = "doi-btn warn",
                     label = tagList(tags$i(class = "fa-solid fa-cloud-arrow-down"), " Importar este DOI"))
    )
  })
  
  outputOptions(output, "resultado_doi", suspendWhenHidden = FALSE)

  # ============================================================================
  # 13. BUSCADOR DE ARTICULOS (Taller 4)
  # ============================================================================

  consulta_activa <- reactiveVal(NULL)
  

  desfase_indice <- reactive({
    if (is.null(INDICE_RI)) return(0L)
    d <- datos_reactivos()
    if (is.null(d) || nrow(d) == 0) return(0L)
    max(0L, nrow(d) - nrow(INDICE_RI$meta))
  })
  
  output$bus_estado_indice <- renderUI({
    if (is.null(INDICE_RI)) {
      return(tags$span(style = "color:#f43f5e;", "indice no encontrado"))
    }
    p <- INDICE_RI$parametros
    base <- sprintf("%d articulos | %s terminos | %d componentes LSA",
                    nrow(INDICE_RI$meta),
                    format(length(INDICE_RI$vocabulario), big.mark = ","),
                    p$k_final)
    n <- desfase_indice()
    if (n == 0) return(tags$span(base))
    tagList(tags$span(base),
            tags$span(style = "color:#fbbf24; font-weight:600;",
                      sprintf("  \u00b7  %d sin indexar", n)))
  })
  
  lanzar_busqueda <- function(txt) {
    if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
    consulta_activa(list(texto = trimws(txt),
                         estrategia = input$bus_estrategia,
                         n = as.integer(input$bus_top_n),
                         sello = Sys.time()))
  }
  
  observeEvent(input$bus_buscar, { lanzar_busqueda(input$bus_consulta) }, ignoreInit = TRUE)
  
  # Recalcular al cambiar estrategia o numero de resultados, sin volver a pulsar
  observeEvent(list(input$bus_estrategia, input$bus_top_n), {
    if (!is.null(consulta_activa())) lanzar_busqueda(consulta_activa()$texto)
  }, ignoreInit = TRUE)
  
  observeEvent(input$bus_limpiar, {
    updateTextAreaInput(session, "bus_consulta", value = "")
    consulta_activa(NULL)
  }, ignoreInit = TRUE)
  
  # Consultas de ejemplo
  ejemplos <- c(
    bus_ej1 = "generative AI for medical diagnosis",
    bus_ej2 = "explainable machine learning",
    bus_ej3 = "aprendizaje profundo en imagenes medicas",
    bus_ej4 = "swarm optimization algorithms",
    bus_ej5 = "privacidad y seguridad en federated learning"
  )
  lapply(names(ejemplos), function(id) {
    observeEvent(input[[id]], {
      updateTextAreaInput(session, "bus_consulta", value = unname(ejemplos[id]))
      lanzar_busqueda(unname(ejemplos[id]))
    }, ignoreInit = TRUE)
  })
  
  output$bus_diagnostico <- renderUI({
    st <- consulta_activa()
    if (is.null(st) || is.null(INDICE_RI)) return(NULL)
    d <- diagnosticar_consulta(st$texto, INDICE_RI)
    tags$span(d$mensaje)
  })
  
  # ----------------------------------------------------------------------------
  # Render de un resultado individual
  # ----------------------------------------------------------------------------
  .tarjeta_hit <- function(r, i, etiqueta_puntaje) {
    fecha <- if (is.na(r$fecha[i]) || r$fecha[i] == "NA") "s. f." else r$fecha[i]
    aut   <- if (is.na(r$autores[i])) "Autores no registrados" else r$autores[i]
    if (nchar(aut) > 110) aut <- paste0(substr(aut, 1, 110), " et al.")
    
    div(class = "hit",
        div(class = "hit-rank", r$posicion[i]),
        div(class = "hit-body",
            div(class = "hit-title", r$titulo[i]),
            div(class = "hit-meta",
                aut, tags$span(class = "sep", "|"),
                fecha, tags$span(class = "sep", "|"),
                sprintf("%s citas", ifelse(is.na(r$citas[i]), 0, r$citas[i]))
            ),
            div(class = "hit-frag", r$fragmento[i]),
            div(class = "hit-foot",
                tags$a(class = "hit-doi", href = r$url[i], target = "_blank", r$doi[i]),
                tags$span(class = "hit-tag", r$tema[i])
            )
        ),
        div(class = "hit-score",
            tags$b(format(round(r$puntaje[i], 4), nsmall = 4)),
            tags$span(etiqueta_puntaje),
            div(class = "hit-bar",
                div(style = sprintf("width:%.0f%%;", 100 * max(0.04, r$puntaje_norm[i]))))
        )
    )
  }
  
  .etiqueta <- function(est) switch(est,
                                    bm25    = "BM25",
                                    tfidf   = "coseno TF-IDF",
                                    lsa     = "coseno LSA",
                                    hibrido = "RRF",
                                    "puntaje")
  
  .bloque_resultados <- function(r, est, titulo_card, subtitulo) {
    div(class = "glass-card",
        div(class = "card-header",
            div(class = "card-header-icon", tags$i(class = "fa-solid fa-list-ol")),
            tags$h3(titulo_card),
            tags$span(class = "card-sub", subtitulo)
        ),
        div(class = "card-body",
            if (nrow(r) == 0) {
              tags$p(style = "color:rgba(255,255,255,0.45); font-size:12px; margin:0;",
                     "Sin resultados para esta estrategia.")
            } else {
              lapply(seq_len(nrow(r)), function(i) .tarjeta_hit(r, i, .etiqueta(est)))
            }
        )
    )
  }
  
  # ----------------------------------------------------------------------------
  # Salida principal
  # ----------------------------------------------------------------------------
  output$bus_resultados <- renderUI({
    
    if (is.null(INDICE_RI)) {
      return(div(class = "bus-aviso",
                 tags$strong("El indice de busqueda no esta disponible."), tags$br(),
                 "Falta el archivo search_index.rds. Genera el indice ejecutando ",
                 tags$code("source(\"build_index.R\")"),
                 " en la carpeta de la aplicacion."))
    }
    
    aviso_desfase <- if (desfase_indice() > 0) {
      div(class = "bus-aviso", style = "border-color: rgba(245,158,11,0.35);",
          tags$strong(sprintf("El indice esta %d articulo(s) por detras de la base. ",
                              desfase_indice())),
          "Los articulos sincronizados despues de construir el indice aparecen en el ",
          "resto de la aplicacion pero no son buscables todavia. Para incorporarlos: ",
          tags$code("source(\"pipeline.R\"); construir_indice()"),
          " y reinicia la aplicacion.")
    } else NULL
    
    st <- consulta_activa()
    if (is.null(st)) return(aviso_desfase)
    
    diag <- diagnosticar_consulta(st$texto, INDICE_RI)
    if (!isTRUE(diag$ok)) {
      return(div(class = "bus-aviso",
                 tags$strong("Sin coincidencias."), tags$br(), diag$mensaje, tags$br(), tags$br(),
                 "El corpus esta en ingles. Una consulta en espanol se traduce termino a termino ",
                 "con un lexico de dominio; si usa vocabulario fuera de ese lexico, conviene ",
                 "reformularla en ingles."))
    }
    
    t0 <- Sys.time()
    
    # --- Modos de comparacion lado a lado (punto 2.3 del enunciado) -----------
    if (st$estrategia %in% c("comparar", "ablacion")) {
      
      ablacion <- identical(st$estrategia, "ablacion")
      est_a    <- if (ablacion) "tfidf" else "bm25"
      
      ra <- buscar_articulos(st$texto, INDICE_RI, estrategia = est_a, n = st$n)
      rb <- buscar_articulos(st$texto, INDICE_RI, estrategia = "lsa", n = st$n)
      ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
      
      comunes <- length(intersect(head(ra$doi, 5), head(rb$doi, 5)))
      
      nota <- if (ablacion) {
        tagList(
          tags$strong("Ablacion de la reduccion dimensional. "),
          sprintf("Ambas columnas usan el MISMO pesado tf-idf y la MISMA similitud coseno. Lo unico que cambia es el espacio: %s dimensiones a la izquierda, %d componentes latentes a la derecha. ",
                  format(length(INDICE_RI$vocabulario), big.mark = ","),
                  INDICE_RI$parametros$k_final),
          "Toda diferencia de ranking es atribuible a la reduccion. Aqui los puntajes SI son comparables: ambos son cosenos."
        )
      } else {
        tagList(
          tags$strong("Los puntajes no son comparables entre columnas: "),
          "BM25 no esta acotado y el coseno LSA vive en [-1, 1]. Lo comparable son las posiciones."
        )
      }
      
      return(tagList(
        aviso_desfase,
        div(class = "bus-aviso",
            sprintf("Consulta: \"%s\". Coincidencias en el top-5: %d de 5. ", st$texto, comunes),
            nota, sprintf(" Tiempo total: %.0f ms.", ms)),
        div(class = "bus-cols",
            div(.bloque_resultados(ra, est_a,
                                   if (ablacion) "TF-IDF (espacio original, sin reducir)" else "BM25 (lexica)",
                                   sprintf("%s dimensiones", format(length(INDICE_RI$vocabulario), big.mark = ",")))),
            div(.bloque_resultados(rb, "lsa", "LSA (espacio reducido)",
                                   sprintf("%d componentes", INDICE_RI$parametros$k_final)))
        )
      ))
    }
    
    # --- Modo estrategia unica ------------------------------------------------
    r  <- buscar_articulos(st$texto, INDICE_RI, estrategia = st$estrategia, n = st$n)
    ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
    
    descripcion <- switch(st$estrategia,
                          tfidf = paste("Recuperacion lexica en el espacio disperso ORIGINAL de",
                                        format(length(INDICE_RI$vocabulario), big.mark = ","),
                                        "dimensiones. Pesado tf-idf con tf sublineal y similitud coseno.",
                                        "Es la linea base contra la que se mide el efecto de la reduccion."),
                          bm25 = paste("Recuperacion lexica sobre el espacio disperso completo de",
                                       format(length(INDICE_RI$vocabulario), big.mark = ","),
                                       "terminos. Puntaje BM25 (k1 =",
                                       INDICE_RI$parametros$bm25_k1, ", b =",
                                       INDICE_RI$parametros$bm25_b, ")."),
                          lsa = paste("Recuperacion semantica: la consulta se proyecta al espacio latente de",
                                      INDICE_RI$parametros$k_final,
                                      "componentes con la misma matriz V del SVD y se compara por coseno."),
                          hibrido = paste("Fusion de rangos (Reciprocal Rank Fusion, k = 60) entre BM25 y LSA.",
                                          "Opera sobre posiciones, no sobre puntajes, porque las escalas difieren."))
    
    tagList(
      aviso_desfase,
      div(class = "bus-aviso",
          sprintf("%d resultados en %.0f ms. ", nrow(r), ms), descripcion),
      .bloque_resultados(r, st$estrategia,
                         sprintf("Resultados (%s)", .etiqueta(st$estrategia)),
                         sprintf("consulta: %s", st$texto))
    )
  })
  
  outputOptions(output, "bus_resultados", suspendWhenHidden = FALSE)

}
