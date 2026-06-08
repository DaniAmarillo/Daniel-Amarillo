server <- function(input, output, session) {
  # ============================================================================
  # 1. DATOS REACTIVOS
  # ============================================================================
  datos_reactivos <- reactiveVal(data.frame())
  estado_scraping <- reactiveVal("online")
  estado_mensaje  <- reactiveVal("Sistema en línea y operando de forma óptima.")
  
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
    tagList(
      tags$h4(class="text-white mb-1", "Centro de Control de Scraping"),
      tags$p(class=if(estado_scraping()=="online") "text-success" else "text-warning", paste("Estado actual:", toupper(estado_scraping()))),
      tags$p(class="text-muted small", estado_mensaje())
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
  # 10. EVENTOS DE OPERACIONES Y FORZADO DE RENDERIZADO (Pestañas Ocultas)
  # ============================================================================
  observeEvent(input$btn_actualizar, {
    estado_scraping("syncing")
    estado_mensaje("Iniciando motor de extracción...")
    
    withProgress(message = 'Sincronizando con Springer', detail = 'Conectando...', value = 0.3, {
      msg <- tryCatch({ 
        incProgress(0.2, detail = "Extrayendo nuevos artículos...")
        ejecutar_scraping(FALSE)
        
        incProgress(0.2, detail = "Estructurando base de datos...")
        normalizar_tablas()
        
        incProgress(0.2, detail = "Cargando datos al sistema...")
        cargar_datos()
        
        "Sincronización exitosa" 
      }, error=function(e) paste("Error:", e$message))
    })
    
    estado_scraping("online")
    estado_mensaje(msg)
    shinyjs::runjs("setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 200);")
  })
  
  observeEvent(input$btn_emergencia, {
    if(input$superuser_key != "1234") { showNotification("PIN incorrecto", type="error"); return() }
    
    estado_scraping("rebuilding")
    estado_mensaje("Purgando base de datos...")
    
    withProgress(message = 'Reconstrucción Crítica', detail = 'Borrando datos actuales...', value = 0.2, {
      msg <- tryCatch({ 
        incProgress(0.4, detail = "Scraping intensivo en curso (Modo Emergencia)...")
        ejecutar_scraping(TRUE)
        
        incProgress(0.2, detail = "Restructurando relaciones...")
        normalizar_tablas()
        cargar_datos()
        
        "Reconstrucción completada" 
      }, error=function(e) paste("Error:", e$message))
    })
    
    estado_scraping("online")
    estado_mensaje(msg)
  })
  
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
  
  observeEvent(input$btn_actualizar, {
    estado_scraping("syncing")
    estado_mensaje("Sincronizando con Springer Link...")
    withProgress(message = "Extrayendo novedades...", value = 0.5, {
      msg <- tryCatch(ejecutar_scraping(modo_emergencia = FALSE), error = function(e) paste("Error:", e$message))
      normalizar_tablas()
      cargar_datos()
    })
    estado_scraping("online")
    estado_mensaje(msg)
  })
  

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
    
    estado_scraping("rebuilding")
    estado_mensaje("Reconstrucción estructural iniciada...")
    
    withProgress(message = "Purgando y reconstruyendo BD...", detail = "Analizando 35 páginas", value = 0.5, {
      msg <- tryCatch(ejecutar_scraping(modo_emergencia = TRUE), error = function(e) paste("Error:", e$message))
      normalizar_tablas()
      cargar_datos()
    })
    
    estado_scraping("online")
    estado_mensaje(msg)
    
    updateTextInput(session, "superuser_key", value = "")
    shinyjs::disable("btn_emergencia")
    shinyjs::runjs("$('#btn_emergencia').css({'opacity': '0.5', 'cursor': 'not-allowed', 'box-shadow': 'none'});")
    shinyjs::runjs("$('#superuser_key').css({'border-color': '', 'box-shadow': 'none'});")
  })
  
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
  
}
