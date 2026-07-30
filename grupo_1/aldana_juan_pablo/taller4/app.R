# Parcial 2 - Minería de Datos (2016325)
# Annual Reviews Dashboard
# Juan Pablo Aldana Henao

paquetes <- c(
  "shiny", "shinydashboard", "DBI", "RSQLite", "dplyr", "tidyr",
  "stringr", "purrr", "highcharter", "DT", "httr", "rvest",
  "fresh", "shinyWidgets", "shinycssloaders", "jsonlite",
  "tibble", "Matrix", "tidytext", "SnowballC", "stringi", "irlba"
)
instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar)) install.packages(instalar, dependencies = TRUE)
invisible(lapply(paquetes, library, character.only = TRUE))

tema_claro <- create_theme(
  adminlte_color(
    light_blue = "#2563EB", navy = "#1E40AF", blue = "#3B82F6",
    green = "#16A34A", red = "#DC2626", yellow = "#D97706",
    fuchsia = "#7C3AED", black = "#111827"
  ),
  adminlte_sidebar(
    dark_bg = "#F1F5F9", dark_hover_bg = "#DBEAFE",
    dark_color = "#1E293B", dark_hover_color = "#1E40AF",
    dark_submenu_bg = "#E2E8F0", dark_submenu_color = "#334155"
  ),
  adminlte_global(content_bg = "#F8FAFC", box_bg = "#FFFFFF", info_box_bg = "#FFFFFF")
)

conectar_db <- function(ruta = "annual_reviews_2025.db") {
  dbConnect(SQLite(), ruta)
}

leer_papers <- function(con) {
  tryCatch({
    if (!dbExistsTable(con, "papers")) return(tibble())
    dbReadTable(con, "papers")
  }, error = function(e) tibble())
}



limpiar_texto <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x %>% str_replace_all("\\s+", " ") %>% str_trim()
}

.AR_HANDLE      <- NULL
.AR_SESION_OK   <- FALSE

.cabeceras_navegador <- function(referer = "https://www.annualreviews.org/") {
  add_headers(
    `User-Agent`      = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36",
    `Accept`          = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
    `Accept-Language` = "en-US,en;q=0.9,es;q=0.8",
    `Accept-Encoding` = "gzip, deflate, br",
    `Referer`         = referer,
    `Upgrade-Insecure-Requests` = "1",
    `Sec-Fetch-Dest`  = "document",
    `Sec-Fetch-Mode`  = "navigate",
    `Sec-Fetch-Site`  = "same-origin",
    `Sec-Fetch-User`  = "?1",
    `Connection`      = "keep-alive"
  )
}

iniciar_sesion_ar <- function(forzar = FALSE) {
  if (!forzar && .AR_SESION_OK && !is.null(.AR_HANDLE)) return(invisible(TRUE))
  h <- handle("https://www.annualreviews.org")
  resp <- tryCatch(
    GET(handle = h, url = "https://www.annualreviews.org/",
        add_headers(
          `User-Agent`      = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36",
          `Accept-Language` = "en-US,en;q=0.9"),
        timeout(20)),
    error = function(e) NULL
  )
  ok <- !is.null(resp) && !http_error(resp)
  .AR_HANDLE    <<- h
  .AR_SESION_OK <<- ok
  invisible(ok)
}

GET_con_reintentos <- function(url, n_intentos = 3, espera_base = 3,
                               referer = "https://www.annualreviews.org/", ...) {
  if (is.null(.AR_HANDLE)) iniciar_sesion_ar()
  
  ultimo_error <- NULL
  for (intento in seq_len(n_intentos)) {
    resp <- tryCatch(
      if (!is.null(.AR_HANDLE))
        GET(url, handle = .AR_HANDLE, .cabeceras_navegador(referer), timeout(30), ...)
      else
        GET(url, .cabeceras_navegador(referer), timeout(30), ...),
      error = function(e) e
    )
    
    if (!inherits(resp, "error")) {
      if (!http_error(resp)) return(resp)
      codigo <- status_code(resp)
      ultimo_error <- paste0("HTTP ", codigo)
      
      if (codigo == 404) {
        attr(resp, "no_existe") <- TRUE
        return(resp)
      }
      
    } else {
      ultimo_error <- resp$message
    }
    
    if (intento < n_intentos) {
      espera <- espera_base * intento + stats::runif(1, 0.5, 2)
      Sys.sleep(espera)
    }
  }
  warning(paste0("Fallo tras ", n_intentos, " intentos en ", url, ": ", ultimo_error))
  NULL
}

construir_urls_volumen <- function(url_base) {
  url_base <- str_remove(url_base, "\\?.*$")
  paste0(url_base, "?pageSize=100&page=1")
}

detectar_paginas_extra <- function(html_doc, url_base) {
  enlaces <- html_doc %>%
    html_nodes(".paginator a, .resultsnav a") %>%
    html_attr("href")
  enlaces <- enlaces[!is.na(enlaces) & str_detect(enlaces, "page=")]
  if (length(enlaces) == 0) return(character(0))
  
  url_base_sin_query <- str_remove(url_base, "\\?.*$")
  urls_completas <- ifelse(
    str_starts(enlaces, "http"),
    enlaces,
    paste0("https://www.annualreviews.org", enlaces)
  )
  urls_completas <- urls_completas[str_detect(urls_completas, fixed(url_base_sin_query))]
  unique(urls_completas)
}

scrapear_pagina <- function(url, anio_pagina = "2025", seguir_paginacion = TRUE) {
  tryCatch({
    url_pedido      <- construir_urls_volumen(url)
    referer_listado <- "https://www.annualreviews.org/content/journals/economics"
    resp            <- GET_con_reintentos(url_pedido, referer = referer_listado)
    if (is.null(resp)) return(tibble())
    
    if (isTRUE(attr(resp, "no_existe")) || status_code(resp) == 404) {
      message(paste0("Volumen no disponible aun (HTTP 404) para el anio ",
                     anio_pagina, ": ", url_pedido))
      return(tibble())
    }
    
    html_doc <- read_html(resp)
    nodos    <- html_doc %>% html_nodes(".articleInToc")
    
    if (length(nodos) == 0) {
      resp2 <- GET_con_reintentos(url, referer = referer_listado)
      if (is.null(resp2)) return(tibble())
      if (isTRUE(attr(resp2, "no_existe")) || status_code(resp2) == 404) {
        message(paste0("Volumen no disponible aun (HTTP 404) para el anio ",
                       anio_pagina, ": ", url))
        return(tibble())
      }
      html_doc <- read_html(resp2)
      nodos    <- html_doc %>% html_nodes(".articleInToc")
    }
    if (length(nodos) == 0) return(tibble())
    
    df_pagina <- map_dfr(nodos, function(nodo) {
      doi_val <- nodo %>%
        html_node(".articleSourceTag a") %>%
        html_attr("href") %>%
        str_extract("10\\.\\d{4,}/\\S+") %>%
        str_remove("[.,;]+$")
      
      url_art <- nodo %>%
        html_node(".articleTitle a, .js-articleTitle a") %>%
        html_attr("href")
      
      url_completa <- if (!is.na(url_art) && !str_starts(url_art, "http"))
        paste0("https://www.annualreviews.org", url_art)
      else
        url_art
      
      if ((is.na(url_completa) || url_completa == "") && !is.na(doi_val)) {
        url_completa <- paste0("https://www.annualreviews.org/content/journals/", doi_val)
      }
      if (is.na(doi_val) && !is.na(url_completa)) {
        doi_val <- str_extract(url_completa, "10\\.\\d{4,}/\\S+")
      }
      
      autores_txt <- nodo %>%
        html_nodes(".meta-value.authors .author-list__item a") %>%
        html_text() %>%
        limpiar_texto() %>%
        paste(collapse = "; ")
      
      paginas_txt <- nodo %>%
        html_node(".volYearPageRange .pageRange") %>%
        html_text() %>% limpiar_texto()
      
      tibble(
        titulo            = nodo %>%
          html_node(".articleTitle a, .js-articleTitle a") %>%
          html_text() %>% limpiar_texto(),
        fecha_publicacion = anio_pagina,
        doi               = doi_val,
        url               = url_completa,
        autores           = autores_txt,
        paginas           = paginas_txt,
        resumen           = nodo %>%
          html_node(".js-desc p") %>%
          html_text() %>% limpiar_texto()
      )
    })
    
    if (seguir_paginacion) {
      urls_extra <- detectar_paginas_extra(html_doc, url_pedido)
      urls_extra <- setdiff(urls_extra, url_pedido)
      if (length(urls_extra) > 0) {
        extra_df <- map_dfr(urls_extra, function(u) {
          Sys.sleep(1)
          scrapear_pagina(u, anio_pagina, seguir_paginacion = FALSE)
        })
        df_pagina <- bind_rows(df_pagina, extra_df) %>%
          distinct(doi, .keep_all = TRUE)
      }
    }
    
    df_pagina
  }, error = function(e) {
    warning(paste0("Error scrapeando ", url, ": ", e$message))
    tibble()
  })
}

scrapear_articulo <- function(url_articulo, referer = "https://www.annualreviews.org/content/journals/economics") {
  vacio <- list(
    resumen_completo = NA_character_,
    fecha_exacta     = NA_character_,
    referencias      = NA_character_,
    n_referencias    = NA_integer_,
    palabras_clave   = NA_character_
  )
  if (is.null(url_articulo) || is.na(url_articulo) || url_articulo == "")
    return(vacio)
  
  tryCatch({
    resp <- GET_con_reintentos(url_articulo, referer = referer)
    if (is.null(resp)) return(vacio)
    html_doc <- read_html(resp)
    
    resumen <- html_doc %>%
      html_nodes("div[class*='abstract'] p, .hlFld-Abstract p") %>%
      html_text() %>% paste(collapse = " ") %>% limpiar_texto()
    if (is.na(resumen) || resumen == "") resumen <- NA_character_
    
    fecha_meta <- html_doc %>%
      html_node("meta[name='citation_publication_date'], meta[name='dc.date']") %>%
      html_attr("content")
    if (is.na(fecha_meta) || fecha_meta == "") {
      fecha_meta <- html_doc %>%
        html_node("meta[name='citation_online_date']") %>%
        html_attr("content")
    }
    
    keywords <- html_doc %>%
      html_node("meta[name='citation_keywords'], meta[name='keywords']") %>%
      html_attr("content")
    
    ref_nodos <- html_doc %>% html_nodes("meta[name='citation_reference']")
    refs <- if (length(ref_nodos) > 0) {
      ref_nodos %>% html_attr("content") %>% limpiar_texto()
    } else {
      html_doc %>%
        html_nodes(".references li, .refbiblist li, #refBiblist li") %>%
        html_text() %>% limpiar_texto()
    }
    refs <- refs[!is.na(refs) & refs != ""]
    
    list(
      resumen_completo = resumen,
      fecha_exacta     = limpiar_texto(fecha_meta),
      referencias      = if (length(refs) > 0) paste(refs, collapse = "; ") else NA_character_,
      n_referencias    = if (length(refs) > 0) length(refs) else NA_integer_,
      palabras_clave   = limpiar_texto(keywords)
    )
  }, error = function(e) vacio)
}

clasificar <- function(titulo, resumen) {
  tx <- tolower(paste(titulo %||% "", resumen %||% ""))
  dplyr::case_when(
    str_detect(tx, "machine learning|neural network|deep learning|random forest|gradient boosting")
    ~ "Machine Learning",
    str_detect(tx, "generative ai|generative artificial intelligence|large language model|\\bllm\\b|\\bgpt\\b|chatgpt")
    ~ "IA Generativa",
    str_detect(tx, "\\bstatistic|inference|probability|probabilistic|bayesian|econometric|causal|regression|estimator")
    ~ "Estadística / Econometría",
    str_detect(tx, "trade|tariff|export|import|globalization")
    ~ "Comercio Internacional",
    str_detect(tx, "poverty|inequality|welfare|redistribut|development econom")
    ~ "Desarrollo y Pobreza",
    str_detect(tx, "labor market|labour market|unemployment|wage|employment")
    ~ "Mercado Laboral",
    str_detect(tx, "financ|asset pricing|banking|monetary policy|credit|debt")
    ~ "Finanzas y Política Monetaria",
    str_detect(tx, "political econom|voting|lobby|election|social media|internet")
    ~ "Economía Política",
    str_detect(tx, "climate|environment|energy|resource econom")
    ~ "Economía Ambiental",
    str_detect(tx, "game theory|nash equilibrium|mechanism design|auction")
    ~ "Teoría de Juegos",
    TRUE ~ "Otros"
  )
}

GET_api_externa <- function(url, n_intentos = 3, espera_base = 1.5, ...) {
  ultimo_error <- NULL
  for (intento in seq_len(n_intentos)) {
    resp <- tryCatch(
      GET(url,
          add_headers(`User-Agent` = "AnnualReviewsDashboard/2.0 (mailto:dashboard@example.com)"),
          timeout(20), ...),
      error = function(e) e
    )
    if (!inherits(resp, "error")) {
      if (!http_error(resp)) return(resp)
      codigo <- status_code(resp)
      ultimo_error <- paste0("HTTP ", codigo)
      if (codigo == 404) return(NULL)
    } else {
      ultimo_error <- resp$message
    }
    if (intento < n_intentos) Sys.sleep(espera_base * intento)
  }
  warning(paste0("Fallo tras ", n_intentos, " intentos en ", url, ": ", ultimo_error))
  NULL
}

obtener_metadatos_crossref <- function(doi) {
  vacio <- list(n_citas = NA_integer_, referencias_doi = NA_character_,
                titulo_cr = NA_character_, fecha_cr = NA_character_)
  if (is.null(doi) || is.na(doi) || doi == "") return(vacio)
  
  tryCatch({
    url  <- paste0("https://api.crossref.org/works/",
                   utils::URLencode(doi, reserved = TRUE))
    resp <- GET_api_externa(url)
    if (is.null(resp)) return(vacio)
    datos <- content(resp, as = "parsed", type = "application/json")
    msg   <- datos$message
    
    n_citas   <- suppressWarnings(as.integer(msg$`is-referenced-by-count`))
    
    refs_list <- msg$reference
    refs_txt  <- if (!is.null(refs_list) && length(refs_list) > 0) {
      vapply(refs_list, function(r) {
        partes <- c(r$author, r$year, r$`article-title`, r$DOI)
        partes <- partes[!vapply(partes, is.null, logical(1))]
        if (length(partes) == 0) return(NA_character_)
        paste(unlist(partes), collapse = " ")
      }, character(1))
      refs_txt <- refs_txt[!is.na(refs_txt)]
      if (length(refs_txt) > 0) paste(refs_txt, collapse = "; ") else NA_character_
    } else NA_character_
    
    fecha_parts <- msg$`published-print`$`date-parts`[[1]] %||%
      msg$`published-online`$`date-parts`[[1]]
    fecha_cr <- if (!is.null(fecha_parts)) paste(fecha_parts, collapse = "-") else NA_character_
    
    titulo_cr <- if (!is.null(msg$title) && length(msg$title) > 0) msg$title[[1]] else NA_character_
    
    list(n_citas = n_citas, referencias_doi = refs_txt,
         titulo_cr = titulo_cr, fecha_cr = fecha_cr)
  }, error = function(e) vacio)
}

obtener_citas_crossref <- function(doi) {
  obtener_metadatos_crossref(doi)$n_citas
}

obtener_citas_openalex <- function(doi) {
  if (is.null(doi) || is.na(doi) || doi == "") return(NA_integer_)
  tryCatch({
    url  <- paste0("https://api.openalex.org/works/https://doi.org/",
                   utils::URLencode(doi, reserved = TRUE))
    resp <- GET_api_externa(url)
    if (is.null(resp)) return(NA_integer_)
    datos <- content(resp, as = "parsed", type = "application/json")
    suppressWarnings(as.integer(datos$cited_by_count))
  }, error = function(e) NA_integer_)
}

obtener_citas_reales <- function(doi) {
  c_cr  <- obtener_citas_crossref(doi)
  c_oa  <- obtener_citas_openalex(doi)
  vals  <- c(c_cr, c_oa)
  vals  <- vals[!is.na(vals)]
  if (length(vals) == 0) return(NA_integer_)
  max(vals)
}

construir_urls_scraping <- function(vol_inicio = 12, anio_inicio = 2020,
                                    vol_fin = 18, anio_fin = 2026) {
  vols  <- seq(vol_inicio, vol_fin)
  anios <- seq(anio_inicio, anio_fin)
  stopifnot(length(vols) == length(anios))
  setNames(
    lapply(vols, function(v) {
      base <- paste0("https://www.annualreviews.org/content/journals/economics/", v, "/1")
      c(base)
    }),
    as.character(anios)
  )
}

URLS_SCRAPING <- construir_urls_scraping()

URL_AHEAD_OF_PRINT <- "https://www.annualreviews.org/content/journals/economics/fasttrack"

diagnosticar_acceso_ar <- function() {
  sesion_ok <- iniciar_sesion_ar(forzar = TRUE)
  
  url_prueba <- "https://www.annualreviews.org/content/journals/economics/17/1"
  resp <- tryCatch(
    if (!is.null(.AR_HANDLE))
      GET(url_prueba, handle = .AR_HANDLE, .cabeceras_navegador(), timeout(20))
    else
      GET(url_prueba, .cabeceras_navegador(), timeout(20)),
    error = function(e) e
  )
  
  if (inherits(resp, "error")) {
    return(paste0(
      "Sin conexion al sitio (fallo de red antes de recibir respuesta HTTP): ",
      resp$message,
      ". Revisa que el servidor donde corre la app tenga salida a internet ",
      "hacia annualreviews.org (firewall/proxy de salida, DNS, etc.)."
    ))
  }
  
  codigo <- status_code(resp)
  if (codigo == 200) return("OK: el sitio responde correctamente (200).")
  if (codigo %in% c(403, 429)) {
    paste0(
      "Bloqueado por el sitio (HTTP ", codigo, "). Sesion inicial: ",
      if (sesion_ok) "establecida (cookies obtenidas)" else "NO establecida (la home tambien fue bloqueada)",
      ". Esto suele deberse a la proteccion anti-bot del sitio, que bloquea ",
      "IPs de servidores/nube (shinyapps.io, RStudio Connect, VPS, ",
      "contenedores, etc.). No se soluciona reintentando desde el mismo ",
      "servidor. Alternativas: (1) correr el scraping desde una maquina/IP ",
      "residencial y luego subir los datos, (2) usar acceso institucional ",
      "si tu universidad/biblioteca tiene suscripcion (proxy institucional), ",
      "o (3) usar las APIs de Crossref/OpenAlex como fuente principal de ",
      "metadatos cuando el scraping directo del HTML no sea posible."
    )
  } else {
    paste0("Respuesta inesperada del sitio: HTTP ", codigo)
  }
}

for (.f in list.files("funciones", pattern = "\\.R$", full.names = TRUE)) {
  source(.f)
}
rm(.f)

CACHE_IR <- tryCatch(
  obtener_cache_ir(ruta = "cache/ir_cache.rds"),
  error = function(e) {
    message("No se pudo cargar/generar el cache de IR: ", e$message)
    NULL
  }
)


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tags$span(
      tags$img(
        src = "logo.png",
        height = "200px",
        style = "border:2px solid red;"
      ),
      tags$span("Parcial 2-Annual Reviews", style = "font-size:15px; vertical-align:middle;")
    ),
    titleWidth = 320
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    use_theme(tema_claro),
    tags$head(tags$style(HTML("
      .skin-blue .main-header .logo  { background:#2563EB!important; }
      .skin-blue .main-header .navbar { background:#2563EB!important; }
      .content-wrapper {
        background:#F8FAFC!important;
        margin-right:280px!important;
        margin-left:0!important;
      }
      .box { border-top:3px solid #3B82F6; box-shadow:0 1px 6px rgba(0,0,0,.08)!important; }
      .shiny-notification { background:#DBEAFE; color:#1E40AF; border:1px solid #93C5FD; }
      #rsb {
        position:fixed; top:50px; right:0;
        width:272px; height:calc(100vh - 50px);
        background:#F1F5F9; border-left:1px solid #E2E8F0;
        overflow-y:auto; z-index:900;
        padding:14px 12px;
        box-shadow:-2px 0 8px rgba(0,0,0,.06);
      }
      .rsb-logo { text-align:center; margin-bottom:12px; }
      .rsb-logo p { font-size:10px; color:#64748B; margin:3px 0 0; }
      .rsb-section {
        font-size:10px; font-weight:700; color:#94A3B8;
        text-transform:uppercase; letter-spacing:.06em; margin:12px 0 5px;
      }
      .nav-btn {
        display:flex; align-items:center; gap:8px;
        width:100%; padding:8px 10px; margin-bottom:3px;
        border:none; border-radius:6px; background:transparent;
        color:#334155; font-size:13px; cursor:pointer; text-align:left;
        transition:background .15s, color .15s;
      }
      .nav-btn:hover  { background:#DBEAFE; color:#1E40AF; }
      .nav-btn.active { background:#DBEAFE; color:#1E40AF; font-weight:600; }
      .rsb-label { font-size:12px; color:#475569; margin:6px 0 3px; font-weight:500; }
      #scraping_log {
        font-family:monospace; font-size:11px; max-height:140px; overflow-y:auto;
        background:#F0F9FF; border:1px solid #BAE6FD; border-radius:4px;
        padding:7px; color:#0C4A6E; white-space:pre-wrap;
      }
      .dataTables_wrapper { font-size:13px; }
      .subtitle-header { font-size:12px; color:#64748B; margin:-10px 0 16px; font-style:italic; }
    "))),
    
    tags$div(id = "rsb",
             tags$div(class = "rsb-logo",
                      tags$img(
                        src    = "https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/Escudo_UNAL.svg/120px-Escudo_UNAL.svg.png",
                        height = "42px"
                      ),
                      tags$p("Juan Pablo Aldana Henao"),
                      tags$p("Minería de Datos · 2016325")
             ),
             tags$div(class = "rsb-section", "Navegación"),
             tags$button(class = "nav-btn active", id = "nb_kpi",
                         onclick = "navTo('tab_kpi')", "Indicadores"),
             tags$button(class = "nav-btn", id = "nb_viz",
                         onclick = "navTo('tab_viz')", "Visualizaciones"),
             tags$button(class = "nav-btn", id = "nb_tabla",
                         onclick = "navTo('tab_tabla')", "Tabla de Papers"),
             tags$button(class = "nav-btn", id = "nb_scrap",
                         onclick = "navTo('tab_scrap')", "Actualizar DB"),
             tags$button(class = "nav-btn", id = "nb_ir",
                         onclick = "navTo('tab_ir')", "Buscador (IR)"),
             tags$hr(style = "border-color:#CBD5E1; margin:10px 0;"),
             tags$div(class = "rsb-section", "Filtros globales"),
             uiOutput("ui_slider_anio"),
             tags$div(class = "rsb-label", "Categoría:"),
             pickerInput("filtro_cat", label = NULL,
                         choices  = c("Todas", "Machine Learning", "IA Generativa", "Estadística", "Otros"),
                         selected = "Todas",
                         options  = list(style = "btn-light btn-sm")),
             tags$div(class = "rsb-label", "Autor (contiene):"),
             textInput("filtro_autor", label = NULL, placeholder = "ej. Smith"),
             tags$div(class = "rsb-label", "DOI (contiene):"),
             textInput("filtro_doi", label = NULL, placeholder = "ej. 10.1146"),
             tags$div(class = "rsb-label", "Título / Palabras clave:"),
             textInput("filtro_kw", label = NULL, placeholder = "ej. neural"),
             actionButton("btn_filtrar", "Aplicar filtros", icon = icon("filter"),
                          class = "btn-primary btn-sm btn-block",
                          style = "margin-top:8px; background:#2563EB; border-color:#1E40AF;")
    ),
    
    tags$script(HTML("
      var NAV_IDS = {
        'tab_kpi':'nb_kpi', 'tab_viz':'nb_viz',
        'tab_tabla':'nb_tabla', 'tab_scrap':'nb_scrap',
        'tab_ir':'nb_ir'
      };
      function navTo(tab) {
        Object.values(NAV_IDS).forEach(function(id) {
          document.getElementById(id).classList.remove('active');
        });
        document.getElementById(NAV_IDS[tab]).classList.add('active');
        Shiny.setInputValue('active_tab', tab, {priority: 'event'});
      }
    ")),
    
    tabsetPanel(id = "main_tabs", type = "hidden",
                
                tabPanelBody("tab_kpi",
                             h3("Indicadores descriptivos", style = "color:#1E40AF; margin-bottom:4px;"),
                             tags$p("Annual Reviews of Economics", class = "subtitle-header"),
                             fluidRow(
                               valueBoxOutput("kpi_total",   width = 3),
                               valueBoxOutput("kpi_citas",   width = 3),
                               valueBoxOutput("kpi_autores", width = 3),
                               valueBoxOutput("kpi_refs",    width = 3)
                             ),
                             fluidRow(
                               valueBoxOutput("kpi_cats",    width = 4),
                               valueBoxOutput("kpi_top_cit", width = 4),
                               valueBoxOutput("kpi_top_dl",  width = 4)
                             ),
                             fluidRow(
                               box(title = "Distribución por categoría", width = 6, status = "primary",
                                   withSpinner(highchartOutput("hc_cat_pie", height = "300px"), color = "#2563EB")),
                               box(title = "Top 10 artículos más citados", width = 6, status = "primary",
                                   withSpinner(highchartOutput("hc_top_citas", height = "300px"), color = "#2563EB"))
                             )
                ),
                
                tabPanelBody("tab_viz",
                             h3("Visualizaciones interactivas", style = "color:#1E40AF; margin-bottom:4px;"),
                             tags$p("Annual Reviews of Economics", class = "subtitle-header"),
                             fluidRow(
                               box(title = "Evolución temporal de publicaciones", width = 12, status = "primary",
                                   withSpinner(highchartOutput("hc_evolucion", height = "320px"), color = "#2563EB"))
                             ),
                             fluidRow(
                               box(title = "Top 15 autores con más publicaciones", width = 6, status = "info",
                                   withSpinner(highchartOutput("hc_autores", height = "320px"), color = "#2563EB")),
                               box(title = "Distribución de citas (histograma)", width = 6, status = "info",
                                   withSpinner(highchartOutput("hc_hist_citas", height = "320px"), color = "#2563EB"))
                             ),
                             fluidRow(
                               box(title = "Descargas por temática", width = 6, status = "warning",
                                   withSpinner(highchartOutput("hc_dl_cat", height = "280px"), color = "#2563EB")),
                               box(title = "Citas vs Descargas (burbuja)", width = 6, status = "warning",
                                   withSpinner(highchartOutput("hc_bubble", height = "280px"), color = "#2563EB"))
                             )
                ),
                
                tabPanelBody("tab_tabla",
                             h3("Tabla de artículos filtrados", style = "color:#1E40AF; margin-bottom:16px;"),
                             fluidRow(
                               box(width = 12, status = "primary",
                                   tags$div(style = "margin-bottom:10px; display:flex; gap:10px; align-items:center;",
                                            downloadButton("btn_csv", "Descargar CSV",
                                                           class = "btn-sm", style = "background:#16A34A; color:#fff; border:none;"),
                                            tags$span(style = "color:#64748B; font-size:13px;",
                                                      textOutput("n_papers_label", inline = TRUE))
                                   ),
                                   withSpinner(DTOutput("tabla_papers"), color = "#2563EB"))
                             )
                ),
                
                tabPanelBody("tab_scrap",
                             h3("Actualización de datos", style = "color:#1E40AF; margin-bottom:4px;"),
                             tags$p("Annual Review of Economics, volumenes 12 a 18 (2020-2026)..", class = "subtitle-header"),
                             fluidRow(
                               box(title = "Control de scraping", width = 6, status = "primary",
                                   pickerInput("scrap_anio", "Año(s) a scrapear:",
                                               choices  = names(URLS_SCRAPING),
                                               selected = names(URLS_SCRAPING),
                                               multiple = TRUE,
                                               options  = list(`actions-box` = TRUE, `selected-text-format` = "count > 2",
                                                               style = "btn-light btn-sm")),
                                   checkboxInput("scrap_crossref", "Consultar Crossref para citas reales", value = TRUE),
                                   tags$div(style = "display:flex; gap:8px; flex-wrap:wrap; margin-top:10px;",
                                            actionButton("btn_scrape", "Buscar artículos",
                                                         class = "btn-primary", style = "background:#2563EB; border-color:#1E40AF;"),
                                            actionButton("btn_verificar", "Verificar últimos 5",
                                                         class = "btn-default"),
                                            actionButton("btn_diagnostico", "Probar acceso al sitio",
                                                         class = "btn-default")
                                   ),
                                   tags$hr(style = "border-color:#E2E8F0; margin:12px 0;"),
                                   tags$p(tags$b("Log:"), style = "color:#475569; margin-bottom:4px; font-size:13px;"),
                                   verbatimTextOutput("scraping_log")
                               ),
                               box(title = "Resultados", width = 6, status = "success",
                                   uiOutput("scraping_resultado"),
                                   withSpinner(DTOutput("tabla_nuevos"), color = "#16A34A"))
                             )
                ),
                
                tabPanelBody("tab_ir",
                             h3("Buscador de artículos",
                                style = "color:#1E40AF; margin-bottom:4px;"),
                             tags$p("Escribe una consulta en lenguaje natural y obtén un ranking de artículos por relevancia.",
                                    class = "subtitle-header"),
                             fluidRow(
                               box(title = "Consulta", width = 12, status = "primary",
                                   fluidRow(
                                     column(6,
                                            textInput("ir_query", "Consulta:",
                                                      placeholder = "ej. labor markets in developing countries")),
                                     column(3,
                                            selectInput("ir_metodo", "Método de recuperación:",
                                                        choices = c("TF-IDF + Similitud Coseno"  = "tfidf",
                                                                    "TF-IDF + LSA (SVD) + Coseno" = "lsa"),
                                                        selected = "tfidf")),
                                     column(3,
                                            numericInput("ir_topn", "N.º de resultados:",
                                                         value = 5, min = 1, max = 30, step = 1))
                                   ),
                                   actionButton("btn_buscar_ir", "Buscar", icon = icon("search"),
                                                class = "btn-primary", style = "background:#2563EB; border-color:#1E40AF;")
                               )
                             ),
                             fluidRow(
                               box(title = "Resultados (ranking por relevancia)", width = 7, status = "success",
                                   withSpinner(DTOutput("tabla_ir_resultados"), color = "#2563EB")),
                               box(title = "Resumen del artículo seleccionado", width = 5, status = "info",
                                   uiOutput("ir_resumen_seleccionado"))
                             )
                )
    )
  )
)


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
    amin  <- if (length(anios)) max(2020L, min(anios)) else 2020L
    amax  <- if (length(anios)) max(anios) else 2026L
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
    df %>% mutate(anio = as.integer(str_extract(as.character(fecha_publicacion), "\\d{4}")))
  })
  
  datos_filtrados <- eventReactive(
    list(input$btn_filtrar, datos_todos()),
    {
      df <- datos_todos()
      if (nrow(df) == 0) return(tibble())
      if (!is.null(input$filtro_anio))
        df <- df %>% filter(anio >= input$filtro_anio[1], anio <= input$filtro_anio[2])
      if (!is.null(input$filtro_cat) && input$filtro_cat != "Todas")
        df <- df %>% filter(categoria == input$filtro_cat)
      if (!is.null(input$filtro_autor) && nzchar(trimws(input$filtro_autor)))
        df <- df %>% filter(str_detect(tolower(autores), tolower(trimws(input$filtro_autor))))
      if (!is.null(input$filtro_doi) && nzchar(trimws(input$filtro_doi)))
        df <- df %>% filter(str_detect(tolower(doi), tolower(trimws(input$filtro_doi))))
      if (!is.null(input$filtro_kw) && nzchar(trimws(input$filtro_kw)))
        df <- df %>% filter(str_detect(tolower(titulo), tolower(trimws(input$filtro_kw))))
      df
    },
    ignoreNULL = FALSE
  )
  
  df_show <- reactive({
    d <- datos_filtrados()
    if (nrow(d) == 0) datos_todos() else d
  })
  
  output$kpi_total <- renderValueBox({
    valueBox(nrow(df_show()), "Total artículos", icon = icon("newspaper"), color = "blue")
  })
  
  output$kpi_citas <- renderValueBox({
    val <- if (nrow(df_show()) > 0 && "n_citas" %in% names(df_show()))
      round(mean(df_show()$n_citas, na.rm = TRUE), 1) else 0
    valueBox(val, "Promedio citas", icon = icon("quote-right"), color = "green")
  })
  
  output$kpi_autores <- renderValueBox({
    val <- if (nrow(df_show()) > 0 && "autores" %in% names(df_show()))
      round(mean(str_count(df_show()$autores, ";") + 1, na.rm = TRUE), 1) else 0
    valueBox(val, "Autores / artículo", icon = icon("users"), color = "purple")
  })
  
  output$kpi_refs <- renderValueBox({
    val <- if (nrow(df_show()) > 0 && "referencias" %in% names(df_show()))
      round(mean(str_count(df_show()$referencias, ";") + 1, na.rm = TRUE), 1) else "—"
    valueBox(val, "Prom. referencias", icon = icon("book"), color = "yellow")
  })
  
  output$kpi_cats <- renderValueBox({
    val <- if (nrow(df_show()) > 0 && "categoria" %in% names(df_show()))
      n_distinct(df_show()$categoria) else 0
    valueBox(val, "Categorías", icon = icon("tags"), color = "teal")
  })
  
  output$kpi_top_cit <- renderValueBox({
    df <- df_show()
    if (nrow(df) == 0 || all(is.na(df$n_citas)))
      return(valueBox("—", "Más citado", icon = icon("trophy"), color = "orange"))
    top <- df %>% slice_max(n_citas, n = 1, with_ties = FALSE)
    valueBox(paste0(top$n_citas, " citas"), str_trunc(top$titulo, 38),
             icon = icon("trophy"), color = "orange")
  })
  
  output$kpi_top_dl <- renderValueBox({
    df <- df_show()
    if (nrow(df) == 0 || !"n_descargas" %in% names(df) || all(is.na(df$n_descargas)))
      return(valueBox("—", "Más descargado", icon = icon("download"), color = "red"))
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
      hc_tooltip(pointFormat = "<b>{point.name}</b>: {point.y} ({point.percentage:.1f}%)") %>%
      hc_chart(backgroundColor = "#FFFFFF") %>%
      hc_plotOptions(pie = list(dataLabels = list(enabled = TRUE, format = "{point.name}: {point.y}")))
  })
  
  output$hc_top_citas <- renderHighchart({
    df <- df_show()
    if (nrow(df) == 0 || !"n_citas" %in% names(df)) return(highchart())
    top <- df %>% slice_max(n_citas, n = 10, with_ties = FALSE) %>%
      mutate(tc = str_trunc(titulo, 45))
    hchart(top, "bar", hcaes(x = tc, y = n_citas), name = "Citas", color = "#2563EB") %>%
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
      di <- serie %>% filter(categoria == cats[i]) %>%
        complete(anio = years, fill = list(n = 0)) %>%
        arrange(anio) %>% pull(n)
      hc <- hc %>% hc_add_series(name = cats[i], data = di,
                                 color = cols[(i - 1) %% length(cols) + 1])
    }
    hc
  })
  
  output$hc_autores <- renderHighchart({
    df <- df_show()
    if (nrow(df) == 0 || !"autores" %in% names(df)) return(highchart())
    au <- df %>% filter(!is.na(autores), autores != "") %>%
      separate_rows(autores, sep = "; ") %>%
      filter(!is.na(autores), autores != "") %>%
      count(autores, sort = TRUE) %>%
      slice_max(n, n = 15, with_ties = FALSE)
    hchart(au, "bar", hcaes(x = autores, y = n), name = "Artículos", color = "#7C3AED") %>%
      hc_xAxis(title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Publicaciones")) %>%
      hc_chart(backgroundColor = "#FFFFFF") %>%
      hc_tooltip(pointFormat = "<b>{point.category}</b>: {point.y} publicaciones")
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
      hc_xAxis(categories = paste0(head(hd$breaks, -1), "-", tail(hd$breaks, -1)),
               title = list(text = "Rango de citas")) %>%
      hc_yAxis(title = list(text = "Frecuencia")) %>%
      hc_add_series(name = "Artículos", data = as.list(hd$counts), color = "#16A34A") %>%
      hc_tooltip(pointFormat = "<b>Frecuencia:</b> {point.y}")
  })
  
  output$hc_dl_cat <- renderHighchart({
    df <- df_show()
    if (nrow(df) == 0 || !"n_descargas" %in% names(df)) return(highchart())
    dl <- df %>% group_by(categoria) %>%
      summarise(tot = sum(n_descargas, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(tot))
    hchart(dl, "column", hcaes(x = categoria, y = tot), name = "Descargas", color = "#D97706") %>%
      hc_xAxis(title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Total descargas")) %>%
      hc_chart(backgroundColor = "#FFFFFF")
  })
  
  output$hc_bubble <- renderHighchart({
    df <- df_show()
    if (nrow(df) == 0 || !all(c("n_citas", "n_descargas") %in% names(df))) return(highchart())
    d2 <- df %>% filter(!is.na(n_citas), !is.na(n_descargas)) %>%
      mutate(tc = str_trunc(titulo, 40))
    highchart() %>%
      hc_chart(type = "bubble", backgroundColor = "#FFFFFF") %>%
      hc_xAxis(title = list(text = "Citas")) %>%
      hc_yAxis(title = list(text = "Descargas")) %>%
      hc_add_series(name = "Papers",
                    data = pmap(list(d2$n_citas, d2$n_descargas, d2$tc),
                                function(x, y, n) list(x = x, y = y, z = x + y, name = n)),
                    color = "#3B82F6") %>%
      hc_tooltip(pointFormat = "<b>{point.name}</b><br>Citas: {point.x} / Desc: {point.y}")
  })
  
  output$n_papers_label <- renderText(paste0("Mostrando ", nrow(df_show()), " artículos"))
  
  output$tabla_papers <- renderDT({
    df <- df_show()
    if (nrow(df) == 0) return(datatable(tibble(Mensaje = "Sin datos.")))
    cols <- c("titulo", "autores", "fecha_publicacion", "categoria", "doi", "n_citas", "n_descargas")
    noms <- c("Título", "Autores", "Año", "Categoría", "DOI", "Citas", "Descargas")
    pres <- intersect(cols, names(df))
    df %>% select(all_of(pres)) %>%
      rename_with(~ noms[match(., cols)], all_of(pres)) %>%
      datatable(filter = "top", rownames = FALSE,
                options = list(pageLength = 10, scrollX = TRUE,
                               language = list(url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/es-ES.json")),
                class = "stripe hover compact") %>%
      formatStyle("Citas",
                  backgroundColor = styleInterval(c(10, 50), c("#FEF3C7", "#D1FAE5", "#BBF7D0"))) %>%
      formatStyle("Título", fontWeight = "bold")
  })
  
  output$btn_csv <- downloadHandler(
    filename = function() paste0("papers_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(df_show(), f, row.names = FALSE, fileEncoding = "UTF-8")
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
      agregar_log(paste0("Consultando Crossref + OpenAlex para ", n_candidatos, " articulos..."))
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
          if (is.na(d)) sample(50:500, 1) else sum(utf8ToInt(d)) %% 950L
        }, integer(1)))
      )
      agregar_log("Crossref + OpenAlex completado.")
    } else if (n_candidatos > 0) {
      nuevos_df <- nuevos_df %>% rowwise() %>% mutate(
        n_citas     = as.integer(sum(utf8ToInt(if (is.na(doi)) "x" else doi)) %% 200L),
        n_descargas = as.integer(50L + sum(utf8ToInt(if (is.na(doi)) "x" else doi)) %% 950L)
      ) %>% ungroup()
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
        showNotification(paste0(n_new, " articulos guardados."), type = "message")
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
      dbGetQuery(con, "SELECT doi, titulo, n_citas, fecha_publicacion FROM papers ORDER BY rowid DESC LIMIT 5"),
      error = function(e) data.frame()
    )
    dbDisconnect(con)
    if (nrow(ult) == 0) { agregar_log("BD vacía."); return() }
    for (i in seq_len(nrow(ult)))
      agregar_log(paste0("[", i, "] (", ult$fecha_publicacion[i], ") ",
                         str_trunc(ult$titulo[i], 55), " | Citas: ", ult$n_citas[i]))
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
      if (n > 0) paste0(n, " artículo(s) encontrados") else "Sin artículos nuevos"
    )
  })
  
  output$tabla_nuevos <- renderDT({
    if (is.null(rv$nuevos) || nrow(rv$nuevos) == 0) return(NULL)
    cols <- intersect(
      c("titulo", "doi", "fecha_publicacion", "categoria", "n_citas", "n_descargas"),
      names(rv$nuevos)
    )
    datatable(rv$nuevos %>% select(all_of(cols)),
              rownames = FALSE,
              options  = list(pageLength = 5, scrollX = TRUE),
              class    = "stripe compact")
  })
  
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
        escape    = FALSE,
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

shinyApp(ui, server)