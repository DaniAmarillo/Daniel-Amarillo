library(shiny)
library(shinydashboard)
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(highcharter)
library(DT)
library(httr)
library(rvest)
library(fresh)
library(shinyWidgets)
library(shinycssloaders)
library(jsonlite)
library(tibble)
library(tidyr)
library(Matrix)
library(tidytext)
library(SnowballC)
library(stringi)
library(irlba)

tema_claro <- create_theme(
  adminlte_color(
    light_blue = "#2563EB", navy = "#1E40AF", blue = "#3B82F6",
    green      = "#16A34A", red  = "#DC2626", yellow = "#D97706",
    fuchsia    = "#7C3AED", black = "#111827"
  ),
  adminlte_sidebar(
    dark_bg           = "#F1F5F9", dark_hover_bg    = "#DBEAFE",
    dark_color        = "#1E293B", dark_hover_color = "#1E40AF",
    dark_submenu_bg   = "#E2E8F0", dark_submenu_color = "#334155"
  ),
  adminlte_global(
    content_bg = "#F8FAFC", box_bg = "#FFFFFF", info_box_bg = "#FFFFFF"
  )
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
      html_nodes(".articleabstract .description p, .articleabstract p") %>%
      html_text() %>% paste(collapse = " ") %>% limpiar_texto()
    if (is.na(resumen) || resumen == "") {
      resumen <- html_doc %>%
        html_node("meta[name='citation_abstract']") %>%
        html_attr("content") %>% limpiar_texto()
    }
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
    if (is.na(keywords) || keywords == "") {
      kw_nodos <- html_doc %>%
        html_nodes(".bottom-side-nav a[href*='pub_keyword']")
      kw_txt <- kw_nodos %>% html_text() %>% limpiar_texto()
      kw_txt <- kw_txt[!is.na(kw_txt) & kw_txt != ""]
      keywords <- if (length(kw_txt) > 0) paste(kw_txt, collapse = "; ") else NA_character_
    }

    ref_nodos <- html_doc %>%
      html_nodes("meta[name='citation_reference']")
    refs <- if (length(ref_nodos) > 0) {
      ref_nodos %>% html_attr("content") %>% limpiar_texto()
    } else {
      html_doc %>%
        html_nodes("#referenceContainer li.refbody .ref-content") %>%
        html_text() %>% limpiar_texto()
    }

    if (length(refs) == 0 || all(is.na(refs) | refs == "")) {
      primer_ol <- html_doc %>% html_node("#articlereference")
      refs <- if (!is.null(primer_ol) && !is.na(primer_ol)) {
        primer_ol %>% html_nodes("li.refbody .ref-content") %>%
          html_text() %>% limpiar_texto()
      } else character(0)
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