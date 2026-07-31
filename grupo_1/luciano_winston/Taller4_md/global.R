library(shiny)
library(bslib)
library(highcharter)
library(RSQLite)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(jsonlite)
library(DT)
library(shinyjs)
library(httr2)
library(rvest)
library(xml2)
library(tibble)

# ------------------------------------------------------------------------------
# CONFIGURACION
# ------------------------------------------------------------------------------
DB_PATH <- "Springer_Visual_Miner.sqlite"

AIR_ISSN       <- "1573-7462"   
AIR_JOURNAL_ID <- 10462

CONTACTO_MAIL <- Sys.getenv("SVM_MAIL", unset = "")

UA_CLIENTE <- local({
  base <- "SpringerVisualMiner/2.0 (proyecto academico UNAL"
  if (nzchar(CONTACTO_MAIL)) base <- paste0(base, "; mailto:", CONTACTO_MAIL)
  paste0(base, ")")
})
UA_CROSSREF <- UA_CLIENTE
UA_WEB      <- UA_CLIENTE

# Topes de seguridad
MAX_NUEVOS_POR_SYNC <- 60     
PAUSA_MIN <- 0.8              
PAUSA_MAX <- 1.6

# ==============================================================================
# HELPERS DE RED
# ==============================================================================

.get_json <- function(url, ua = UA_CROSSREF, timeout = 30) {
  resp <- request(url) %>%
    req_user_agent(ua) %>%
    req_timeout(timeout) %>%
    req_retry(max_tries = 3, backoff = function(i) 2 * i) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()

  if (resp_status(resp) != 200) return(NULL)
  tryCatch(resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL)
}

.get_html <- function(url, ua = UA_WEB, timeout = 25) {
  resp <- request(url) %>%
    req_user_agent(ua) %>%
    req_headers(`Accept-Language` = "en-US,en;q=0.9") %>%
    req_timeout(timeout) %>%
    req_retry(max_tries = 2, backoff = function(i) 2 * i) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()

  if (resp_status(resp) != 200) return(NULL)
  tryCatch(resp_body_html(resp), error = function(e) NULL)
}

.pausa <- function() Sys.sleep(stats::runif(1, PAUSA_MIN, PAUSA_MAX))

# ==============================================================================
# HELPERS DE TEXTO / DOI
# ==============================================================================

normalizar_doi <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  d <- trimws(as.character(x[1]))
  if (is.na(d) || d == "") return(NA_character_)
  d <- str_remove(d, regex("^\\s*(https?://)?(dx\\.)?doi\\.org/", ignore_case = TRUE))
  d <- str_remove(d, regex("^\\s*doi:\\s*", ignore_case = TRUE))
  d <- str_remove(d, regex("^\\s*https?://link\\.springer\\.com/article/", ignore_case = TRUE))
  d <- str_remove(d, "[\\s.,;]+$")
  d <- tolower(trimws(d))
  if (!str_detect(d, "^10\\.\\d{4,9}/")) return(NA_character_)
  d
}

es_doi_valido <- function(x) !is.na(normalizar_doi(x))

.limpiar_jats <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(NA_character_)
  txt <- as.character(x[1])
  txt <- str_remove_all(txt, "<[^>]+>")
  txt <- str_replace_all(txt, "&amp;", "&")
  txt <- str_replace_all(txt, "&lt;", "<")
  txt <- str_replace_all(txt, "&gt;", ">")
  txt <- str_squish(txt)
  txt <- str_remove(txt, regex("^abstract\\s*", ignore_case = TRUE))
  if (nchar(txt) < 3) return(NA_character_)
  txt
}

.fecha_crossref <- function(nodo) {
  if (is.null(nodo) || is.null(nodo$`date-parts`)) return(NA_character_)
  p <- nodo$`date-parts`[[1]]
  if (is.null(p) || length(p) == 0) return(NA_character_)
  a <- as.integer(p[[1]])
  m <- if (length(p) >= 2) as.integer(p[[2]]) else 1L
  d <- if (length(p) >= 3) as.integer(p[[3]]) else 1L
  sprintf("%04d-%02d-%02d", a, m, d)
}

.meta <- function(html, nombre) {
  if (is.null(html)) return(character(0))
  nodos <- html_elements(
    html,
    xpath = sprintf("//meta[@name='%s'] | //meta[@property='%s']", nombre, nombre)
  )
  if (length(nodos) == 0) return(character(0))
  v <- html_attr(nodos, "content")
  v[!is.na(v) & nzchar(trimws(v))]
}

.normalizar_autor <- function(x) {
  x <- str_squish(as.character(x))
  if (length(x) == 0) return(character(0))
  una_coma <- str_count(x, ",") == 1 & !is.na(x)
  if (any(una_coma)) {
    partes <- str_split_fixed(x[una_coma], ",", 2)
    x[una_coma] <- str_squish(paste(partes[, 2], partes[, 1]))
  }
  x
}

.si_vacio <- function(x, defecto) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) defecto else x[1]
}

.primero <- function(x, defecto = "NA") {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(trimws(x[1]))) return(defecto)
  str_squish(as.character(x[1]))
}

safe_to_json <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  as.character(toJSON(x, auto_unbox = TRUE))
}

# ==============================================================================
# FASE 1 - DESCUBRIMIENTO VIA CROSSREF
# ==============================================================================

.crossref_a_tibble <- function(item) {
  doi <- normalizar_doi(item$DOI)
  if (is.na(doi)) return(NULL)

  autores <- character(0)
  if (!is.null(item$author) && length(item$author) > 0) {
    autores <- map_chr(item$author, function(a) {
      nom <- paste(c(a$given, a$family), collapse = " ")
      if (!nzchar(trimws(nom)) && !is.null(a$name)) nom <- a$name
      str_squish(nom)
    })
    autores <- autores[nzchar(autores)]
  }

  fecha <- .fecha_crossref(item$`published-online`)
  if (is.na(fecha)) fecha <- .fecha_crossref(item$published)
  if (is.na(fecha)) fecha <- .fecha_crossref(item$issued)

  citas <- if (!is.null(item$`is-referenced-by-count`)) as.integer(item$`is-referenced-by-count`) else 0L

  tibble(
    titulo                   = .primero(unlist(item$title)),
    fecha_publicacion        = ifelse(is.na(fecha), "NA", fecha),
    doi                      = doi,
    url                      = paste0("https://link.springer.com/article/", doi),
    autores_json             = safe_to_json(autores),
    abstract                 = .si_vacio(.limpiar_jats(item$abstract), "NA"),
    referencias_json         = NA_character_,
    metricas_visualizaciones = "NA",
    metricas_citas           = paste(citas, "Citations")
  )
}


#' @param desde  
#' @param limite 
descubrir_articulos <- function(desde = "2025-01-01", limite = NULL,
                                filtro_fecha = "from-pub-date",
                                filas = 200, tope_offset = 10000) {

  campos <- paste(
    "DOI", "title", "author", "abstract", "issued", "published",
    "published-online", "is-referenced-by-count", "URL", "type",
    sep = ","
  )

  base <- paste0(
    "https://api.crossref.org/journals/", AIR_ISSN, "/works",
    "?filter=type:journal-article,", filtro_fecha, ":", desde,
    "&select=", utils::URLencode(campos, reserved = TRUE),
    "&sort=published&order=desc"
  )


  js0 <- .get_json(paste0(base, "&rows=0"))
  total_api <- NA_integer_
  if (!is.null(js0) && !is.null(js0$message) && !is.null(js0$message$`total-results`)) {
    total_api <- as.integer(js0$message$`total-results`)
  }

  objetivo <- if (!is.null(limite)) {
    if (is.na(total_api)) limite else min(limite, total_api)
  } else {
    if (is.na(total_api)) 1000L else total_api
  }
  objetivo <- min(objetivo, tope_offset)

  salida <- list()
  offset <- 0L

  while (offset < objetivo) {
    n_pedir <- min(filas, objetivo - offset)

    js <- .get_json(paste0(base, "&rows=", n_pedir, "&offset=", offset))
    if (is.null(js) || is.null(js$message) || is.null(js$message$items)) break

    items <- js$message$items


    if (length(items) == 0) break

    salida <- c(salida, map(items, .crossref_a_tibble))


    offset <- offset + length(items)
  }

  salida <- compact(salida)

  if (length(salida) == 0) {
    res <- tibble()
    attr(res, "total_api") <- total_api
    return(res)
  }

  res <- bind_rows(salida) %>%
    distinct(doi, .keep_all = TRUE) %>%
    arrange(fecha_publicacion == "NA", desc(fecha_publicacion))

  if (!is.null(limite)) res <- head(res, limite)

  attr(res, "total_api") <- total_api
  res
}

# ==============================================================================
# FASE 2 - ENRIQUECIMIENTO DESDE LA PAGINA DE SPRINGER
# ==============================================================================

.extraer_contador <- function(html, etiqueta) {
  # 1) selector data-test 
  sel  <- sprintf("[data-test='%s-count']", etiqueta)
  nodo <- html_elements(html, sel)
  if (length(nodo) > 0) {
    n <- str_extract(html_text2(nodo[1]), "[0-9][0-9,\\.]*")
    if (!is.na(n)) return(str_remove_all(n, "[,\\.]"))
  }
  # 2) barra de metricas clasica
  barra <- html_elements(html, "p.c-article-metrics-bar__count, .c-article-metrics-bar__count")
  if (length(barra) > 0) {
    txt   <- html_text2(barra)
    clave <- if (etiqueta == "access") "Access" else "Citation"
    hit   <- txt[str_detect(txt, regex(clave, ignore_case = TRUE))]
    if (length(hit) > 0) {
      n <- str_extract(hit[1], "[0-9][0-9,\\.]*")
      if (!is.na(n)) return(str_remove_all(n, "[,\\.]"))
    }
  }
  # 3) Regex sobre el texto completo
  pleno <- html_text2(html)
  patron <- if (etiqueta == "access") "([0-9][0-9,\\.]*)\\s*Accesses" else "([0-9][0-9,\\.]*)\\s*Citations"
  m <- str_match(pleno, patron)
  if (!is.na(m[1, 2])) return(str_remove_all(m[1, 2], "[,\\.]"))

  NA_character_
}

.extraer_referencias <- function(html) {
  refs <- html_elements(html, ".c-article-references__text")
  if (length(refs) == 0) refs <- html_elements(html, "li.c-article-references__item p")
  if (length(refs) > 0) {
    v <- str_squish(html_text2(refs))
    v <- v[nzchar(v)]
    if (length(v) > 0) return(v)
  }

  v <- str_squish(.meta(html, "citation_reference"))
  v[nzchar(v)]
}

.extraer_keywords <- function(html) {

  kw <- str_squish(.meta(html, "citation_keywords"))
  if (length(kw) == 0) kw <- str_squish(.meta(html, "dc.subject"))
  if (length(kw) == 0) {
    nodos <- html_elements(html, ".c-article-subject-list__subject")
    if (length(nodos) > 0) kw <- str_squish(html_text2(nodos))
  }
  kw <- kw[nzchar(kw)]
  if (length(kw) == 1 && str_detect(kw, ";")) {
    kw <- str_squish(str_split(kw, ";")[[1]])
  }
  unique(kw[nzchar(kw)])
}


enriquecer_desde_springer <- function(doi) {
  doi <- normalizar_doi(doi)
  if (is.na(doi)) return(NULL)

  html <- .get_html(paste0("https://link.springer.com/article/", doi))
  if (is.null(html)) return(NULL)

  titulo <- .primero(.meta(html, "citation_title"))
  if (titulo == "NA") {
    h1 <- html_elements(html, "h1.c-article-title, h1.app-article-title, h1")
    if (length(h1) > 0) titulo <- .primero(html_text2(h1[1]))
  }

  fecha <- .primero(.meta(html, "citation_online_date"), NA_character_)
  if (is.na(fecha)) fecha <- .primero(.meta(html, "prism.publicationDate"), NA_character_)
  if (is.na(fecha)) fecha <- .primero(.meta(html, "citation_cover_date"), NA_character_)
  if (!is.na(fecha)) fecha <- str_replace_all(fecha, "/", "-")

  abstract <- .primero(.meta(html, "dc.description"), NA_character_)
  if (is.na(abstract)) {
    nodo <- html_elements(html, "#Abs1-content, .c-article-section__content")
    if (length(nodo) > 0) abstract <- .primero(html_text2(nodo[1]), NA_character_)
  }

  autores <- .normalizar_autor(.meta(html, "citation_author"))
  autores <- autores[nzchar(autores)]
  if (length(autores) == 0) {
    nodo <- html_elements(html, ".c-article-author-list__item, .app-article-author-list__item")
    if (length(nodo) > 0) autores <- str_squish(html_text2(nodo))
  }

  refs      <- .extraer_referencias(html)
  keywords  <- .extraer_keywords(html)

  accesos <- .extraer_contador(html, "access")
  citas   <- .extraer_contador(html, "citation")

  rm(html); invisible(gc(verbose = FALSE, full = FALSE))

  list(
    titulo            = titulo,
    fecha_publicacion = fecha,
    abstract          = abstract,
    autores           = autores,
    referencias       = refs,
    keywords          = keywords,
    accesos           = accesos,
    citas             = citas
  )
}

.fusionar <- function(fila, extra) {
  if (is.null(extra)) return(fila)

  if (!is.null(extra$titulo) && extra$titulo != "NA") fila$titulo <- extra$titulo
  if (!is.null(extra$fecha_publicacion) && !is.na(extra$fecha_publicacion))
    fila$fecha_publicacion <- extra$fecha_publicacion
  if (!is.null(extra$abstract) && !is.na(extra$abstract) && nchar(extra$abstract) > 20)
    fila$abstract <- extra$abstract
  if (length(extra$autores) > 0) fila$autores_json <- safe_to_json(extra$autores)
  if (length(extra$referencias) > 0) fila$referencias_json <- safe_to_json(extra$referencias)
  if (length(extra$keywords) > 0) fila$keywords <- paste(extra$keywords, collapse = "; ")
  if (!is.na(extra$accesos)) fila$metricas_visualizaciones <- paste(extra$accesos, "Accesses")
  if (!is.na(extra$citas))   fila$metricas_citas <- paste(extra$citas, "Citations")

  fila
}

# ==============================================================================
# CAPA DE BASE DE DATOS
# ==============================================================================

.abrir_db <- function(db = DB_PATH) {
  con <- dbConnect(RSQLite::SQLite(), dbname = db)
  dbExecute(con, "PRAGMA busy_timeout = 5000;")
  con
}

.escribir_lote <- function(con, tabla, df) {
  if (nrow(df) == 0) return(invisible(NULL))

  if (!dbExistsTable(con, tabla)) {
    dbWriteTable(con, tabla, as.data.frame(df))
    return(invisible(NULL))
  }
  cols_bd <- dbListFields(con, tabla)
  for (cc in setdiff(names(df), cols_bd)) {
    dbExecute(con, sprintf('ALTER TABLE "%s" ADD COLUMN "%s" TEXT', tabla, cc))
  }
  cols_bd <- dbListFields(con, tabla)
  for (cc in setdiff(cols_bd, names(df))) df[[cc]] <- NA
  dbWriteTable(con, tabla, as.data.frame(df[, cols_bd, drop = FALSE]), append = TRUE)
  invisible(NULL)
}

.dois_existentes <- function(con) {
  if (!dbExistsTable(con, "articulos_crudos")) return(character(0))
  d <- dbGetQuery(con, "SELECT doi FROM articulos_crudos WHERE doi IS NOT NULL")$doi
  unique(tolower(trimws(d)))
}

.solo_numero <- function(x) {
  n <- suppressWarnings(as.integer(str_remove_all(as.character(x), "[^0-9]")))
  ifelse(is.na(n), 0L, n)
}

# ==============================================================================
# FUNCION PRINCIPAL
# ==============================================================================



#' @param modo        "incremental" (por defecto) o "completo"
#' @param desde       fecha minima de publicacion (YYYY-MM-DD)
#' @param max_nuevos  tope de articulos enriquecidos en esta corrida
#' @param enriquecer  TRUE = visita la pagina de Springer para accesos/referencias
#' @param progreso    funcion opcional f(fraccion, texto) para withProgress
#'
#' @return lista con: ok, tipo, titulo, mensaje, detalle, n_nuevos,
#'         n_actualizados, n_metricas_cambiadas, dois_nuevos, fallidos, segundos
ejecutar_scraping <- function(modo = c("incremental", "completo"),
                              desde = "2025-01-01",
                              max_nuevos = MAX_NUEVOS_POR_SYNC,
                              enriquecer = TRUE,
                              db = DB_PATH,
                              progreso = NULL) {

  modo <- match.arg(modo)
  t0   <- Sys.time()
  avisar <- function(f, txt) if (is.function(progreso)) progreso(f, txt)

  con <- .abrir_db(db)
  on.exit({ if (dbIsValid(con)) dbDisconnect(con) }, add = TRUE)

  # ---- Modo completo: purga previa ------------------------------------------
  if (modo == "completo") {
    for (t in c("articulos_crudos", "papers", "authors", "paper_authors",
                "references", "paper_references")) {
      if (dbExistsTable(con, t)) dbRemoveTable(con, t)
    }
  }

  # ---- FASE 1: descubrimiento -----------------------------------------------
  avisar(0.15, "Consultando el indice de Crossref...")
  limite_descubrimiento <- if (modo == "completo") NULL else 300
  catalogo  <- descubrir_articulos(desde = desde, limite = limite_descubrimiento)
  total_api <- attr(catalogo, "total_api")

  if (nrow(catalogo) == 0) {
    return(list(
      ok = FALSE, tipo = "error",
      titulo  = "Sin respuesta del indice",
      mensaje = "Crossref no devolvio resultados. Revisa la conexion o el filtro de fechas.",
      detalle = character(0),
      n_nuevos = 0, n_actualizados = 0, n_metricas_cambiadas = 0,
      dois_nuevos = character(0), fallidos = character(0),
      segundos = as.numeric(difftime(Sys.time(), t0, units = "secs"))
    ))
  }

  existentes <- .dois_existentes(con)
  nuevos     <- catalogo %>% filter(!(doi %in% existentes))

  # --------------------------------------------------------------------------
  # RAMA A: no hay articulos nuevos -> refrescar metricas de los ultimos 5
  # --------------------------------------------------------------------------
  if (nrow(nuevos) == 0 && modo == "incremental") {

    avisar(0.35, "Sin novedades. Refrescando metricas recientes...")

    ultimos <- dbGetQuery(con, "
      SELECT doi, titulo, metricas_visualizaciones, metricas_citas
      FROM articulos_crudos
      WHERE doi IS NOT NULL
      ORDER BY date(fecha_publicacion) DESC, rowid DESC
      LIMIT 5")

    if (nrow(ultimos) == 0) {
      return(list(
        ok = TRUE, tipo = "sin_cambios",
        titulo  = "Base vacia",
        mensaje = "No hay articulos en la base todavia. Ejecuta una reconstruccion completa.",
        detalle = character(0),
        n_nuevos = 0, n_actualizados = 0, n_metricas_cambiadas = 0,
        dois_nuevos = character(0), fallidos = character(0),
        segundos = as.numeric(difftime(Sys.time(), t0, units = "secs"))
      ))
    }

    cambios  <- character(0)
    fallidos <- character(0)

    for (i in seq_len(nrow(ultimos))) {
      d <- ultimos$doi[i]
      avisar(0.35 + 0.5 * i / nrow(ultimos),
             paste0("Actualizando metricas ", i, "/", nrow(ultimos)))

      extra <- if (enriquecer) tryCatch(enriquecer_desde_springer(d), error = function(e) NULL) else NULL
      .pausa()

      acc_new <- if (!is.null(extra) && !is.na(extra$accesos)) extra$accesos else NA_character_
      cit_new <- if (!is.null(extra) && !is.na(extra$citas))   extra$citas   else NA_character_


      if (is.na(cit_new)) {
        fila_cr <- catalogo %>% filter(doi == d)
        if (nrow(fila_cr) == 1) cit_new <- as.character(.solo_numero(fila_cr$metricas_citas[1]))
      }

      if (is.na(acc_new) && is.na(cit_new)) { fallidos <- c(fallidos, d); next }

      acc_old <- .solo_numero(ultimos$metricas_visualizaciones[i])
      cit_old <- .solo_numero(ultimos$metricas_citas[i])

      if (!is.na(acc_new)) {
        dbExecute(con, "UPDATE articulos_crudos SET metricas_visualizaciones = ? WHERE doi = ?",
                  params = list(paste(acc_new, "Accesses"), d))
      }
      if (!is.na(cit_new)) {
        dbExecute(con, "UPDATE articulos_crudos SET metricas_citas = ? WHERE doi = ?",
                  params = list(paste(cit_new, "Citations"), d))
      }

      d_acc <- if (!is.na(acc_new)) as.integer(acc_new) - acc_old else 0L
      d_cit <- if (!is.na(cit_new)) as.integer(cit_new) - cit_old else 0L

      if (d_acc != 0 || d_cit != 0) {
        cambios <- c(cambios, sprintf(
          "%s  ->  accesos %+d, citas %+d",
          str_trunc(ultimos$titulo[i], 58), d_acc, d_cit
        ))
      }
    }

    n_camb <- length(cambios)
    msg <- if (n_camb > 0) {
      sprintf("Sin articulos nuevos. Se refrescaron las metricas de los %d articulos mas recientes; %d cambiaron.",
              nrow(ultimos), n_camb)
    } else {
      sprintf("Sin articulos nuevos. Se refrescaron las metricas de los %d articulos mas recientes; ninguna vario.",
              nrow(ultimos))
    }

    return(list(
      ok = TRUE, tipo = "refresco_metricas",
      titulo  = "Metricas actualizadas",
      mensaje = msg,
      detalle = cambios,
      n_nuevos = 0, n_actualizados = nrow(ultimos), n_metricas_cambiadas = n_camb,
      dois_nuevos = character(0), fallidos = fallidos,
      segundos = as.numeric(difftime(Sys.time(), t0, units = "secs"))
    ))
  }

  # --------------------------------------------------------------------------
  # RAMA B: hay articulos nuevos
  # --------------------------------------------------------------------------
  truncado <- FALSE
  if (nrow(nuevos) > max_nuevos) {
    nuevos   <- head(nuevos, max_nuevos)
    truncado <- TRUE
  }

  lote     <- list()
  fallidos <- character(0)
  pendiente <- 1L          
  TAM_CHUNK <- 25L


  vaciar_buffer <- function() {
    if (length(lote) < pendiente) return(TRUE)
    df <- bind_rows(lote[pendiente:length(lote)]) %>% distinct(doi, .keep_all = TRUE)
    dbBegin(con)
    ok <- tryCatch({ .escribir_lote(con, "articulos_crudos", df); TRUE },
                   error = function(e) FALSE)
    if (ok) {
      dbCommit(con)
      pendiente <<- length(lote) + 1L
    } else {
      dbRollback(con)
    }
    ok
  }

  for (i in seq_len(nrow(nuevos))) {
    fila <- nuevos[i, ]
    avisar(0.2 + 0.65 * i / nrow(nuevos),
           paste0("Procesando ", i, "/", nrow(nuevos), ": ", str_trunc(fila$titulo, 40)))

    if (enriquecer) {
      extra <- tryCatch(enriquecer_desde_springer(fila$doi), error = function(e) NULL)
      if (is.null(extra)) fallidos <- c(fallidos, fila$doi)
      fila <- .fusionar(fila, extra)
      .pausa()
    }

    lote[[length(lote) + 1]] <- fila

    if (length(lote) - pendiente + 1L >= TAM_CHUNK) vaciar_buffer()
  }

  if (length(lote) > 0) {
    avisar(0.9, "Guardando en la base de datos...")
    if (!vaciar_buffer()) {
      return(list(
        ok = FALSE, tipo = "error",
        titulo  = "Error al guardar",
        mensaje = sprintf("Se guardaron %d articulos, pero el ultimo bloque fallo al escribirse en SQLite.",
                          pendiente - 1L),
        detalle = character(0),
        n_nuevos = pendiente - 1L, n_actualizados = 0, n_metricas_cambiadas = 0,
        dois_nuevos = character(0), fallidos = fallidos,
        segundos = as.numeric(difftime(Sys.time(), t0, units = "secs"))
      ))
    }
  }

  titulos <- head(bind_rows(lote)$titulo, 5)

  msg <- if (modo == "completo") {
    sprintf("Reconstruccion total: la base quedo con %d articulos indexados.", length(lote))
  } else if (length(lote) == 1) {
    "Sincronizacion completada: 1 articulo nuevo agregado al corpus."
  } else {
    sprintf("Sincronizacion completada: %d articulos nuevos agregados al corpus.", length(lote))
  }
  if (truncado) {
    msg <- paste0(msg, sprintf(" Se aplico el tope de %d por corrida; vuelve a sincronizar para traer el resto.", max_nuevos))
  }
  if (length(fallidos) > 0) {
    msg <- paste0(msg, sprintf(" %d articulo(s) quedaron solo con metadatos de Crossref (sin accesos).", length(fallidos)))
  }
  if (modo == "completo" && !is.na(total_api) && total_api > nrow(catalogo)) {
    msg <- paste0(msg, sprintf(
      " AVISO: Crossref reporta %d registros para este filtro pero solo se recuperaron %d; revisa `desde` o `max_paginas`.",
      total_api, nrow(catalogo)))
  }

  list(
    ok = TRUE,
    tipo    = if (modo == "completo") "reconstruccion" else "nuevos",
    titulo  = if (modo == "completo") "Base reconstruida" else "Articulos nuevos",
    mensaje = msg,
    detalle = str_trunc(titulos, 70),
    n_nuevos = length(lote), n_actualizados = 0, n_metricas_cambiadas = 0,
    dois_nuevos = bind_rows(lote)$doi,
    fallidos = fallidos,
    segundos = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}


CENTINELA_SIN_KW <- "SIN_KEYWORDS"

completar_keywords <- function(db = DB_PATH, max_articulos = 2000, progreso = NULL) {

  t0  <- Sys.time()
  con <- .abrir_db(db)
  on.exit({ if (dbIsValid(con)) dbDisconnect(con) }, add = TRUE)

  if (!dbExistsTable(con, "articulos_crudos")) {
    return(list(ok = FALSE, mensaje = "No existe la tabla articulos_crudos."))
  }

  if (!("keywords" %in% dbListFields(con, "articulos_crudos"))) {
    dbExecute(con, 'ALTER TABLE "articulos_crudos" ADD COLUMN "keywords" TEXT')
  }

  pendientes <- dbGetQuery(con, "
    SELECT doi FROM articulos_crudos
    WHERE doi IS NOT NULL AND trim(doi) <> ''
      AND (keywords IS NULL OR trim(keywords) = '' OR keywords = 'NA')
    ORDER BY rowid")$doi

  total_pend <- length(pendientes)
  if (total_pend == 0) {
    return(list(ok = TRUE, n_con_kw = 0, n_sin_kw = 0, n_fallidos = 0,
                pendientes = 0, segundos = 0,
                mensaje = "Todos los articulos ya tienen el campo keywords resuelto."))
  }

  pendientes <- head(pendientes, max_articulos)
  n <- length(pendientes)

  con_kw <- 0L; sin_kw <- 0L; fallidos <- character(0)

  for (i in seq_len(n)) {
    d <- pendientes[i]

    if (is.function(progreso)) {
      progreso(i / n, sprintf("Keywords %d/%d", i, n))
    } else if (i %% 25 == 0 || i == 1 || i == n) {
      cat(sprintf("\r  %d/%d  (con: %d | sin: %d | fallos: %d)   ",
                  i, n, con_kw, sin_kw, length(fallidos)))
      utils::flush.console()
    }

    html <- tryCatch(.get_html(paste0("https://link.springer.com/article/", d)),
                     error = function(e) NULL)
    .pausa()

    if (is.null(html)) { fallidos <- c(fallidos, d); next }

    kw <- tryCatch(.extraer_keywords(html), error = function(e) character(0))
    rm(html); invisible(gc(verbose = FALSE, full = FALSE))

    valor <- if (length(kw) > 0) paste(kw, collapse = "; ") else CENTINELA_SIN_KW

    ok <- tryCatch({
      dbExecute(con, "UPDATE articulos_crudos SET keywords = ? WHERE doi = ?",
                params = list(valor, d))
      TRUE
    }, error = function(e) FALSE)

    if (!ok) { fallidos <- c(fallidos, d); next }
    if (length(kw) > 0) con_kw <- con_kw + 1L else sin_kw <- sin_kw + 1L
  }

  if (!is.function(progreso)) cat("\n")

  restantes <- max(0L, total_pend - n)

  msg <- sprintf("Keywords: %d articulos con palabras clave, %d sin ellas en la fuente, %d fallidos.",
                 con_kw, sin_kw, length(fallidos))
  if (restantes > 0) {
    msg <- paste0(msg, sprintf(" Quedan %d por procesar; vuelve a ejecutar.", restantes))
  }
  if (length(fallidos) > 0) {
    msg <- paste0(msg, " Los fallidos se reintentan en la siguiente corrida.")
  }

  list(ok = TRUE, n_con_kw = con_kw, n_sin_kw = sin_kw,
       n_fallidos = length(fallidos), pendientes = restantes,
       fallidos = fallidos, mensaje = msg,
       segundos = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

# ==============================================================================
# BUSQUEDA POR DOI
# ==============================================================================

buscar_doi_local <- function(doi_raw, db = DB_PATH) {
  d <- normalizar_doi(doi_raw)
  if (is.na(d)) return(NULL)

  con <- .abrir_db(db)
  on.exit(dbDisconnect(con), add = TRUE)
  if (!dbExistsTable(con, "papers")) return(NULL)

  res <- dbGetQuery(con, "SELECT * FROM papers WHERE lower(trim(doi)) = ?", params = list(d))
  if (nrow(res) == 0) return(NULL)
  res
}

sugerir_dois <- function(fragmento, n = 8, db = DB_PATH) {
  f <- tolower(trimws(as.character(fragmento)))
  if (!nzchar(f)) return(data.frame())

  con <- .abrir_db(db)
  on.exit(dbDisconnect(con), add = TRUE)
  if (!dbExistsTable(con, "papers")) return(data.frame())

  dbGetQuery(con,
    "SELECT doi, title, year FROM papers WHERE lower(doi) LIKE ? ORDER BY year DESC LIMIT ?",
    params = list(paste0("%", f, "%"), n))
}


#' @return 
importar_doi <- function(doi_raw, enriquecer = TRUE, db = DB_PATH) {

  d <- normalizar_doi(doi_raw)
  if (is.na(d)) {
    return(list(ok = FALSE, doi = NA_character_, titulo = NA_character_,
                mensaje = "El texto ingresado no tiene forma de DOI (debe empezar por 10.xxxx/)."))
  }

  con <- .abrir_db(db)
  on.exit(dbDisconnect(con), add = TRUE)

  if (d %in% .dois_existentes(con)) {
    return(list(ok = FALSE, doi = d, titulo = NA_character_,
                mensaje = "Ese DOI ya esta en el corpus."))
  }

  js <- .get_json(paste0("https://api.crossref.org/works/", d))
  if (is.null(js) || is.null(js$message)) {
    return(list(ok = FALSE, doi = d, titulo = NA_character_,
                mensaje = "Crossref no encontro ese DOI."))
  }

  fila <- .crossref_a_tibble(js$message)
  if (is.null(fila)) {
    return(list(ok = FALSE, doi = d, titulo = NA_character_,
                mensaje = "El registro de Crossref llego incompleto."))
  }

  revista <- .primero(unlist(js$message$`container-title`), "")
  es_air  <- str_detect(tolower(revista), "artificial intelligence review")

  if (enriquecer) {
    extra <- tryCatch(enriquecer_desde_springer(d), error = function(e) NULL)
    fila  <- .fusionar(fila, extra)
  }

  dbBegin(con)
  ok <- tryCatch({ .escribir_lote(con, "articulos_crudos", fila); TRUE },
                 error = function(e) FALSE)
  if (ok) dbCommit(con) else dbRollback(con)

  if (!ok) {
    return(list(ok = FALSE, doi = d, titulo = fila$titulo[1],
                mensaje = "No se pudo escribir el registro en SQLite."))
  }

  msg <- paste0("DOI importado: ", str_trunc(fila$titulo[1], 60), ".")
  if (!es_air && nzchar(revista)) {
    msg <- paste0(msg, " Aviso: pertenece a '", str_trunc(revista, 40),
                  "', no a Artificial Intelligence Review.")
  }

  list(ok = TRUE, doi = d, titulo = fila$titulo[1], mensaje = msg)
}


SUPERUSER_PASS <- Sys.getenv("SVM_SUPERUSER", unset = "mineria2026")

# ==============================================================================
# HELPERS GENERALES (BLINDADOS CONTRA VECTORES)
# ==============================================================================
seguro_texto <- function(x) {
  if (is.null(x) || length(x) == 0) return("NA")
  as.character(x[1])
}

format_numero <- function(n) {
  if (is.null(n) || length(n) == 0 || any(is.na(n))) return("\u2014")
  n <- as.numeric(n[1])
  if (is.na(n)) return("\u2014")
  if (n >= 1e6) return(paste0(round(n / 1e6, 1), "M"))
  if (n >= 1e3) return(paste0(round(n / 1e3, 1), "K"))
  as.character(round(n))
}

# ==============================================================================
# TEMA HIGHCHARTER - NEXUS DARK
# ==============================================================================
nexus_theme <- hc_theme(
  chart = list(
    backgroundColor = "transparent",
    style = list(fontFamily = "'IBM Plex Sans', 'Inter', sans-serif")
  ),
  colors = c("#6366f1", "#22d3ee", "#f59e0b", "#10b981", "#f43f5e", "#a78bfa", "#34d399"),
  title = list(
    style = list(color = "#f1f5f9", fontWeight = "700", fontSize = "14px")
  ),
  subtitle = list(style = list(color = "#64748b", fontSize = "11px")),
  xAxis = list(
    gridLineColor = "rgba(148,163,184,0.07)",
    lineColor = "rgba(148,163,184,0.15)",
    tickColor = "transparent",
    labels = list(style = list(color = "#64748b", fontSize = "11px")),
    title = list(style = list(color = "#64748b"))
  ),
  yAxis = list(
    gridLineColor = "rgba(148,163,184,0.07)",
    lineColor = "transparent",
    labels = list(style = list(color = "#64748b", fontSize = "11px")),
    title = list(style = list(color = "#64748b"))
  ),
  legend = list(
    itemStyle = list(color = "#94a3b8", fontWeight = "500", fontSize = "12px"),
    itemHoverStyle = list(color = "#f1f5f9")
  ),
  tooltip = list(
    backgroundColor = "rgba(15, 23, 42, 0.95)",
    borderColor = "rgba(99,102,241,0.4)",
    borderWidth = 1,
    borderRadius = 10,
    style = list(color = "#e2e8f0", fontSize = "12px"),
    shadow = list(color = "rgba(99,102,241,0.2)", offsetX = 0, offsetY = 4, opacity = 0.4, width = 12)
  ),
  plotOptions = list(
    series = list(
      animation = list(duration = 900, easing = "easeOutCubic")
    )
  )
)

# ==============================================================================
# NORMALIZACION RELACIONAL
# ==============================================================================
normalizar_tablas <- function(db = DB_PATH) {
  con <- dbConnect(RSQLite::SQLite(), dbname = db)
  on.exit(dbDisconnect(con), add = TRUE)

  if (!dbExistsTable(con, "articulos_crudos")) return(invisible(NULL))

  datos_crudos <- dbReadTable(con, "articulos_crudos") %>%
    mutate(doi = tolower(trimws(doi))) %>%
    filter(!is.na(doi), doi != "")


  if (!("keywords" %in% names(datos_crudos))) datos_crudos$keywords <- NA_character_
  datos_crudos$keywords[datos_crudos$keywords %in% c("SIN_KEYWORDS", "NA", "")] <- NA_character_

  if (nrow(datos_crudos) == 0) return(invisible(NULL))

  p_actual <- datos_crudos %>%
    select(doi, titulo, fecha_publicacion, url, abstract, keywords,
           metricas_visualizaciones, metricas_citas) %>%
    distinct(doi, .keep_all = TRUE) %>%
    arrange(desc(fecha_publicacion), doi) %>%   
    mutate(
      visualizaciones_num = as.integer(str_remove_all(metricas_visualizaciones, "[^0-9]")),
      citas_num           = as.integer(str_remove_all(metricas_citas, "[^0-9]"))
    ) %>%
    select(-metricas_visualizaciones, -metricas_citas)

  a_actual <- datos_crudos %>%
    select(doi, autores_json) %>%
    filter(!is.na(autores_json) & autores_json != "NA") %>%
    mutate(autor = map(autores_json, ~ tryCatch(fromJSON(.x), error = function(e) character(0)))) %>%
    unnest(autor) %>%
    select(doi, autor) %>%
    filter(!is.na(autor), nzchar(trimws(autor)))

  r_actual <- datos_crudos %>%
    select(doi, referencias_json) %>%
    filter(!is.na(referencias_json) & referencias_json != "NA") %>%
    mutate(referencia = map(referencias_json, ~ tryCatch(fromJSON(.x), error = function(e) character(0)))) %>%
    unnest(referencia) %>%
    select(doi, referencia) %>%
    filter(!is.na(referencia), nzchar(trimws(referencia)))

  authors <- a_actual %>%
    distinct(autor) %>%
    rename(author_name = autor) %>%
    arrange(author_name) %>%
    mutate(author_id = row_number())

  references <- r_actual %>%
    distinct(referencia) %>%
    rename(reference_text_normalized = referencia) %>%
    arrange(reference_text_normalized) %>%
    mutate(reference_id = row_number())

  p_con_id <- p_actual %>%
    mutate(paper_id = row_number()) %>%
    select(paper_id, doi)

  paper_authors <- a_actual %>%
    inner_join(p_con_id, by = "doi") %>%
    inner_join(authors, by = c("autor" = "author_name")) %>%
    group_by(paper_id) %>%
    mutate(author_order = row_number()) %>%
    ungroup() %>%
    select(paper_id, author_id, author_order)

  paper_references <- r_actual %>%
    inner_join(p_con_id, by = "doi") %>%
    inner_join(references, by = c("referencia" = "reference_text_normalized")) %>%
    select(paper_id, reference_id)

  resumen_autores <- a_actual %>%
    group_by(doi) %>%
    summarise(n_authors = n(), authors_raw = paste(autor, collapse = ", "), .groups = "drop")

  resumen_refs <- r_actual %>%
    group_by(doi) %>%
    summarise(n_references = n(), .groups = "drop")

  kw_ia_gen <- "generative|llm|chatgpt|large language model|diffusion model|gan\\b|transformer"
  kw_ml     <- "machine learning|deep learning|neural network|predictive|classification|reinforcement"
  kw_stat   <- "statistics|statistical|bayesian|regression|anova|probability|inference"

  papers_final <- p_actual %>%
    inner_join(p_con_id, by = "doi") %>%
    left_join(resumen_autores, by = "doi") %>%
    left_join(resumen_refs, by = "doi") %>%
    mutate(
      journal_name     = "Artificial Intelligence Review",
      year             = as.numeric(str_extract(fecha_publicacion, "\\d{4}")),
      title            = titulo,
      publication_date = fecha_publicacion,
      citations        = citas_num,
      downloads        = visualizaciones_num,
      texto_busqueda   = tolower(paste(titulo, abstract, ifelse(is.na(keywords), "", keywords))),
      topic_label = case_when(
        str_detect(texto_busqueda, kw_ia_gen) ~ "IA Generativa",
        str_detect(texto_busqueda, kw_ml)     ~ "Machine Learning",
        str_detect(texto_busqueda, kw_stat)   ~ "Estad\u00edstica",
        TRUE ~ "Otros"
      )
    ) %>%
    select(paper_id, journal_name, title, publication_date, year, doi, url,
           abstract, keywords, authors_raw, n_authors, citations, downloads,
           n_references, topic_label)

  dbWriteTable(con, "papers",           papers_final,     overwrite = TRUE)
  dbWriteTable(con, "authors",          authors,          overwrite = TRUE)
  dbWriteTable(con, "paper_authors",    paper_authors,    overwrite = TRUE)
  dbWriteTable(con, "references",       references,       overwrite = TRUE)
  dbWriteTable(con, "paper_references", paper_references, overwrite = TRUE)


  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_papers_doi ON papers(doi)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_crudos_doi ON articulos_crudos(doi)")

  invisible(TRUE)
}

# ==============================================================================
# MODULO DE RECUPERACION DE INFORMACION  (Taller 4)
# ==============================================================================

library(Matrix)
library(SnowballC)
library(stringi)

RUTA_INDICE <- "search_index.rds"

# ------------------------------------------------------------------------------
# LISTAS DE PALABRAS
# ------------------------------------------------------------------------------
STOPWORDS_EN <- c(
  "the","and","for","are","but","not","you","all","any","can","had","her","was",
  "one","our","out","day","get","has","him","his","how","its","new","now","old",
  "see","two","way","who","boy","did","use","that","this","with","from","they",
  "have","been","were","said","each","which","their","will","other","about",
  "into","than","them","these","some","would","make","like","time","also","more",
  "very","when","much","then","only","over","such","most","after","before",
  "between","both","during","under","while","where","there","through","against",
  "above","below","again","further","once","here","because","being","doing",
  "does","done","having","itself","myself","ourselves","themselves","what",
  "whom","whose","why","yourself","should","could","might","must","shall","upon",
  "within","without","among","along","across","behind","beyond","toward",
  "towards","however","therefore","thus","hence","moreover","furthermore",
  "although","though","whereas","whether","either","neither","nor","yet","per",
  "via","versus","etc","ing","non","pre","post","sub","non"
)

STOPWORDS_DOM <- c(
  "paper","papers","article","articles","abstract","introduction","conclusion",
  "conclusions","author","authors","doi","springer","journal","copyright",
  "publish","published","publisher","review","reviews","section","figure",
  "table","reference","references","et","al"
)

STOPWORDS_ES <- c(
  "para","como","los","las","una","unos","unas","del","con","por","que","sus",
  "esta","este","estos","estas","son","fue","ser","han","hay","mas","pero",
  "sobre","entre","desde","hasta","cuando","donde","porque","aunque","tambien",
  "muy","cada","otro","otra","otros","otras","todo","toda","todos","todas",
  "ademas","segun","mediante","durante","antes","despues","siendo","haber"
)

ACRONIMOS_CORTOS <- c("ai", "ml", "dl", "rl", "nn", "cv", "kg", "qa",
                      "ir", "ar", "vr", "ga", "kb", "bi", "lm", "rf")

STOPWORDS <- unique(c(STOPWORDS_EN, STOPWORDS_DOM, STOPWORDS_ES))

# ------------------------------------------------------------------------------
# PUENTE ESPANOL -> INGLES
# ------------------------------------------------------------------------------
LEXICO_ES_EN <- c(
  "inteligencia" = "intelligence", "artificial" = "artificial",
  "aprendizaje" = "learning", "profundo" = "deep", "profunda" = "deep",
  "maquina" = "machine", "maquinas" = "machine", "automatico" = "automatic",
  "red" = "network", "redes" = "network", "neuronal" = "neural",
  "neuronales" = "neural", "generativa" = "generative", "generativo" = "generative",
  "modelo" = "model", "modelos" = "model", "lenguaje" = "language",
  "grande" = "large", "grandes" = "large", "texto" = "text",
  "imagen" = "image", "imagenes" = "image", "vision" = "vision",
  "diagnostico" = "diagnosis", "enfermedad" = "disease",
  "enfermedades" = "disease", "medico" = "medical", "medica" = "medical",
  "salud" = "health", "clinico" = "clinical", "clinica" = "clinical",
  "paciente" = "patient", "pacientes" = "patient",
  "datos" = "data", "mineria" = "mining", "clasificacion" = "classification",
  "clasificador" = "classifier", "regresion" = "regression",
  "agrupamiento" = "clustering", "prediccion" = "prediction",
  "predictivo" = "predictive", "optimizacion" = "optimization",
  "algoritmo" = "algorithm", "algoritmos" = "algorithm",
  "reconocimiento" = "recognition", "deteccion" = "detection",
  "segmentacion" = "segmentation", "procesamiento" = "processing",
  "natural" = "natural", "refuerzo" = "reinforcement",
  "supervisado" = "supervised", "transferencia" = "transfer",
  "atencion" = "attention", "transformador" = "transformer",
  "explicabilidad" = "explainability", "explicable" = "explainable",
  "interpretabilidad" = "interpretability", "incertidumbre" = "uncertainty",
  "difuso" = "fuzzy", "difusa" = "fuzzy", "evolutivo" = "evolutionary",
  "genetico" = "genetic", "enjambre" = "swarm", "agente" = "agent",
  "agentes" = "agent", "conocimiento" = "knowledge", "grafo" = "graph",
  "grafos" = "graph", "recuperacion" = "retrieval", "busqueda" = "search",
  "recomendacion" = "recommendation", "seguridad" = "security",
  "privacidad" = "privacy", "sesgo" = "bias", "etica" = "ethics",
  "energia" = "energy", "industria" = "industry", "financiero" = "financial",
  "aplicacion" = "application", "aplicaciones" = "application",
  "revision" = "review", "encuesta" = "survey", "sistemas" = "system",
  "sistema" = "system", "cuantico" = "quantum", "cuantica" = "quantum",
  "federado" = "federated", "federada" = "federated"
)

# ==============================================================================
# CORRECCION APROXIMADA # ==============================================================================

.mas_cercano <- function(token, candidatos, frec = NULL, max_dist = 2L,
                         exigir_unico = FALSE) {
  if (length(candidatos) == 0 || is.na(token) || nchar(token) < 4) return(NA_character_)
  n <- nchar(token)
  cand <- candidatos[substr(candidatos, 1, 1) == substr(token, 1, 1) &
                       abs(nchar(candidatos) - n) <= max_dist]
  if (length(cand) == 0) return(NA_character_)

  d <- utils::adist(token, cand)[1, ]
  m <- min(d)
  if (m == 0) return(NA_character_)
  if (m > max_dist || m > max(1L, floor(n / 3))) return(NA_character_)

  empatados <- cand[d == m]
  if (length(empatados) == 1) return(empatados)


  if (exigir_unico) return(NA_character_)


  if (!is.null(frec)) {
    f <- frec[match(empatados, names(frec))]
    f[is.na(f)] <- 0L
    return(empatados[which.max(f)])
  }
  empatados[which.min(abs(nchar(empatados) - n))]
}

# ==============================================================================
# SEGMENTACION DE PALABRAS PEGADAS
# ==============================================================================
.segmentar <- function(token, superficie, frec, min_parte = 4L) {
  n <- nchar(token)
  if (is.na(token) || n < 8L) return(NULL)

  mejor <- NULL
  mejor_puntaje <- -1L

  for (i in seq.int(min_parte, n - min_parte)) {
    a <- substr(token, 1L, i)
    b <- substr(token, i + 1L, n)
    if (!(a %in% superficie) || !(b %in% superficie)) next

    puntaje <- min(frec[[a]], frec[[b]])
    if (puntaje > mejor_puntaje) {
      mejor_puntaje <- puntaje
      mejor <- c(a, b)
    }
  }
  mejor
}

# ==============================================================================
# TOKENIZADOR  (compartido por build_index.R y por las consultas)
# ==============================================================================


#' @param txt vector de textos (se procesa elemento por elemento)
#' @param bigramas si TRUE agrega los bigramas de tokens contiguos
#' @return lista de vectores de tokens, uno por documento
#' @param idx si se pasa el indice cargado, se activa la correccion aproximada
#'   (modo consulta). Si es NULL no se corrige nada (modo indexacion). Las
#'   correcciones aplicadas quedan en el atributo "correcciones".
#' @param solo_superficie si TRUE devuelve las formas SIN stemmizar y sin
#'   bigramas. Lo usa build_index.R para armar el diccionario de superficie
#'   contra el que se corrigen las consultas.
tokenizar <- function(txt, bigramas = TRUE, idx = NULL, solo_superficie = FALSE) {
  txt <- as.character(txt)
  txt[is.na(txt)] <- ""

  corregir      <- !is.null(idx)
  vocabulario   <- if (corregir) idx$vocabulario   else NULL
  superficie    <- if (corregir) idx$superficie    else NULL
  superficie_df <- if (corregir) idx$superficie_df else NULL
  claves_es <- names(LEXICO_ES_EN)
  registro  <- new.env(parent = emptyenv())
  registro$cor <- character(0)

  txt <- tolower(txt)
  txt <- stringi::stri_trans_general(txt, "Latin-ASCII")


  txt <- gsub("[^a-z0-9-]+", " ", txt)

  res <- lapply(txt, function(s) {
    toks <- strsplit(trimws(s), "\\s+")[[1]]
    if (length(toks) == 0 || identical(toks, "")) return(character(0))

    toks <- gsub("^-+|-+$", "", toks)          
    toks <- toks[nchar(toks) >= 3 | toks %in% ACRONIMOS_CORTOS]
    toks <- toks[!grepl("^[0-9-]+$", toks)]    
    if (length(toks) == 0) return(character(0))


    intocable <- rep(FALSE, length(toks))
    if (corregir) {
      intocable <- (SnowballC::wordStem(toks, language = "english") %in% vocabulario) |
                   (toks %in% superficie)
    }


    hit <- (toks %in% claves_es) & !intocable
    if (any(hit)) toks[hit] <- unname(LEXICO_ES_EN[toks[hit]])

    if (corregir && any(!hit & !intocable)) {
      for (p in which(!hit & !intocable)) {
        cand <- .mas_cercano(toks[p], claves_es, max_dist = 1L, exigir_unico = TRUE)
        if (!is.na(cand)) {
          registro$cor <- c(registro$cor, stats::setNames(cand, toks[p]))
          toks[p] <- unname(LEXICO_ES_EN[cand])
          intocable[p] <- TRUE
        }
      }
    }

    toks <- toks[!toks %in% STOPWORDS]
    if (length(toks) == 0) return(character(0))


    if (corregir && !is.null(superficie)) {
      provisional <- SnowballC::wordStem(toks, language = "english")
      fuera <- which(!(provisional %in% vocabulario) & !intocable)


      piezas <- as.list(toks)

      for (p in fuera) {
        partes <- .segmentar(toks[p], superficie, superficie_df)
        if (!is.null(partes)) {
          registro$cor <- c(registro$cor,
                            stats::setNames(paste(partes, collapse = " "), toks[p]))
          piezas[[p]] <- partes
          next
        }
        cand <- .mas_cercano(toks[p], superficie, frec = superficie_df)
        if (!is.na(cand)) {
          registro$cor <- c(registro$cor, stats::setNames(cand, toks[p]))
          piezas[[p]] <- cand
        }
      }
      toks <- unlist(piezas, use.names = FALSE)
    }

    if (solo_superficie) return(toks)


    toks <- SnowballC::wordStem(toks, language = "english")
    toks <- toks[nchar(toks) >= 3 | toks %in% ACRONIMOS_CORTOS]
    if (length(toks) == 0) return(character(0))

    # Respaldo para indices antiguos sin diccionario de superficie.
    if (corregir && is.null(superficie)) {
      vocab_uni <- vocabulario[!grepl("_", vocabulario, fixed = TRUE)]
      fuera <- which(!(toks %in% vocabulario))
      for (p in fuera) {
        cand <- .mas_cercano(toks[p], vocab_uni)
        if (!is.na(cand)) {
          registro$cor <- c(registro$cor, stats::setNames(cand, toks[p]))
          toks[p] <- cand
        }
      }
    }

    if (bigramas && length(toks) > 1) {
      toks <- c(toks, paste(toks[-length(toks)], toks[-1], sep = "_"))
    }
    toks
  })

  attr(res, "correcciones") <- registro$cor
  res
}


construir_texto <- function(titulo, abstract, keywords = NA,
                            peso_titulo = 3L, peso_keywords = 2L) {
  limpio <- function(x) {
    x <- as.character(x)
    x[is.na(x) | x %in% c("NA", "SIN_KEYWORDS")] <- ""
    x
  }
  t <- limpio(titulo); a <- limpio(abstract); k <- limpio(keywords)
  paste(strrep(paste0(t, " "), peso_titulo), a,
        strrep(paste0(k, " "), peso_keywords))
}

# ==============================================================================
# CARGA DEL INDICE
# ==============================================================================

#' Carga search_index.rds una sola vez por sesion de R.
cargar_indice <- function(ruta = RUTA_INDICE) {
  if (!file.exists(ruta)) return(NULL)
  tryCatch(readRDS(ruta), error = function(e) NULL)
}

# ==============================================================================
# VECTORIZACION DE LA CONSULTA
# ==============================================================================

.consulta_a_terminos <- function(consulta, idx) {
  tk   <- tokenizar(consulta, idx = idx)
  toks <- tk[[1]]
  cor  <- attr(tk, "correcciones")
  if (length(toks) == 0) return(NULL)


  es_bi <- grepl("_", toks, fixed = TRUE)

  j <- match(toks, idx$vocabulario)
  ok <- !is.na(j)
  j  <- j[ok]
  if (length(j) == 0) return(NULL)

  tab <- table(j)
  list(j = as.integer(names(tab)), tf = as.integer(tab),
       n_tokens = length(toks), n_reconocidos = length(j),
       n_unigramas = sum(!es_bi), n_bigramas = sum(es_bi),
       uni_ok = sum(ok & !es_bi), bi_ok = sum(ok & es_bi),
       correcciones = cor)
}

# ==============================================================================
# ESTRATEGIA 1 -- BM25 (recuperacion lexica, espacio disperso completo)
# ==============================================================================

.puntajes_bm25 <- function(consulta, idx) {
  q <- .consulta_a_terminos(consulta, idx)
  if (is.null(q)) return(NULL)
  sub <- idx$bm25[, q$j, drop = FALSE]
  as.numeric(sub %*% q$tf)
}

# ==============================================================================
# ESTRATEGIA 2 -- TF-IDF + COSENO (lexica, espacio disperso SIN reducir)
# ==============================================================================

.vector_consulta_tfidf <- function(q, idx) {
  w <- (1 + log(q$tf)) * idx$idf[q$j]
  nw <- sqrt(sum(w^2))
  if (!is.finite(nw) || nw == 0) return(NULL)
  w / nw
}

.puntajes_tfidf <- function(consulta, idx) {
  if (is.null(idx$tfidf)) return(NULL)
  q <- .consulta_a_terminos(consulta, idx)
  if (is.null(q)) return(NULL)
  w <- .vector_consulta_tfidf(q, idx)
  if (is.null(w)) return(NULL)
  # Filas de idx$tfidf ya normalizadas en L2 -> el producto es el coseno.
  as.numeric(idx$tfidf[, q$j, drop = FALSE] %*% w)
}

# ==============================================================================
# ESTRATEGIA 2 -- LSA (recuperacion semantica en el espacio reducido)
# ==============================================================================

.puntajes_lsa <- function(consulta, idx) {
  q <- .consulta_a_terminos(consulta, idx)
  if (is.null(q)) return(NULL)

  # mismo vector tf-idf que usa la estrategia sin reducir
  w <- .vector_consulta_tfidf(q, idx)
  if (is.null(w)) return(NULL)

  # proyeccion al espacio latente: q' V
  ql <- as.numeric(crossprod(idx$V[q$j, , drop = FALSE], w))
  nq <- sqrt(sum(ql^2))
  if (!is.finite(nq) || nq == 0) return(NULL)
  ql <- ql / nq

  as.numeric(idx$docs_lsa %*% ql)   # filas ya normalizadas -> coseno
}

# ==============================================================================
# ESTRATEGIA 3 -- HIBRIDA (fusion de rangos)
# ==============================================================================

.puntajes_hibrido <- function(consulta, idx, k_rrf = 60) {
  a <- .puntajes_bm25(consulta, idx)
  b <- .puntajes_lsa(consulta, idx)
  if (is.null(a) && is.null(b)) return(NULL)

  rrf <- numeric(nrow(idx$meta))
  for (p in list(a, b)) {
    if (is.null(p)) next
    r <- rank(-p, ties.method = "min")
    rrf <- rrf + 1 / (k_rrf + r)
  }
  rrf
}

# ==============================================================================
# FRAGMENTO RELEVANTE
# ==============================================================================

.fragmento <- function(abstract, stems_consulta, max_chars = 300) {
  if (is.na(abstract) || abstract == "NA" || nchar(abstract) < 10) {
    return("Sin resumen disponible.")
  }
  oraciones <- unlist(strsplit(abstract, "(?<=[.!?])\\s+", perl = TRUE))
  oraciones <- oraciones[nchar(oraciones) > 25]
  if (length(oraciones) == 0) return(substr(abstract, 1, max_chars))

  puntos <- vapply(tokenizar(oraciones, bigramas = FALSE),
                   function(tk) sum(tk %in% stems_consulta), numeric(1))

  mejor <- if (max(puntos) > 0) which.max(puntos) else 1L
  frag  <- oraciones[mejor]
  if (nchar(frag) > max_chars) frag <- paste0(substr(frag, 1, max_chars), "...")
  if (mejor > 1L) frag <- paste0("[...] ", frag)
  frag
}

# ==============================================================================
# API PUBLICA DEL BUSCADOR
# ==============================================================================

#'
#' @param consulta   texto libre en ingles o espanol
#' @param idx        indice cargado con cargar_indice()
#' @param estrategia "bm25" (lexica), "lsa" (semantica reducida) o "hibrido"
#' @param n          numero de resultados
#'
#' @return data.frame con posicion, titulo, autores, fecha, tema, doi, url,
#'         puntaje, puntaje_norm y fragmento. Cero filas si no hay coincidencias.
buscar_articulos <- function(consulta, idx,
                             estrategia = c("bm25", "tfidf", "lsa", "hibrido"),
                             n = 10) {

  estrategia <- match.arg(estrategia)
  vacio <- data.frame()

  if (is.null(idx) || is.null(consulta) || !nzchar(trimws(consulta))) return(vacio)

  puntajes <- switch(estrategia,
                     bm25    = .puntajes_bm25(consulta, idx),
                     tfidf   = .puntajes_tfidf(consulta, idx),
                     lsa     = .puntajes_lsa(consulta, idx),
                     hibrido = .puntajes_hibrido(consulta, idx))

  if (is.null(puntajes) || all(!is.finite(puntajes)) || max(puntajes, na.rm = TRUE) <= 0) {
    return(vacio)
  }

  meta <- idx$meta


  orden <- order(-puntajes,
                 -ifelse(is.na(meta$citations), 0, meta$citations),
                 meta$publication_date,
                 decreasing = c(FALSE, FALSE, TRUE),
                 method = "radix")

  orden <- orden[puntajes[orden] > 0]
  if (length(orden) == 0) return(vacio)
  orden <- head(orden, n)

  stems <- tokenizar(consulta, idx = idx)[[1]]

  res <- data.frame(
    posicion         = seq_along(orden),
    paper_id         = meta$paper_id[orden],
    titulo           = meta$title[orden],
    autores          = meta$authors_raw[orden],
    fecha            = meta$publication_date[orden],
    tema             = meta$topic_label[orden],
    doi              = meta$doi[orden],
    url              = meta$url[orden],
    citas            = meta$citations[orden],
    puntaje          = round(puntajes[orden], 5),
    stringsAsFactors = FALSE
  )

  rango <- range(res$puntaje)
  res$puntaje_norm <- if (diff(rango) > 0) {
    (res$puntaje - rango[1]) / diff(rango)
  } else rep(1, nrow(res))

  res$fragmento <- vapply(meta$abstract[orden],
                          function(a) .fragmento(a, stems), character(1))

  res$estrategia <- estrategia
  rownames(res) <- NULL
  res
}

# ------------------------------------------------------------------------------
# CARGA UNICA DEL INDICE
# ------------------------------------------------------------------------------

INDICE_RI <- cargar_indice()

diagnosticar_consulta <- function(consulta, idx) {
  if (is.null(idx)) return(list(ok = FALSE, mensaje = "Indice no cargado."))
  toks <- tokenizar(consulta)[[1]]
  if (length(toks) == 0) {
    return(list(ok = FALSE, mensaje = "La consulta no dejo terminos utilizables."))
  }
  q <- .consulta_a_terminos(consulta, idx)
  if (is.null(q)) {
    return(list(ok = FALSE, n_tokens = length(toks), n_reconocidos = 0,
                correcciones = character(0),
                mensaje = "Ningun termino de la consulta esta en el vocabulario del corpus."))
  }

  msg <- sprintf("%d de %d palabras reconocidas", q$uni_ok, q$n_unigramas)
  if (q$n_bigramas > 0) {
    msg <- paste0(msg, sprintf(" (+%d de %d bigramas)", q$bi_ok, q$n_bigramas))
  }
  msg <- paste0(msg, ".")
  if (length(q$correcciones) > 0) {
    msg <- paste0(msg, "  Se corrigio: ",
                  paste(sprintf("%s -> %s", names(q$correcciones),
                                unname(q$correcciones)), collapse = ", "), ".")
  }

  list(ok = TRUE, n_tokens = q$n_tokens, n_reconocidos = q$n_reconocidos,
       correcciones = q$correcciones, mensaje = msg)
}
