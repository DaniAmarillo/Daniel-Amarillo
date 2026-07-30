
RUTA_CACHE_IR_DEFECTO <- "cache/ir_cache.rds"
VERSION_ESQUEMA_CACHE_IR <- 2L

precomputar_ir <- function(con) {
  res_corpus <- construir_corpus(con)
  corpus <- res_corpus$corpus

  if (nrow(corpus) == 0) {
    stop("No hay documentos en `papers` para construir el índice de IR.")
  }

  doc_term <- preprocesar_corpus(corpus %>% dplyr::select(doc_id, texto))
  tfidf    <- construir_matriz_tfidf(doc_term)
  lsa      <- construir_lsa(tfidf$dtm)

  for (col in c("autores", "doi", "url")) {
    if (!col %in% names(corpus)) corpus[[col]] <- NA_character_
  }

  meta <- corpus %>%
    dplyr::select(doc_id, titulo, resumen, categoria, fecha_publicacion,
                   autores, doi, url)

  list(
    tfidf            = tfidf,
    lsa              = lsa,
    meta             = meta,
    corpus_stats     = res_corpus$stats,
    fecha_generacion = Sys.time(),
    version_esquema  = VERSION_ESQUEMA_CACHE_IR
  )
}

guardar_cache_ir <- function(cache_ir, ruta = RUTA_CACHE_IR_DEFECTO) {
  dir.create(dirname(ruta), showWarnings = FALSE, recursive = TRUE)
  saveRDS(cache_ir, ruta)
  invisible(ruta)
}

cargar_cache_ir <- function(ruta = RUTA_CACHE_IR_DEFECTO) {
  if (!file.exists(ruta)) return(NULL)
  cache_ir <- readRDS(ruta)

  version_guardada <- cache_ir$version_esquema %||% 1L
  if (!identical(version_guardada, VERSION_ESQUEMA_CACHE_IR)) {
    message(
      "cache/ir_cache.rds tiene un esquema desactualizado (version ",
      version_guardada, " vs ", VERSION_ESQUEMA_CACHE_IR,
      " esperada); se reconstruira el indice de IR."
    )
    return(NULL)
  }
  cache_ir
}

obtener_cache_ir <- function(con = NULL, ruta = RUTA_CACHE_IR_DEFECTO, forzar = FALSE) {
  if (!forzar) {
    cache_ir <- cargar_cache_ir(ruta)
    if (!is.null(cache_ir)) return(cache_ir)
  }

  cerrar_con <- FALSE
  if (is.null(con)) {
    con <- conectar_db()
    cerrar_con <- TRUE
  }
  cache_ir <- precomputar_ir(con)
  if (cerrar_con) dbDisconnect(con)

  guardar_cache_ir(cache_ir, ruta)
  cache_ir
}

