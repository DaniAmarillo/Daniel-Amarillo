
construir_matriz_tfidf <- function(doc_term_df) {
  t0 <- Sys.time()

  conteos <- doc_term_df %>%
    dplyr::count(.data$doc_id, .data$term, name = "n")

  conteos_tfidf <- conteos %>%
    tidytext::bind_tf_idf(term, doc_id, n)

  idf_map <- conteos_tfidf %>%
    dplyr::distinct(.data$term, .data$idf)

  dtm <- tidytext::cast_sparse(conteos_tfidf, doc_id, term, tf_idf)

  t1 <- Sys.time()

  n_docs  <- nrow(dtm)
  n_terms <- ncol(dtm)
  n_no_cero <- Matrix::nnzero(dtm)

  stats <- list(
    n_docs                  = n_docs,
    n_terms_vocabulario     = n_terms,
    dimensiones             = paste0(n_docs, " documentos x ", n_terms, " términos"),
    porcentaje_dispersion   = round(100 * (1 - n_no_cero / (n_docs * n_terms)), 2),
    memoria_mb              = round(as.numeric(utils::object.size(dtm)) / 1024^2, 4),
    tiempo_construccion_seg = round(as.numeric(difftime(t1, t0, units = "secs")), 4)
  )

  list(
    dtm     = dtm,
    idf     = idf_map,
    vocab   = colnames(dtm),
    doc_ids = rownames(dtm),
    stats   = stats
  )
}

