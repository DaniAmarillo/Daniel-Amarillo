
construir_vector_consulta <- function(texto_consulta, vocab, idf_map) {
  vec <- stats::setNames(numeric(length(vocab)), vocab)

  q_df <- tibble::tibble(doc_id = "query", texto = texto_consulta)
  terminos <- preprocesar_corpus(q_df)$term

  if (length(terminos) == 0) return(vec)

  conteo <- table(terminos)
  tf <- as.numeric(conteo) / sum(conteo)
  names(tf) <- names(conteo)

  comunes <- intersect(names(tf), vocab)
  if (length(comunes) == 0) return(vec)

  idf_vals <- idf_map$idf[match(comunes, idf_map$term)]
  vec[comunes] <- tf[comunes] * idf_vals
  vec
}

buscar_tfidf <- function(texto_consulta, cache_ir, top_n = 10) {
  vocab <- cache_ir$tfidf$vocab
  q_vec <- construir_vector_consulta(texto_consulta, vocab, cache_ir$tfidf$idf)

  norma_q <- sqrt(sum(q_vec^2))
  if (norma_q == 0) {
    return(tibble::tibble(doc_id = character(0), score = numeric(0)))
  }

  dtm <- cache_ir$tfidf$dtm
  q_sparse <- Matrix::Matrix(q_vec, nrow = 1, sparse = TRUE)

  producto_punto <- as.numeric(dtm %*% Matrix::t(q_sparse))
  norma_docs <- sqrt(Matrix::rowSums(dtm^2))

  scores <- ifelse(norma_docs == 0, 0, producto_punto / (norma_docs * norma_q))

  rankear(scores, rownames(dtm), cache_ir$meta, top_n)
}

buscar_lsa <- function(texto_consulta, cache_ir, top_n = 10) {
  vocab <- cache_ir$tfidf$vocab
  q_tfidf <- construir_vector_consulta(texto_consulta, vocab, cache_ir$tfidf$idf)

  if (sum(q_tfidf^2) == 0) {
    return(tibble::tibble(doc_id = character(0), score = numeric(0)))
  }

  modelo <- cache_ir$lsa
  q_reducida <- proyectar_consulta_lsa(q_tfidf, modelo)

  U <- modelo$doc_vectores
  norma_q <- sqrt(sum(q_reducida^2))
  if (norma_q == 0) {
    return(tibble::tibble(doc_id = character(0), score = numeric(0)))
  }
  norma_docs <- sqrt(rowSums(U^2))

  producto <- as.numeric(U %*% q_reducida)
  scores <- ifelse(norma_docs == 0, 0, producto / (norma_docs * norma_q))
  names(scores) <- rownames(U)

  rankear(scores, rownames(U), cache_ir$meta, top_n)
}

