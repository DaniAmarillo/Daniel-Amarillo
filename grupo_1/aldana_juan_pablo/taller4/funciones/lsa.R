
construir_lsa <- function(dtm, var_explicada_min = 0.8, k_max = 15) {
  t0 <- Sys.time()

  k_tope <- min(k_max, nrow(dtm) - 1, ncol(dtm) - 1)
  if (k_tope < 2) k_tope <- max(2, min(nrow(dtm), ncol(dtm)) - 1)

  modelo_full <- irlba::irlba(dtm, nv = k_tope)

  frob_sq <- sum(dtm@x^2)
  var_exp_acum <- cumsum(modelo_full$d^2) / frob_sq

  k <- which(var_exp_acum >= var_explicada_min)[1]
  if (is.na(k)) k <- k_tope
  k <- max(k, 2)

  u <- modelo_full$u[, 1:k, drop = FALSE]
  d <- modelo_full$d[1:k]
  v <- modelo_full$v[, 1:k, drop = FALSE]

  doc_vectores <- u
  rownames(doc_vectores) <- rownames(dtm)

  t1 <- Sys.time()

  stats <- list(
    dimension_original          = ncol(dtm),
    dimension_reducida_k        = k,
    k_tope_considerado          = k_tope,
    criterio_seleccion_k        = paste0(
      "menor k con varianza acumulada (Frobenius) >= ", var_explicada_min * 100, "%"),
    varianza_conservada_pct     = round(100 * var_exp_acum[k], 2),
    tiempo_entrenamiento_seg    = round(as.numeric(difftime(t1, t0, units = "secs")), 4),
    memoria_mb                  = round(
      as.numeric(utils::object.size(list(u = u, d = d, v = v))) / 1024^2, 4)
  )

  list(
    doc_vectores = doc_vectores,
    d            = d,
    v            = v,
    k            = k,
    vocab        = colnames(dtm),
    stats        = stats
  )
}

proyectar_consulta_lsa <- function(vector_tfidf_query, modelo_lsa) {
  v <- modelo_lsa$v
  d <- modelo_lsa$d
  as.numeric(vector_tfidf_query %*% v %*% diag(1 / d, length(d), length(d)))
}

