
CONSULTAS_EVALUACION <- list(
  list(id = "q1_exacta", tipo = "exacta",
       texto = "climate change macroeconomic modeling",
       relevantes = c("6")),
  list(id = "q2_sinonimos", tipo = "sinonimos",
       texto = "job opportunities for workers in poor countries",
       relevantes = c("30", "5")),
  list(id = "q3_general", tipo = "general",
       texto = "economic policy",
       relevantes = c("8", "10", "15", "19", "22")),
  list(id = "q4_especifica", tipo = "especifica",
       texto = "tax incidence anomalies",
       relevantes = c("25")),
  list(id = "q5_fallo_parcial", tipo = "fallo_parcial",
       texto = "artificial intelligence and deep learning models in economics",
       relevantes = c("6"))
)

precision_at_k <- function(ranking_doc_ids, relevantes, k = 5) {
  top_k <- utils::head(ranking_doc_ids, k)
  if (length(top_k) == 0) return(0)
  sum(top_k %in% relevantes) / k
}

reciprocal_rank <- function(ranking_doc_ids, relevantes) {
  pos <- which(ranking_doc_ids %in% relevantes)
  if (length(pos) == 0) return(0)
  1 / min(pos)
}

recall_at_k <- function(ranking_doc_ids, relevantes, k = 5) {
  top_k <- utils::head(ranking_doc_ids, k)
  if (length(relevantes) == 0) return(NA_real_)
  sum(top_k %in% relevantes) / length(relevantes)
}

ndcg_at_k <- function(ranking_doc_ids, relevantes, k = 5) {
  top_k <- utils::head(ranking_doc_ids, k)
  rel <- as.integer(top_k %in% relevantes)
  if (length(rel) == 0) return(0)
  dcg <- sum(rel / log2(seq_along(rel) + 1))
  ideal_rel <- c(rep(1, min(length(relevantes), k)),
                 rep(0, max(0, k - length(relevantes))))
  idcg <- sum(ideal_rel / log2(seq_along(ideal_rel) + 1))
  if (idcg == 0) return(0)
  dcg / idcg
}

evaluar_consulta <- function(consulta, cache_ir, k = 5) {
  r_tfidf <- buscar_tfidf(consulta$texto, cache_ir, top_n = max(k, nrow(cache_ir$meta)))
  r_lsa   <- buscar_lsa(consulta$texto, cache_ir, top_n = max(k, nrow(cache_ir$meta)))

  tibble::tibble(
    consulta_id = consulta$id,
    tipo        = consulta$tipo,
    texto       = consulta$texto,
    metodo      = c("tfidf", "lsa"),
    precision_at_k = c(precision_at_k(r_tfidf$doc_id, consulta$relevantes, k),
                        precision_at_k(r_lsa$doc_id,   consulta$relevantes, k)),
    mrr            = c(reciprocal_rank(r_tfidf$doc_id, consulta$relevantes),
                        reciprocal_rank(r_lsa$doc_id,   consulta$relevantes)),
    recall_at_k    = c(recall_at_k(r_tfidf$doc_id, consulta$relevantes, k),
                        recall_at_k(r_lsa$doc_id,   consulta$relevantes, k)),
    ndcg_at_k      = c(ndcg_at_k(r_tfidf$doc_id, consulta$relevantes, k),
                        ndcg_at_k(r_lsa$doc_id,   consulta$relevantes, k))
  )
}

evaluar_todas <- function(cache_ir, k = 5) {
  purrr::map_dfr(CONSULTAS_EVALUACION, evaluar_consulta, cache_ir = cache_ir, k = k)
}

