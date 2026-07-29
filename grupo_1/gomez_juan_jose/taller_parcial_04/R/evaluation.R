# =============================================================================
# evaluation.R  --  Taller 4 · Minería de Datos (2016325) · UNAL
# Métricas de recuperación: Precision@k, MRR, nDCG@k.
#
# Los juicios de relevancia (eval/juicios_relevancia.csv) se construyeron por
# POOLING al estilo TREC: para cada consulta se tomó la unión de los 10
# primeros resultados de las tres estrategias y se juzgó manualmente cada
# documento del pool leyendo título y abstract. Los documentos fuera del pool
# se asumen NO relevantes (supuesto estándar de pool completeness).
#
# Consecuencia metodológica: Precision@5, MRR y nDCG@10 son insesgados bajo
# este supuesto, pero Recall NO lo es (el denominador solo cuenta relevantes
# descubiertos por alguna de las estrategias). Por eso se reporta como
# "Recall@10 sobre el pool" y no como recall absoluto.
# =============================================================================

#' Precision@k
precision_at_k <- function(ids_ranking, ids_relevantes, k = 5L) {
  top <- utils::head(ids_ranking, k)
  if (length(top) == 0) return(0)
  sum(top %in% ids_relevantes) / k
}

#' Mean Reciprocal Rank (sobre una sola consulta: reciprocal rank)
reciprocal_rank <- function(ids_ranking, ids_relevantes) {
  hit <- which(ids_ranking %in% ids_relevantes)
  if (length(hit) == 0) return(0)
  1 / hit[1]
}

#' nDCG@k con ganancias binarias.
#' DCG = sum_i  rel_i / log2(i+1);  IDCG = DCG del ranking ideal.
ndcg_at_k <- function(ids_ranking, ids_relevantes, k = 10L) {
  top <- utils::head(ids_ranking, k)
  rel <- as.numeric(top %in% ids_relevantes)
  if (sum(rel) == 0) return(0)
  dcg <- sum(rel / log2(seq_along(rel) + 1))
  n_ideal <- min(length(ids_relevantes), k)
  idcg <- sum(1 / log2(seq_len(n_ideal) + 1))
  dcg / idcg
}

#' Recall@k relativo al pool juzgado.
recall_pool_at_k <- function(ids_ranking, ids_relevantes, k = 10L) {
  if (length(ids_relevantes) == 0) return(NA_real_)
  sum(utils::head(ids_ranking, k) %in% ids_relevantes) / length(ids_relevantes)
}

#' Evalúa una estrategia sobre todas las consultas.
evaluar_estrategia <- function(idx, consultas, qrels, estrategia, k_rank = 10L) {
  do.call(rbind, lapply(names(consultas), function(cid) {
    r <- ejecutar_busqueda(idx, consultas[[cid]], estrategia, n = k_rank)
    ids <- r$resultados$paper_id
    rel <- qrels$paper_id[qrels$consulta_id == cid & qrels$relevante == 1]
    data.frame(
      consulta_id = cid, estrategia = estrategia,
      P5   = precision_at_k(ids, rel, 5L),
      P10  = precision_at_k(ids, rel, 10L),
      RR   = reciprocal_rank(ids, rel),
      nDCG10 = ndcg_at_k(ids, rel, 10L),
      Rpool10 = recall_pool_at_k(ids, rel, 10L),
      n_rel_pool = length(rel),
      stringsAsFactors = FALSE
    )
  }))
}
