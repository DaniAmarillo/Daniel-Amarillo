# ============================================================
# search_engine.R — Taller 4 · Minería de Datos (2016325)
# Funciones de recuperación y ranking. Las carga app.R y el
# documento reproducible. El preprocesamiento DEBE ser idéntico
# al de build_index.R para que la consulta caiga en el mismo
# espacio vectorial que los documentos.
# ============================================================

suppressPackageStartupMessages({
  library(Matrix)
  library(text2vec)
  library(SnowballC)
})

# ── Preprocesamiento (idéntico a build_index.R) ─────────────
.prep_txt <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z -]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}
.tok_txt <- function(x, en_stop) {
  toks <- text2vec::word_tokenizer(x)
  lapply(toks, function(w) {
    w <- w[nchar(w) >= 3]
    w <- w[!w %in% en_stop]
    SnowballC::wordStem(w, language = "en")
  })
}

# ── Vectorización de la consulta ────────────────────────────
# Devuelve el vector TF-IDF disperso (1 × V) de la consulta,
# usando el vocabulario e IDF aprendidos sobre el corpus.
vectorizar_consulta <- function(query, IDX) {
  it_q <- itoken(query,
                 preprocessor = .prep_txt,
                 tokenizer    = function(x) .tok_txt(x, IDX$en_stop),
                 progressbar  = FALSE)
  dtm_q <- create_dtm(it_q, IDX$vectorizer)
  transform(dtm_q, IDX$tfidf)          # 1 × V, L2-normalizado
}

.norm_l2_vec <- function(v) {
  d <- sqrt(sum(v^2))
  if (d < 1e-12) v else v / d
}

# ── Estrategia A: léxica (TF-IDF + similitud coseno) ────────
buscar_lexico <- function(query, IDX, topk = 10) {
  q_tfidf <- vectorizar_consulta(query, IDX)                 # 1 × V (L2)
  sims <- as.numeric(q_tfidf %*% Matrix::t(IDX$dtm_tfidf))   # coseno (ambos L2)
  .armar_resultados(sims, IDX, topk, metodo = "Léxica (TF-IDF)")
}

# ── Estrategia B: semántica reducida (LSA + coseno) ─────────
buscar_semantico <- function(query, IDX, topk = 10) {
  q_tfidf <- vectorizar_consulta(query, IDX)                 # 1 × V (L2)
  q_lsa   <- as.numeric(as.matrix(q_tfidf) %*% IDX$svd_v)    # proyección a R^k
  q_lsa   <- .norm_l2_vec(q_lsa)
  sims    <- as.numeric(IDX$doc_lsa %*% q_lsa)               # coseno en R^k
  .armar_resultados(sims, IDX, topk, metodo = "Semántica (LSA)")
}

# ── Ranking y ensamblado del resultado ──────────────────────
# Ordena por puntaje descendente. Empates: se rompen por número de
# citas (criterio secundario) y luego por fecha más reciente.
.armar_resultados <- function(sims, IDX, topk, metodo) {
  m <- IDX$meta
  citas <- ifelse(is.na(m$citations), 0, m$citations)
  fecha <- as.numeric(as.Date(m$publication_date))
  fecha[is.na(fecha)] <- 0
  ord <- order(-sims, -citas, -fecha)
  ord <- ord[seq_len(min(topk, length(ord)))]

  data.frame(
    ranking          = seq_along(ord),
    metodo           = metodo,
    score            = round(sims[ord], 4),
    paper_id         = m$paper_id[ord],
    title            = m$title[ord],
    authors_raw      = m$authors_raw[ord],
    publication_date = m$publication_date[ord],
    topic_label      = m$topic_label[ord],
    doi              = m$doi[ord],
    url              = m$url[ord],
    citations        = m$citations[ord],
    fragmento        = m$fragmento[ord],
    stringsAsFactors = FALSE
  )
}

# ── Interfaz unificada ──────────────────────────────────────
buscar <- function(query, IDX, estrategia = c("lexico","semantico"), topk = 10) {
  estrategia <- match.arg(estrategia)
  if (is.null(query) || nchar(trimws(query)) == 0) return(NULL)
  if (estrategia == "lexico") buscar_lexico(query, IDX, topk)
  else                        buscar_semantico(query, IDX, topk)
}

# ── Precision@k (evaluación) ────────────────────────────────
# 'relevantes' = vector de paper_id considerados relevantes para la consulta.
precision_at_k <- function(resultados, relevantes, k = 5) {
  top <- head(resultados$paper_id, k)
  mean(top %in% relevantes)
}
