

suppressPackageStartupMessages({
  library(tm)
  library(SnowballC)
  library(Matrix)
  library(dplyr)
  library(htmltools)
})

INDEX_DIR <- "index"

# ── Carga de objetos precalculados (caché) ──────────────────────────────────
.cargar_rds_requerido <- function(nombre) {
  ruta <- file.path(INDEX_DIR, nombre)
  if (!file.exists(ruta)) {
    stop(sprintf(
      "Falta el archivo de índice '%s' en la carpeta '%s/'. Ejecuta build_search_index.R para generarlo antes de iniciar la app.",
      nombre, INDEX_DIR
    ))
  }
  readRDS(ruta)
}


.cargar_rds_opcional <- function(nombre) {
  ruta <- file.path(INDEX_DIR, nombre)
  if (!file.exists(ruta)) {
    message(sprintf(
      "Aviso: no se encontró '%s' en '%s/'. Esa estrategia quedará deshabilitada hasta que ejecutes build_search_index.R.",
      nombre, INDEX_DIR
    ))
    return(NULL)
  }
  readRDS(ruta)
}

.meta       <- .cargar_rds_requerido("meta.rds")
.tfidf_mat  <- .cargar_rds_requerido("tfidf_mat.rds")
.idf_vector <- .cargar_rds_requerido("idf_vector.rds")
.lsa        <- .cargar_rds_requerido("lsa_model.rds")
.bm25       <- .cargar_rds_opcional("bm25_model.rds")
.stats      <- .cargar_rds_requerido("stats.rds")

source("es_en_dict.R")  # dic_es_en

# Normas L2 de cada documento, precalculadas para no recomputarlas en cada
# búsqueda (coseno = producto punto / (norma_doc * norma_query))
.tfidf_norms <- sqrt(Matrix::rowSums(.tfidf_mat^2))
.lsa_norms   <- sqrt(rowSums(.lsa$docs_lsa^2))


# ── Traducción básica español -> inglés ─────────────────────────────────────
quitar_tildes <- function(x) {
  out <- tryCatch(iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT"), error = function(e) rep(NA_character_, length(x)))
  malos <- is.null(out) || anyNA(out)
  if (malos) {
    mapa <- c("á"="a","é"="e","í"="i","ó"="o","ú"="u","ñ"="n","ü"="u",
              "Á"="A","É"="E","Í"="I","Ó"="O","Ú"="U","Ñ"="N","Ü"="U")
    for (k in names(mapa)) x <- gsub(k, mapa[k], x, fixed = TRUE)
    out <- x
  }
  out
}

# Diccionario normalizado (sin tildes, en minúscula), frases largas primero
.dic_norm <- setNames(unname(dic_es_en), quitar_tildes(tolower(names(dic_es_en))))
.dic_norm <- .dic_norm[order(-nchar(names(.dic_norm)))]

traducir_query_basico <- function(texto) {
  t <- quitar_tildes(tolower(texto))
  for (es in names(.dic_norm)) {
    patron <- paste0("\\b", es, "\\b")
    t <- gsub(patron, .dic_norm[[es]], t, ignore.case = TRUE, perl = TRUE)
  }
  t
}

# ── Preprocesamiento de la consulta ─────────────────────────────────────────

preprocesar_query <- function(texto) {
  texto  <- traducir_query_basico(texto)
  corpus <- VCorpus(VectorSource(texto))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
  corpus <- tm_map(corpus, stemDocument, language = "english")
  corpus <- tm_map(corpus, stripWhitespace)
  tokens <- unlist(strsplit(trimws(content(corpus[[1]])), "\\s+"))
  tokens[tokens != ""]
}

# Stems finales de una consulta (usados también para resaltar resultados)
obtener_stems_query <- function(texto) {
  unique(preprocesar_query(texto))
}

# ── Resaltado de términos coincidentes en los resultados ───────────────────

resaltar_terminos <- function(texto, stems) {
  if (is.null(texto) || is.na(texto) || texto == "") return(texto)
  stems <- unique(stems[nchar(stems) >= 3])
  texto_esc <- htmltools::htmlEscape(texto)
  if (length(stems) == 0) return(texto_esc)
  alternativas <- paste0(stems, "[a-zA-Z]*")
  patron <- paste0("\\b(", paste(alternativas, collapse = "|"), ")\\b")
  gsub(patron, "<mark class='hl'>\\1</mark>", texto_esc, ignore.case = TRUE, perl = TRUE)
}

# Vector TF-IDF de la consulta, en el vocabulario del corpus (términos fuera
# del vocabulario se ignoran, igual que en cualquier sistema de RI clásico)
vectorizar_query_tfidf <- function(texto) {
  tokens <- preprocesar_query(texto)
  vocab  <- names(.idf_vector)
  tokens <- tokens[tokens %in% vocab]
  
  vec <- rep(0, length(vocab))
  names(vec) <- vocab
  if (length(tokens) == 0) return(vec)
  
  tf <- table(tokens)
  vec[names(tf)] <- as.numeric(tf) * .idf_vector[names(tf)]
  
  norm <- sqrt(sum(vec^2))
  if (norm > 0) vec <- vec / norm
  vec
}

# ── Estrategia 1: recuperación léxica — TF-IDF + similitud coseno ──────────
buscar_tfidf <- function(query, top_n = 10) {
  qvec <- vectorizar_query_tfidf(query)
  if (sum(qvec) == 0) return(tibble())
  qsparse <- Matrix(qvec, nrow = 1, sparse = TRUE)
  
  scores <- as.numeric(.tfidf_mat %*% t(qsparse))
  qnorm  <- sqrt(sum(qvec^2))
  denom  <- .tfidf_norms * qnorm
  sim    <- ifelse(denom > 0, scores / denom, 0)
  
  .ensamblar_resultado(sim, top_n, estrategia = "TF-IDF + coseno (léxica)", query = query)
}

# ── Estrategia 2: recuperación léxica — BM25 ────────────────────────────────
buscar_bm25 <- function(query, top_n = 10) {
  if (is.null(.bm25)) return(NULL)  # bm25_model.rds no disponible
  
  tokens <- preprocesar_query(query)
  vocab  <- names(.bm25$idf_bm25)
  tokens <- unique(tokens[tokens %in% vocab])
  if (length(tokens) == 0) return(tibble())
  
  k1 <- .bm25$k1; b <- .bm25$b
  doc_len <- .bm25$doc_len
  avgdl   <- .bm25$avgdl
  
  scores <- numeric(length(doc_len))
  for (t in tokens) {
    tf_t   <- .bm25$tf_mat[, t]
    idf_t  <- .bm25$idf_bm25[[t]]
    denom  <- tf_t + k1 * (1 - b + b * doc_len / avgdl)
    scores <- scores + idf_t * (tf_t * (k1 + 1)) / ifelse(denom == 0, 1, denom)
  }
  
  .ensamblar_resultado(scores, top_n, estrategia = "BM25 (léxica)", query = query)
}

# ── Estrategia 3: recuperación semántica — LSA + similitud coseno ──────────
buscar_lsa <- function(query, top_n = 10) {
  qvec <- vectorizar_query_tfidf(query)
  if (sum(qvec) == 0) return(tibble())
  # Proyección de la consulta al espacio LSA reducido:
  # q_lsa = q_tfidf %*% V   (misma proyección que usan los documentos: U*D)
  q_lsa <- matrix(qvec, nrow = 1) %*% .lsa$V
  
  scores <- as.numeric(.lsa$docs_lsa %*% t(q_lsa))
  qnorm  <- sqrt(sum(q_lsa^2))
  denom  <- .lsa_norms * qnorm
  sim    <- ifelse(denom > 0, scores / denom, 0)
  
  .ensamblar_resultado(sim, top_n, estrategia = "LSA + coseno (semántica)", query = query)
}

# ── Estrategia 4: híbrida — TF-IDF (léxica) + LSA (semántica) ──────────────

buscar_hibrida <- function(query, top_n = 10, alpha = 0.5) {
  qvec <- vectorizar_query_tfidf(query)
  if (sum(qvec) == 0) return(tibble())
  
  qsparse <- Matrix(qvec, nrow = 1, sparse = TRUE)
  sim_tfidf <- as.numeric(.tfidf_mat %*% t(qsparse)) /
    ifelse(.tfidf_norms * sqrt(sum(qvec^2)) > 0, .tfidf_norms * sqrt(sum(qvec^2)), 1)
  
  q_lsa <- matrix(qvec, nrow = 1) %*% .lsa$V
  sim_lsa <- as.numeric(.lsa$docs_lsa %*% t(q_lsa)) /
    ifelse(.lsa_norms * sqrt(sum(q_lsa^2)) > 0, .lsa_norms * sqrt(sum(q_lsa^2)), 1)
  
  norm01 <- function(x) {
    r <- range(x)
    if (diff(r) == 0) return(rep(0, length(x)))
    (x - r[1]) / diff(r)
  }
  
  score <- alpha * norm01(sim_tfidf) + (1 - alpha) * norm01(sim_lsa)
  .ensamblar_resultado(score, top_n, estrategia = "Híbrida (TF-IDF + LSA)", query = query)
}

# ── Utilidad interna: arma la tabla de resultados ordenada por score ───────
.ensamblar_resultado <- function(sim, top_n, estrategia, query = NULL) {
  ord <- order(sim, decreasing = TRUE)[seq_len(min(top_n, length(sim)))]
  ord <- ord[sim[ord] > 0]
  if (length(ord) == 0) return(tibble())
  
  stems <- if (!is.null(query)) obtener_stems_query(query) else character(0)
  
  tibble(
    posicion  = seq_along(ord),
    paper_id  = .meta$paper_id[ord],
    title     = .meta$title[ord],
    title_hl  = vapply(.meta$title[ord], resaltar_terminos, character(1), stems = stems),
    authors   = .meta$authors[ord],
    publication_date = .meta$publication_date[ord],
    topic_label = .meta$topic_label[ord],
    doi       = .meta$doi[ord],
    url       = .meta$url[ord],
    score     = round(sim[ord], 4),
    fragmento = substr(.meta$abstract[ord], 1, 220),
    fragmento_hl = vapply(substr(.meta$abstract[ord], 1, 220), resaltar_terminos, character(1), stems = stems),
    estrategia = estrategia
  )
}

# ── Función unificada usada por la app ─────────────────────────────────────
buscar_articulos <- function(query, estrategia = c("tfidf", "bm25", "lsa", "hibrida"), top_n = 10) {
  estrategia <- match.arg(estrategia)
  switch(estrategia,
         tfidf   = buscar_tfidf(query, top_n),
         bm25    = buscar_bm25(query, top_n),
         lsa     = buscar_lsa(query, top_n),
         hibrida = buscar_hibrida(query, top_n)
  )
}