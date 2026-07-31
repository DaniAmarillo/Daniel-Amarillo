# ============================================================
# build_index.R — Taller 4 · Minería de Datos (2016325)
# Precálculo del índice de recuperación de información
# Sebastián Tabares-Segovia
#
# Genera search_index.rds con dos representaciones vectoriales:
#   (A) TF-IDF disperso            -> recuperación léxica
#   (B) LSA (Truncated SVD, k=200) -> recuperación semántica reducida
#
# Uso:  Rscript build_index.R
# El objeto resultante lo carga app.R; NO se recalcula en cada búsqueda.
# ============================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(Matrix)
  library(text2vec)
  library(irlba)
  library(SnowballC)
  library(stopwords)
})

set.seed(42)
DB_PATH  <- "nature_eco_evo_2025.sqlite"
OUT_PATH <- "search_index.rds"
K_SVD    <- 200L          # componentes latentes (criterio: var. acumulada >= 0.75)

t_ini <- Sys.time()

# ── 1. Corpus desde SQLite ──────────────────────────────────
con <- dbConnect(RSQLite::SQLite(), DB_PATH)
papers <- dbGetQuery(con, "
  SELECT paper_id, title, abstract, authors_raw, publication_date,
         topic_label, doi, url, citations, downloads, year
  FROM papers
")
dbDisconnect(con)

n_total <- nrow(papers)

# Documento = título + resumen (si existe) + tema (si informativo).
# No se inventa texto: los abstract ausentes simplemente no aportan tokens.
construir_doc <- function(titulo, resumen, tema) {
  partes <- titulo
  if (!is.na(resumen) && nchar(trimws(resumen)) > 0)
    partes <- paste(partes, resumen)
  if (!is.na(tema) && tema != "Otros")
    partes <- paste(partes, tema)
  partes
}
papers$doc <- mapply(construir_doc, papers$title, papers$abstract, papers$topic_label)

# Fragmento para mostrar en la interfaz (resumen si hay; si no, el título).
papers$fragmento <- ifelse(
  !is.na(papers$abstract) & nchar(trimws(papers$abstract)) > 0,
  paste0(substr(papers$abstract, 1, 260), "…"),
  paste0("[Sin resumen disponible] ", papers$title)
)

# ── 2. Preprocesamiento de texto ────────────────────────────
# Minúsculas, se conservan solo letras y guiones (términos científicos
# como "deep-sea" o "coral-eating"), stopwords en inglés y stemming Porter.
en_stop <- stopwords::stopwords("en")

prep_txt <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z -]", " ", x)     # letras, espacio y guion
  x <- gsub("\\s+", " ", x)
  trimws(x)
}
tok_txt <- function(x) {
  toks <- text2vec::word_tokenizer(x)
  lapply(toks, function(w) {
    w <- w[nchar(w) >= 3]
    w <- w[!w %in% en_stop]
    SnowballC::wordStem(w, language = "en")
  })
}

crear_it <- function() itoken(papers$doc, preprocessor = prep_txt,
                              tokenizer = tok_txt, ids = papers$paper_id,
                              progressbar = FALSE)

# Vocabulario con unigramas y bigramas; poda de términos raros/ubicuos.
vocab <- create_vocabulary(crear_it(), ngram = c(1L, 2L))
vocab <- prune_vocabulary(vocab,
                          term_count_min       = 2L,     # aparece en >= 2 docs
                          doc_proportion_max    = 0.60)   # ignora términos ubicuos
vectorizer <- vocab_vectorizer(vocab)

# ── 3. Representación A: TF-IDF disperso ────────────────────
dtm   <- create_dtm(crear_it(), vectorizer)         # doc × término (disperso)
tfidf <- TfIdf$new(norm = "l2", sublinear_tf = TRUE)
dtm_tfidf <- fit_transform(dtm, tfidf)              # filas L2-normalizadas
dtm_tfidf <- as(dtm_tfidf, "CsparseMatrix")

n_terms <- ncol(dtm_tfidf)

# ── 4. Representación B: LSA = Truncated SVD sobre TF-IDF ────
# Se aplica sobre la matriz dispersa sin densificarla (irlba).
k   <- min(K_SVD, ncol(dtm_tfidf) - 1L, nrow(dtm_tfidf) - 1L)
svd <- irlba(dtm_tfidf, nv = k)

# Varianza explicada acumulada (a partir de los valores singulares).
var_total <- sum(dtm_tfidf^2)
var_expl  <- cumsum(svd$d^2) / var_total

# Documentos en el espacio latente: X %*% V  (= U D). Se L2-normalizan.
doc_lsa <- as.matrix(dtm_tfidf %*% svd$v)
norm_l2 <- function(M) M / pmax(sqrt(rowSums(M^2)), 1e-12)
doc_lsa <- norm_l2(doc_lsa)

# ── 5. Empaquetar y guardar ─────────────────────────────────
meta <- papers[, c("paper_id","title","authors_raw","publication_date",
                   "topic_label","doi","url","citations","downloads",
                   "year","abstract","fragmento")]

IDX <- list(
  meta        = meta,
  vectorizer  = vectorizer,
  tfidf       = tfidf,
  dtm_tfidf   = dtm_tfidf,      # A: léxica
  svd_v       = svd$v,          # proyección de la consulta al espacio latente
  svd_d       = svd$d,
  doc_lsa     = doc_lsa,        # B: semántica reducida
  en_stop     = en_stop,
  k           = k,
  var_expl_k  = as.numeric(var_expl[length(var_expl)]),
  n_docs      = nrow(meta),
  n_terms     = n_terms,
  params      = list(ngram = c(1,2), term_count_min = 2,
                     doc_proportion_max = 0.60, norm = "l2",
                     sublinear_tf = TRUE)
)

saveRDS(IDX, OUT_PATH)

# ── 6. Diagnóstico ──────────────────────────────────────────
mem_mb <- as.numeric(object.size(IDX)) / 1024^2
t_fin  <- difftime(Sys.time(), t_ini, units = "secs")

cat("\n================ ÍNDICE CONSTRUIDO ================\n")
cat(sprintf("Artículos (documentos)   : %d\n", n_total))
cat(sprintf("Vocabulario (términos)   : %d\n", n_terms))
cat(sprintf("Dim. original (TF-IDF)   : %d\n", n_terms))
cat(sprintf("Dim. reducida (LSA)      : %d\n", k))
cat(sprintf("Varianza acumulada (k)   : %.3f\n", IDX$var_expl_k))
cat(sprintf("Densidad matriz TF-IDF   : %.3f %%\n",
            100 * length(dtm_tfidf@x) / (nrow(dtm_tfidf) * ncol(dtm_tfidf))))
cat(sprintf("Tamaño del índice (RAM)  : %.2f MB\n", mem_mb))
cat(sprintf("Tiempo de construcción   : %.2f s\n", as.numeric(t_fin)))
cat(sprintf("Guardado en              : %s\n", OUT_PATH))
cat("==================================================\n")
