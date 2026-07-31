

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(stringr)
  library(tm)
  library(SnowballC)
  library(Matrix)
  library(irlba)
})

set.seed(42)
t0 <- Sys.time()

DB_PATH  <- "revista_plosone.sqlite"
OUT_DIR  <- "index"
dir.create(OUT_DIR, showWarnings = FALSE)

# ── 1. Construcción del corpus ─────────────────────────────────────────────
con <- dbConnect(SQLite(), DB_PATH)
papers <- dbGetQuery(con, "SELECT paper_id, journal_name, title, publication_date, year,
                                   doi, url, abstract, authors, citations, views,
                                   n_references, topic_label
                            FROM papers")
dbDisconnect(con)

n_total <- nrow(papers)
n_sin_abstract <- sum(is.na(papers$abstract) | trimws(papers$abstract) == "")
n_sin_titulo   <- sum(is.na(papers$title) | trimws(papers$title) == "")

cat("Artículos totales:", n_total, "\n")
cat("Sin abstract:", n_sin_abstract, "| Sin título:", n_sin_titulo, "\n")

papers <- papers %>%
  mutate(
    abstract_ok = ifelse(is.na(abstract) | trimws(abstract) == "", "", abstract),
    texto_repr  = trimws(paste(title, abstract_ok))
  ) %>%
  filter(trimws(title) != "")  # excluimos únicamente registros sin ni siquiera título

cat("Artículos incluidos en el corpus:", nrow(papers), "\n")

# ── 2. Preprocesamiento de texto ───────────────────────────────────────────

preprocesar_corpus <- function(textos) {
  corpus <- VCorpus(VectorSource(textos))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stemDocument, language = "english")
  corpus <- tm_map(corpus, stripWhitespace)
  corpus
}

corpus <- preprocesar_corpus(papers$texto_repr)

# ── 3. Representación TF-IDF ───────────────────────────────────────────────
# Se descartan términos que aparecen en menos de 3 documentos (reduce ruido
# de erratas/erratas de OCR y controla el tamaño del vocabulario y la memoria).
dtm <- DocumentTermMatrix(
  corpus,
  control = list(
    wordLengths = c(3, Inf),
    bounds      = list(global = c(3, Inf))
  )
)

vocab_size   <- ncol(dtm)
dim_original <- vocab_size

cat("Tamaño del vocabulario (dim. original TF-IDF):", vocab_size, "\n")
cat("Densidad de la matriz:", round(100 * sum(dtm$v > 0) / (nrow(dtm) * ncol(dtm)), 4), "% (dispersa)\n")

tfidf_dtm <- weightTfIdf(dtm, normalize = TRUE)

# Conversión a matriz dispersa (dgCMatrix) — nunca se convierte a densa
tfidf_mat <- sparseMatrix(
  i = tfidf_dtm$i, j = tfidf_dtm$j, x = tfidf_dtm$v,
  dims = dim(tfidf_dtm), dimnames = list(NULL, colnames(tfidf_dtm))
)

# IDF por término, necesario para proyectar consultas nuevas al mismo espacio
tf_global   <- slam::col_sums(dtm > 0)
idf_vector  <- log(nrow(dtm) / (tf_global + 1)) + 1
names(idf_vector) <- colnames(dtm)

cat("Dimensión de la matriz TF-IDF:", paste(dim(tfidf_mat), collapse = " x "), "\n")

# ── 3b. Matriz de frecuencias crudas + estadísticas para BM25 ──────────────
# BM25 necesita la frecuencia cruda del término en el documento (no TF-IDF),
# la longitud de cada documento y la longitud promedio del corpus.
tf_mat_raw <- sparseMatrix(
  i = dtm$i, j = dtm$j, x = dtm$v,
  dims = dim(dtm), dimnames = list(NULL, colnames(dtm))
)
doc_len_bm25 <- Matrix::rowSums(tf_mat_raw)
avgdl_bm25   <- mean(doc_len_bm25)

# IDF específico de BM25 (Robertson-Sparck-Jones), distinto del IDF usado
# para ponderar TF-IDF
n_docs   <- nrow(dtm)
idf_bm25 <- log((n_docs - tf_global + 0.5) / (tf_global + 0.5) + 1)
names(idf_bm25) <- colnames(dtm)

cat("BM25 — longitud promedio de documento:", round(avgdl_bm25, 1), "términos\n")

# ── 4. Reducción dimensional: Truncated SVD (LSA) ──────────────────────────

k_max <- min(200, nrow(tfidf_mat) - 1)
svd_full <- irlba(tfidf_mat, nv = k_max)

var_total <- sum(tfidf_mat@x^2)
var_exp   <- cumsum(svd_full$d^2) / var_total

# Criterio de selección de k:

ganancia_25 <- diff(var_exp[seq(25, k_max, by = 25)]) * 100
k_candidatos <- seq(50, k_max, by = 25)
idx_corte <- which(ganancia_25 < 1)[1]
k_elegido <- if (!is.na(idx_corte)) k_candidatos[idx_corte] else 150
k_elegido <- min(k_elegido, k_max, 150)  # nunca más que lo calculado ni más de 150

cat("k evaluados:", k_max, "\n")
cat("k elegido para LSA:", k_elegido, "\n")
cat("Varianza acumulada explicada con k =", k_elegido, ":", round(var_exp[k_elegido] * 100, 1), "%\n")

U <- svd_full$u[, 1:k_elegido, drop = FALSE]
D <- svd_full$d[1:k_elegido]
V <- svd_full$v[, 1:k_elegido, drop = FALSE]
rownames(V) <- colnames(tfidf_mat)

# Coordenadas de los documentos en el espacio LSA reducido
docs_lsa <- U %*% diag(D)

# ── 5. Guardado de objetos precalculados (caché) ───────────────────────────
meta <- papers %>%
  dplyr::select(paper_id, title, authors, publication_date, topic_label,
         doi, url, citations, views, abstract = abstract_ok) %>%
  dplyr::mutate(row_id = row_number())



saveRDS(meta,       file.path(OUT_DIR, "meta.rds"))
saveRDS(tfidf_mat,  file.path(OUT_DIR, "tfidf_mat.rds"))
saveRDS(idf_vector, file.path(OUT_DIR, "idf_vector.rds"))
saveRDS(list(V = V, D = D, k = k_elegido, docs_lsa = docs_lsa),
        file.path(OUT_DIR, "lsa_model.rds"))
saveRDS(list(
  tf_mat = tf_mat_raw, doc_len = doc_len_bm25, avgdl = avgdl_bm25,
  idf_bm25 = idf_bm25, k1 = 1.5, b = 0.75
), file.path(OUT_DIR, "bm25_model.rds"))
saveRDS(list(
  n_total = n_total, n_sin_abstract = n_sin_abstract, n_sin_titulo = n_sin_titulo,
  n_incluidos = nrow(papers), vocab_size = vocab_size,
  dim_tfidf = dim(tfidf_mat), k_max = k_max, k_elegido = k_elegido,
  var_exp_k = var_exp[k_elegido], var_exp_full = var_exp
), file.path(OUT_DIR, "stats.rds"))

cat("\nObjetos guardados en:", OUT_DIR, "\n")
cat("Tiempo total:", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")
