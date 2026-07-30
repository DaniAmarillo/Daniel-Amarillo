
library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(tm)
library(Matrix)
library(irlba)

DB_PATH   <- "frontiers_plant_science_2025.sqlite"
INDEX_RDS <- "search_index.rds"
NV_MAX    <- 200    
VAR_OBJ   <- 0.80  

set.seed(2026)

# --- 0. Carga de datos ---------------------------------------------------
con     <- dbConnect(SQLite(), DB_PATH)
papers  <- dbReadTable(con, "papers")
dbDisconnect(con)

papers <- papers %>%
  mutate(
    title    = ifelse(is.na(title), "", title),
    abstract = ifelse(is.na(abstract), "", abstract)
  )

n_docs <- nrow(papers)
cat("== Construcción del corpus ==\n")
cat("Artículos totales      :", n_docs, "\n")
cat("Sin título             :", sum(papers$title == ""), "\n")
cat("Sin resumen (abstract) :", sum(papers$abstract == ""), "\n")
cat("Columna de keywords    : no existe en esta base -> no se usa\n")
cat("Idiomas detectados     : inglés (títulos y resúmenes de Frontiers)\n\n")

 
papers$texto_original <- str_trim(paste(papers$title, papers$abstract))

# --- 1. Procesamiento de texto -------------------------------------------
limpiar_texto <- function(x) {
  x <- tolower(x)
  x <- str_replace_all(x, "<[^>]+>", " ")     # residuos de HTML del scraping
  x <- str_replace_all(x, "&[a-z]+;", " ")    # entidades HTML (&amp;, &#39; ...)
  x <- str_replace_all(x, "[^a-z\\s]", " ")   # conserva solo letras (se descartan
                                               # números y puntuación)
  x <- str_replace_all(x, "\\s+", " ")
  str_trim(x)
}

papers$texto_limpio <- limpiar_texto(papers$texto_original)

corpus <- VCorpus(VectorSource(papers$texto_limpio))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)


dtm_raw <- DocumentTermMatrix(corpus, control = list(wordLengths = c(3, Inf)))


dtm_sparse <- sparseMatrix(
  i = dtm_raw$i, j = dtm_raw$j, x = dtm_raw$v,
  dims = c(dtm_raw$nrow, dtm_raw$ncol),
  dimnames = list(NULL, colnames(dtm_raw))
)

df_terminos <- colSums(dtm_sparse > 0)          # nº de documentos donde aparece cada término
vocab_ok <- df_terminos >= 2 & df_terminos <= 0.90 * n_docs


dtm_sparse <- dtm_sparse[, vocab_ok]
vocab      <- colnames(dtm_sparse)
df_terminos <- df_terminos[vocab_ok]

dim_original <- ncol(dtm_sparse)
cat("== Vocabulario ==\n")
cat("Tamaño del vocabulario tras el recorte:", dim_original, "términos\n")
cat("Densidad de la matriz término-documento:",
    round(100 * length(dtm_sparse@x) / (nrow(dtm_sparse) * ncol(dtm_sparse)), 3), "% (matriz dispersa)\n\n")

dl     <- rowSums(dtm_sparse)          # longitud (nº de términos) de cada documento
avgdl  <- mean(dl)
dl_div <- ifelse(dl == 0, 1, dl)       # evita división 0/0 si algún doc quedó sin vocabulario

# --- 3. Representación vectorial: TF-IDF (para LSA) y BM25 ---------------
# 3.1 TF-IDF normalizado por longitud de documento (para Truncated SVD/LSA)
idf_tfidf <- log(n_docs / df_terminos)
tf_norm   <- dtm_sparse / dl_div                    # tf normalizado por longitud del doc
tfidf_sparse <- tf_norm %*% Diagonal(x = idf_tfidf)
dimnames(tfidf_sparse) <- list(NULL, vocab)

# 3.2 IDF estilo Okapi BM25 (se usa aparte, sobre las frecuencias crudas)
idf_bm25 <- log((n_docs - df_terminos + 0.5) / (df_terminos + 0.5) + 1)

# --- 4. Reducción de dimensionalidad: Truncated SVD (LSA) -----------------
total_var <- sum(tfidf_sparse@x^2)  # ||A||_F^2 exacto (no requiere el SVD completo)

svd_res <- irlba(tfidf_sparse, nv = NV_MAX)
var_explicada <- cumsum(svd_res$d^2) / total_var

k_lsa <- which(var_explicada >= VAR_OBJ)[1]
if (is.na(k_lsa)) k_lsa <- NV_MAX  # si no se alcanza el objetivo, se usan todos los calculados

cat("== Reducción dimensional (Truncated SVD / LSA) ==\n")
cat("Dimensión original           :", dim_original, "\n")
cat("Dimensión final (k)          :", k_lsa, "\n")
cat("Varianza retenida            :", round(100 * var_explicada[k_lsa], 1), "%\n")
cat("Criterio de selección de k   : mínimo k tal que la varianza acumulada de los",
    "valores singulares >=", VAR_OBJ * 100, "%\n\n")

V_k <- svd_res$v[, 1:k_lsa, drop = FALSE]                 # términos -> espacio latente
doc_vectors_lsa <- as.matrix(tfidf_sparse %*% V_k)        # documentos en espacio reducido (== U*Sigma)

# --- 5. Empaquetado y guardado del índice ---------------------------------
metadatos <- papers %>%
  select(paper_id, title, authors_raw, publication_date, year,
         topic_label, doi, url, citations, downloads, abstract)

search_index <- list(
  metadatos        = metadatos,
  vocab            = vocab,
  dim_original     = dim_original,
  n_docs           = n_docs,
  dtm_raw          = dtm_sparse,     # frecuencias crudas (para BM25)
  dl               = dl,
  avgdl            = avgdl,
  idf_bm25         = idf_bm25,
  idf_tfidf        = idf_tfidf,
  k_lsa            = k_lsa,
  V_k              = V_k,
  doc_vectors_lsa  = doc_vectors_lsa,
  var_explicada    = var_explicada[k_lsa],
  built_at         = Sys.time()
)

saveRDS(search_index, INDEX_RDS)
cat("Índice guardado en:", INDEX_RDS, "\n")
