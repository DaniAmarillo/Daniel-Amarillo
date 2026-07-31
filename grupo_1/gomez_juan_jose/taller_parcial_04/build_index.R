# =============================================================================
# build_index.R  --  Taller 4 · Minería de Datos (2016325) · UNAL
#
# Construye TODOS los objetos del buscador a partir de la base SQLite y los
# serializa en index/indice_jmlr.rds.
#
# Se ejecuta UNA sola vez, fuera de la aplicación:
#     Rscript build_index.R
#
# La app Shiny únicamente hace readRDS() al arrancar. Nunca reconstruye la
# DTM, ni reajusta el SVD, ni reprocesa el corpus por consulta (requisito §4.7).
# =============================================================================

suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(Matrix); library(irlba)
})

# --- Raíz del proyecto -------------------------------------------------------
# Los caminos son relativos a la carpeta del proyecto, no al directorio de
# trabajo de R. Esto evita el error "no fue posible abrir el archivo
# 'R/text_processing.R'" cuando el script se ejecuta desde otra carpeta.
raiz_proyecto <- function() {
  args <- commandArgs(trailingOnly = FALSE)          # Rscript build_index.R
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) return(dirname(normalizePath(f[1])))
  for (fr in rev(sys.frames())) {                    # source("build_index.R")
    if (!is.null(fr$ofile)) return(dirname(normalizePath(fr$ofile)))
  }
  getwd()                                            # botón "Source" / consola
}
RAIZ <- raiz_proyecto()
ruta <- function(...) file.path(RAIZ, ...)

if (!file.exists(ruta("R", "text_processing.R")))
  stop("No se encontró la carpeta R/ junto a build_index.R.\n",
       "  Carpeta detectada: ", RAIZ, "\n",
       "  Descomprime el proyecto completo y ejecuta el script desde su raíz.")

source(ruta("R", "text_processing.R"))
source(ruta("R", "retrieval.R"))

DB_PATH   <- ruta("data", "jmlr_q1_2025.sqlite")
OUT_PATH  <- ruta("index", "indice_jmlr.rds")
PESO_TITULO <- 2L     # el título se repite 2 veces en el documento
MIN_DF      <- 2L     # término presente en >= 2 documentos
MAX_DF_PROP <- 0.60   # término presente en <= 60 % de los documentos
K_SVD       <- 120L   # componentes latentes (codo de la curva de energía, ver informe)

cat("=== Taller 4 · construcción del índice ===\n\n")

# --- 1. Corpus ---------------------------------------------------------------
con <- dbConnect(SQLite(), DB_PATH)
meta <- dbGetQuery(con, "
  SELECT paper_id, paper_num, title, abstract, authors_raw, n_authors,
         year, publication_date, doi, url, pdf_url, topic_label,
         n_pages, has_code, citations, downloads
  FROM papers
  ORDER BY paper_num")
dbDisconnect(con)

cat("Artículos leídos:", nrow(meta), "\n")
cat("  sin título  :", sum(is.na(meta$title)  | trimws(meta$title)  == ""), "\n")
cat("  sin abstract:", sum(is.na(meta$abstract) | trimws(meta$abstract) == ""), "\n")
cat("  sin DOI     :", sum(is.na(meta$doi)), "\n")
cat("  sin fecha   :", sum(is.na(meta$publication_date)), "(solo se dispone del año)\n")

# Documento = título (x PESO_TITULO) + abstract.
# NO se incluye topic_label: es una etiqueta derivada de un clasificador regex
# propio del Taller 1; incorporarla realimentaría al buscador las mismas
# palabras clave con las que se etiquetó, inflando artificialmente la
# coincidencia léxica. Tampoco hay campo de keywords: JMLR no las publica.
docs <- paste(
  strrep(paste0(meta$title, ". "), PESO_TITULO),
  ifelse(is.na(meta$abstract), "", meta$abstract)
)

# --- 2. Procesamiento --------------------------------------------------------
t0 <- Sys.time()
tokens <- procesar_corpus(docs)
t_tok <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

cat("\nTokenización:", round(t_tok, 2), "s\n")
cat("  tokens totales   :", format(sum(lengths(tokens)), big.mark = ","), "\n")
cat("  tokens/documento :", round(mean(lengths(tokens)), 1), "\n")

# --- 3. DTM ------------------------------------------------------------------
d <- construir_dtm(tokens, min_df = MIN_DF, max_df_prop = MAX_DF_PROP)
dtm <- d$dtm
cat("\nDTM:", nrow(dtm), "x", ncol(dtm), "\n")
cat("  vocabulario sin filtrar:", format(d$vocab_full_size, big.mark = ","), "\n")
cat("  vocabulario final      :", format(ncol(dtm), big.mark = ","), "\n")
cat("  densidad               :", sprintf("%.4f %%", 100 * Matrix::nnzero(dtm) /
                                            prod(dim(dtm))), "\n")
cat("  memoria (dispersa)     :", format(object.size(dtm), units = "MB"), "\n")
cat("  memoria (si fuera densa):",
    sprintf("%.1f MB", prod(dim(dtm)) * 8 / 1024^2), "\n")

# --- 4. Modelos --------------------------------------------------------------
cat("\nAjustando BM25...\n")
t0 <- Sys.time()
bm25 <- construir_bm25(dtm)
t_bm <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat("  listo en", round(t_bm, 2), "s\n")

cat("Ajustando TF-IDF + Truncated SVD (k =", K_SVD, ")...\n")
tfidf <- construir_tfidf(dtm)
lsa <- construir_lsa(tfidf$X, k = K_SVD)
cat("  irlba:", round(lsa$tiempo_ajuste_s, 2), "s\n")
cat("  energía retenida con k =", K_SVD, ":",
    sprintf("%.1f %%", 100 * lsa$energia_retenida[K_SVD]), "\n")

# --- 5. Serialización --------------------------------------------------------
idx <- list(
  meta = meta, vocab = d$vocab, dtm = dtm, bm25 = bm25,
  tfidf = tfidf, lsa = lsa,
  params = list(peso_titulo = PESO_TITULO, min_df = MIN_DF,
                max_df_prop = MAX_DF_PROP, k_svd = K_SVD,
                k1 = bm25$k1, b = bm25$b,
                vocab_full = d$vocab_full_size,
                construido = Sys.time(), r_version = R.version.string)
)

dir.create(ruta("index"), showWarnings = FALSE)
saveRDS(idx, OUT_PATH, compress = "xz")
cat("\nÍndice guardado en", OUT_PATH,
    sprintf("(%.1f MB en disco)\n", file.info(OUT_PATH)$size / 1024^2))
cat("Memoria del objeto en RAM:", format(object.size(idx), units = "MB"), "\n")
