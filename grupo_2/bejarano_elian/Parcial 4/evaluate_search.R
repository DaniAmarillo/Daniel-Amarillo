library(DBI)
library(RSQLite)
library(dplyr)

source("search_helpers.R")

idx <- readRDS("search_index.rds")

queries <- data.frame(
  query_id = c("Q1_directa", "Q2_sinonimos", "Q3_general", "Q4_especifica", "Q5_dificil"),
  query = c(
    "white box cryptography differential computation attack",
    "language models text generation semantic embeddings",
    "entropy statistical physics complex systems",
    "conditional gaussian nonlinear systems data assimilation uncertainty",
    "marine ecology biodiversity conservation"
  ),
  type = c("terminos directos", "sinonimos o terminos relacionados", "general", "especifica", "caso poco relevante"),
  stringsAsFactors = FALSE
)

all_results <- list()

for (i in seq_len(nrow(queries))) {
  for (strategy in c("tfidf", "lsa")) {
    res <- search_articles(queries$query[i], idx, strategy, 5)
    if (nrow(res) == 0) next
    res$query_id <- queries$query_id[i]
    res$query <- queries$query[i]
    res$query_type <- queries$type[i]
    res$is_relevant <- NA
    all_results[[length(all_results) + 1L]] <- res
  }
}

evaluation <- bind_rows(all_results) |>
  select(query_id, query_type, query, strategy, rank, score, title, topic_label, doi, fragment, is_relevant)

write.csv(evaluation, "consultas_evaluacion.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Archivo consultas_evaluacion.csv generado con", nrow(evaluation), "filas.\n")
cat("Complete manualmente la columna is_relevant con 1/0 y calcule Precision@5.\n")
