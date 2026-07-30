library(DBI)
library(RSQLite)

source("search_helpers.R")

db_path <- "entropy_2025.sqlite"
index_path <- "search_index.rds"

if (!file.exists(db_path)) {
  stop("No se encontro la base SQLite: ", db_path)
}

idx <- load_or_build_search_index(
  db_path = db_path,
  index_path = index_path,
  force = TRUE
)

cat("Indice de busqueda construido correctamente.\n")
cat("Documentos incluidos:", idx$meta$n_docs, "\n")
cat("Dimension original:", idx$meta$original_dim, "\n")
cat("Dimension reducida:", idx$meta$reduced_dim, "\n")
cat("Archivo:", normalizePath(index_path), "\n")
