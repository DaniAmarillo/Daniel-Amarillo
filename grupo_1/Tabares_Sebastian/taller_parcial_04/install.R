# ============================================================
# install.R — Dependencias del Taller 4
# Ejecutar una vez:  Rscript install.R
# ============================================================

pkgs <- c(
  # App Shiny (Taller 2)
  "shiny", "bslib", "DT", "dplyr", "stringr", "tibble",
  "DBI", "RSQLite", "httr2", "purrr", "highcharter", "rvest",
  # Buscador / recuperación de información (Taller 4)
  "Matrix", "text2vec", "irlba", "SnowballC", "stopwords",
  # Documento reproducible
  "knitr", "ggplot2", "scales", "rmarkdown"
)

faltan <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(faltan) > 0) {
  message("Instalando: ", paste(faltan, collapse = ", "))
  install.packages(faltan, repos = "https://cloud.r-project.org")
} else {
  message("Todas las dependencias ya están instaladas.")
}

# Para generar renv.lock (opcional, recomendado para reproducibilidad):
#   renv::init(); renv::snapshot()
