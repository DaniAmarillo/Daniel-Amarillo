# =============================================================================
# install_deps.R  --  Taller 4 · Minería de Datos (2016325) · UNAL
#
# Archivo de dependencias del proyecto.
#     source("install_deps.R")
#
# Para generar un renv.lock con las versiones exactas de TU máquina:
#     install.packages("renv"); renv::init(); renv::snapshot()
# =============================================================================

DEPENDENCIAS <- c(
  # --- Base de datos ---
  "DBI",          # interfaz genérica de bases de datos
  "RSQLite",      # backend SQLite

  # --- Recuperación de información (nuevas en el Taller 4) ---
  "Matrix",       # matrices dispersas (dgCMatrix) para la DTM
  "irlba",        # SVD truncado sin densificar la matriz
  "SnowballC",    # stemming de Porter

  # --- Aplicación Shiny (heredadas del Taller 2) ---
  "shiny", "bslib", "DT", "highcharter", "htmltools",

  # --- Manipulación de datos ---
  "dplyr", "tibble", "purrr", "stringr",

  # --- Scraping (heredado del Taller 2) ---
  "rvest", "httr",

  # --- Documento reproducible ---
  "rmarkdown", "knitr"
)

faltantes <- setdiff(DEPENDENCIAS, rownames(installed.packages()))

if (length(faltantes) == 0) {
  message("Todas las dependencias están instaladas.")
} else {
  message("Instalando: ", paste(faltantes, collapse = ", "))
  install.packages(faltantes, repos = "https://cloud.r-project.org")
}

# Versiones mínimas verificadas durante el desarrollo. No son cotas duras: el
# proyecto no usa nada exótico y versiones posteriores deberían funcionar.
MINIMOS <- c(DBI = "1.2.0", RSQLite = "2.3.0", Matrix = "1.6.0",
             irlba = "2.3.5", SnowballC = "0.7.0", shiny = "1.8.0",
             bslib = "0.6.0", DT = "0.31", rmarkdown = "2.25")

invisible(lapply(names(MINIMOS), function(p) {
  if (p %in% rownames(installed.packages()) &&
      utils::packageVersion(p) < MINIMOS[[p]])
    warning(sprintf("%s %s es anterior a la versión verificada (%s).",
                    p, utils::packageVersion(p), MINIMOS[[p]]), call. = FALSE)
}))

cat("\nR:", R.version.string, "\n")
cat("Desarrollado y verificado con R 4.3.3.\n")
