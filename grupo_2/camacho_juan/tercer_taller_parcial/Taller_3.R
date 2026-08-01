# ==============================================================================
# TALLER 3 - MINERÍA DE DATOS
# PIPELINE MAESTRO: COLELITIASIS (K80)
# Autor: Juan Andrés Camacho Zárate
# ==============================================================================

rm(list = ls())
invisible(gc(full = TRUE))

options(
  scipen = 999,
  stringsAsFactors = FALSE
)

scripts <- c(
  "scripts/00_diagnostico_K80_por_bloques.R",
  "scripts/01_crear_particiones_k80.R",
  "scripts/02_construir_variables_K80_final.R",
  "scripts/03_modelamiento_K80.R",
  "scripts/04_modelamiento_costos_K80.R"
)

faltantes <- scripts[!file.exists(scripts)]

if (length(faltantes) > 0L) {
  stop(
    "No se encontraron estos scripts:\n",
    paste(faltantes, collapse = "\n")
  )
}

if (!file.exists("data/raw/db_2026.csv")) {
  stop(
    "No se encontró data/raw/db_2026.csv.\n",
    "Ubique la base original en esa carpeta."
  )
}

cat("\n============================================================\n")
cat("INICIANDO PIPELINE COMPLETO K80\n")
cat("============================================================\n")

for (archivo in scripts) {

  cat("\nEjecutando:", archivo, "\n")

  entorno_etapa <- new.env(
    parent = globalenv()
  )

  sys.source(
    archivo,
    envir = entorno_etapa
  )

  rm(entorno_etapa)
  invisible(gc(full = TRUE))
}

cat("\n============================================================\n")
cat("PIPELINE COMPLETO FINALIZADO\n")
cat("Compile taller_3.Rmd para generar taller_3.html\n")
cat("============================================================\n")
