# ==============================================================================
# DIAGNÓSTICO SEGURO K80 POR BLOQUES
# ==============================================================================
# Este archivo reemplaza temporalmente los pipelines anteriores.
# NO usa DuckDB y NO carga la base completa en memoria.
#
# Objetivo:
#   1. Leer solo CHAVE_FUNCIONAL y CID en bloques de 50.000 filas.
#   2. Construir la variable objetivo K80 a nivel de beneficiario.
#   3. Confirmar el número real de positivos sin abortar la sesión.
# ==============================================================================

rm(list = ls())
invisible(gc(full = TRUE))

options(
  scipen = 999,
  stringsAsFactors = FALSE,
  readr.show_progress = FALSE
)

paquetes <- c("readr", "data.table", "here")

faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes) > 0) {
  stop(
    "Faltan estos paquetes: ",
    paste(faltantes, collapse = ", "),
    "\nInstálelos con:\ninstall.packages(c(",
    paste(sprintf('"%s"', faltantes), collapse = ", "),
    "))"
  )
}

library(readr)
library(data.table)
library(here)

# ------------------------------------------------------------------------------
# 1. Ruta de la base
# ------------------------------------------------------------------------------

ruta_base <- here("data", "raw", "db_2026.csv")

if (!file.exists(ruta_base)) {
  message("No se encontró db_2026.csv en data/raw/.")
  message("Seleccione manualmente el archivo.")
  ruta_base <- file.choose()
}

ruta_salida <- here("resultados")
dir.create(ruta_salida, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 2. Funciones
# ------------------------------------------------------------------------------

normalizar_cid <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[x %in% c("", "NA", "N/A", "NULL", "-")] <- NA_character_
  x <- gsub("[^A-Z0-9]", "", x)
  x[nchar(x) < 3] <- NA_character_
  x
}

acumulado <- NULL
contador_bloques <- 0L
contador_filas <- 0

procesar_bloque <- function(x, pos) {

  contador_bloques <<- contador_bloques + 1L
  contador_filas <<- contador_filas + nrow(x)

  dt <- as.data.table(x)

  dt[, CHAVE_FUNCIONAL := trimws(as.character(CHAVE_FUNCIONAL))]
  dt[
    is.na(CHAVE_FUNCIONAL) |
      CHAVE_FUNCIONAL %in% c("", "NA", "N/A", "NULL", "-"),
    CHAVE_FUNCIONAL := NA_character_
  ]

  dt[, CID_LIMPIO := normalizar_cid(CID)]

  dt <- dt[!is.na(CHAVE_FUNCIONAL)]

  if (nrow(dt) > 0) {

    resumen_bloque <- dt[, .(
      target_k80 = as.integer(any(
        !is.na(CID_LIMPIO) &
          substr(CID_LIMPIO, 1, 3) == "K80"
      )),
      tiene_cid_registrado = as.integer(any(!is.na(CID_LIMPIO))),
      n_registros_con_cid = sum(!is.na(CID_LIMPIO)),
      n_registros_k80 = sum(
        !is.na(CID_LIMPIO) &
          substr(CID_LIMPIO, 1, 3) == "K80"
      )
    ), by = CHAVE_FUNCIONAL]

    if (is.null(acumulado)) {
      acumulado <<- resumen_bloque
    } else {
      acumulado <<- rbindlist(
        list(acumulado, resumen_bloque),
        use.names = TRUE
      )[, .(
        target_k80 = max(target_k80),
        tiene_cid_registrado = max(tiene_cid_registrado),
        n_registros_con_cid = sum(n_registros_con_cid),
        n_registros_k80 = sum(n_registros_k80)
      ), by = CHAVE_FUNCIONAL]
    }
  }

  rm(dt)
  invisible(gc())

  cat(
    sprintf(
      "\rBloques procesados: %d | Filas leídas: %s | Beneficiarios acumulados: %s",
      contador_bloques,
      format(contador_filas, big.mark = ".", scientific = FALSE),
      if (is.null(acumulado)) {
        "0"
      } else {
        format(nrow(acumulado), big.mark = ".", scientific = FALSE)
      }
    )
  )

  invisible(NULL)
}

# ------------------------------------------------------------------------------
# 3. Lectura por bloques
# ------------------------------------------------------------------------------

cat("\nIniciando lectura segura por bloques...\n")
cat("No cierre RStudio mientras avanza el contador.\n\n")

callback <- SideEffectChunkCallback$new(procesar_bloque)

read_csv_chunked(
  file = ruta_base,
  callback = callback,
  chunk_size = 50000,
  col_types = cols_only(
    CHAVE_FUNCIONAL = col_character(),
    CID = col_character()
  ),
  na = c("", "NA", "N/A", "NULL", "-"),
  progress = FALSE,
  show_col_types = FALSE
)

cat("\n\nLectura terminada.\n")

if (is.null(acumulado) || nrow(acumulado) == 0) {
  stop("No se pudo construir la tabla de beneficiarios.")
}

setorder(acumulado, CHAVE_FUNCIONAL)

# ------------------------------------------------------------------------------
# 4. Resultados
# ------------------------------------------------------------------------------

distribucion_target <- acumulado[, .N, by = target_k80][
  order(target_k80)
]

distribucion_target[, porcentaje :=
  round(100 * N / sum(N), 6)
]

resumen_general <- data.table(
  indicador = c(
    "Filas procesadas",
    "Beneficiarios únicos",
    "Beneficiarios con algún CID registrado",
    "Beneficiarios K80",
    "Prevalencia K80 (%)",
    "Registros K80"
  ),
  valor = c(
    contador_filas,
    nrow(acumulado),
    acumulado[tiene_cid_registrado == 1L, .N],
    acumulado[target_k80 == 1L, .N],
    100 * mean(acumulado$target_k80),
    sum(acumulado$n_registros_k80)
  )
)

saveRDS(
  acumulado,
  file.path(ruta_salida, "target_k80_beneficiarios.rds"),
  compress = "xz"
)

fwrite(
  distribucion_target,
  file.path(ruta_salida, "diagnostico_distribucion_k80.csv"),
  bom = TRUE
)

fwrite(
  resumen_general,
  file.path(ruta_salida, "diagnostico_resumen_k80.csv"),
  bom = TRUE
)

cat("\n============================================================\n")
cat("DIAGNÓSTICO FINALIZADO CORRECTAMENTE\n")
cat("============================================================\n")
print(resumen_general)
cat("\nArchivos generados en la carpeta resultados/:\n")
cat("- target_k80_beneficiarios.rds\n")
cat("- diagnostico_distribucion_k80.csv\n")
cat("- diagnostico_resumen_k80.csv\n")
cat("============================================================\n")
