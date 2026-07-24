# ==============================================================================
# TALLER 3 - MINERÍA DE DATOS
# CREACIÓN REPRODUCIBLE DE PARTICIONES PARA K80
# ==============================================================================

rm(list = ls())
invisible(gc(full = TRUE))

options(
  scipen = 999,
  stringsAsFactors = FALSE
)

set.seed(2016325)

paquetes <- c("data.table", "here")

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

library(data.table)
library(here)


# 1. RUTAS --------------------------------------------------------------------

ruta_target <- here(
  "resultados",
  "target_k80_beneficiarios.rds"
)

if (!file.exists(ruta_target)) {
  stop(
    "No se encontró el archivo:\n",
    ruta_target,
    "\n\nUbíquelo dentro de la carpeta resultados/."
  )
}

dir.create(
  here("resultados"),
  recursive = TRUE,
  showWarnings = FALSE
)


# 2. CARGA Y VALIDACIÓN --------------------------------------------------------

target <- as.data.table(
  readRDS(ruta_target)
)

columnas_requeridas <- c(
  "CHAVE_FUNCIONAL",
  "target_k80",
  "tiene_cid_registrado",
  "n_registros_con_cid",
  "n_registros_k80"
)

columnas_faltantes <- setdiff(
  columnas_requeridas,
  names(target)
)

if (length(columnas_faltantes) > 0) {
  stop(
    "Faltan columnas requeridas: ",
    paste(columnas_faltantes, collapse = ", ")
  )
}

if (anyNA(target$CHAVE_FUNCIONAL)) {
  stop("Existen identificadores CHAVE_FUNCIONAL faltantes.")
}

if (anyDuplicated(target$CHAVE_FUNCIONAL) > 0) {
  stop("Existen beneficiarios duplicados en el archivo target.")
}

if (!all(target$target_k80 %in% c(0L, 1L))) {
  stop("target_k80 contiene valores distintos de 0 y 1.")
}

if (!all(target$tiene_cid_registrado %in% c(0L, 1L))) {
  stop("tiene_cid_registrado contiene valores distintos de 0 y 1.")
}

if (target[
  target_k80 == 1L &
    tiene_cid_registrado == 0L,
  .N
] > 0) {
  stop(
    "Se encontraron positivos K80 sin CID registrado."
  )
}

if (target[
  target_k80 == 1L &
    n_registros_k80 <= 0L,
  .N
] > 0) {
  stop(
    "Se encontraron positivos K80 sin registros K80."
  )
}

if (target[
  target_k80 == 0L &
    n_registros_k80 > 0L,
  .N
] > 0) {
  stop(
    "Se encontraron negativos con registros K80."
  )
}


# 3. ESTRATOS -----------------------------------------------------------------

# Se estratifica por target y disponibilidad de CID.
# Esto preserva en cada partición:
#   - la proporción de casos K80;
#   - la proporción de negativos con CID;
#   - la proporción de negativos sin CID.

target[, estrato := paste0(
  "y", target_k80,
  "_cid", tiene_cid_registrado
)]


# 4. FUNCIÓN DE ASIGNACIÓN -----------------------------------------------------

asignar_particion <- function(indices) {

  indices <- sample(indices)

  n <- length(indices)

  n_entrenamiento <- floor(0.70 * n)
  n_validacion <- floor(0.15 * n)
  n_prueba <- n - n_entrenamiento - n_validacion

  particion <- rep(
    c(
      "entrenamiento",
      "validacion",
      "prueba"
    ),
    times = c(
      n_entrenamiento,
      n_validacion,
      n_prueba
    )
  )

  data.table(
    fila = indices,
    particion = particion
  )
}


# 5. CREACIÓN DE PARTICIONES ---------------------------------------------------

asignaciones <- rbindlist(
  lapply(
    split(
      seq_len(nrow(target)),
      target$estrato
    ),
    asignar_particion
  )
)

setorder(asignaciones, fila)

target[, particion := asignaciones$particion]

target[, particion := factor(
  particion,
  levels = c(
    "entrenamiento",
    "validacion",
    "prueba"
  )
)]


# 6. VERIFICACIONES ------------------------------------------------------------

resumen_particiones <- target[
  ,
  .(
    n_beneficiarios = .N,
    n_k80 = sum(target_k80),
    n_sin_k80 = sum(target_k80 == 0L),
    n_con_cid = sum(tiene_cid_registrado),
    n_sin_cid = sum(tiene_cid_registrado == 0L),
    prevalencia_k80_pct =
      100 * mean(target_k80),
    prevalencia_k80_entre_cid_pct =
      100 * sum(target_k80) /
      sum(tiene_cid_registrado)
  ),
  by = particion
]

resumen_detallado <- target[
  ,
  .N,
  by = .(
    particion,
    target_k80,
    tiene_cid_registrado
  )
][
  order(
    particion,
    target_k80,
    tiene_cid_registrado
  )
]

conteos_positivos <- target[
  target_k80 == 1L,
  .N,
  by = particion
]

if (sum(conteos_positivos$N) != 306L) {
  stop(
    "La suma de positivos en las particiones no es 306."
  )
}


# 7. ANÁLISIS PRINCIPAL Y DE SENSIBILIDAD -------------------------------------

particiones_principal <- target[
  ,
  .(
    CHAVE_FUNCIONAL,
    particion,
    target_k80,
    tiene_cid_registrado
  )
]

particiones_sensibilidad <- target[
  tiene_cid_registrado == 1L,
  .(
    CHAVE_FUNCIONAL,
    particion,
    target_k80
  )
]

resumen_sensibilidad <- particiones_sensibilidad[
  ,
  .(
    n_beneficiarios = .N,
    n_k80 = sum(target_k80),
    n_controles_con_otro_cid =
      sum(target_k80 == 0L),
    prevalencia_k80_pct =
      100 * mean(target_k80)
  ),
  by = particion
]


# 8. EXPORTACIÓN ---------------------------------------------------------------

saveRDS(
  particiones_principal,
  here(
    "resultados",
    "particiones_k80_principal.rds"
  ),
  compress = "xz"
)

saveRDS(
  particiones_sensibilidad,
  here(
    "resultados",
    "particiones_k80_sensibilidad.rds"
  ),
  compress = "xz"
)

fwrite(
  particiones_principal,
  here(
    "resultados",
    "particiones_k80_principal.csv"
  ),
  bom = TRUE
)

fwrite(
  resumen_particiones,
  here(
    "resultados",
    "resumen_particiones_k80.csv"
  ),
  bom = TRUE
)

fwrite(
  resumen_detallado,
  here(
    "resultados",
    "resumen_particiones_k80_detallado.csv"
  ),
  bom = TRUE
)

fwrite(
  resumen_sensibilidad,
  here(
    "resultados",
    "resumen_particiones_k80_sensibilidad.csv"
  ),
  bom = TRUE
)


# 9. RESULTADO EN CONSOLA ------------------------------------------------------

cat("\n============================================================\n")
cat("PARTICIONES K80 CREADAS CORRECTAMENTE\n")
cat("============================================================\n\n")

cat("Análisis principal:\n")
print(resumen_particiones)

cat("\nDistribución de positivos K80:\n")
print(conteos_positivos)

cat("\nAnálisis de sensibilidad:\n")
print(resumen_sensibilidad)

cat("\nArchivos guardados en resultados/:\n")
cat("- particiones_k80_principal.rds\n")
cat("- particiones_k80_sensibilidad.rds\n")
cat("- particiones_k80_principal.csv\n")
cat("- resumen_particiones_k80.csv\n")
cat("- resumen_particiones_k80_detallado.csv\n")
cat("- resumen_particiones_k80_sensibilidad.csv\n")
cat("============================================================\n")
