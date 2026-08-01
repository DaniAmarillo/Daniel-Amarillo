
# Taller 3 - Minería de Datos 
# PARTE 2: Construcción de la variable objetivo
# Jorge Andres Sanchez Duarte



library(dplyr)
library(readr)
library(stringr)
library(knitr)

datos_limpios <- readRDS("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/datos_limpios.rds")


# Definición clínica de la enfermedad seleccionada

# En la Clasificación Internacional de Enfermedades (CIE-10 / ICD-10, OMS),
# el bloque K40-K46 agrupa todas las hernias abdominales, distinguidas por
# localización anatómica:
#   K40 = Hernia inguinal
#   K41 = Hernia femoral
#   K42 = Hernia umbilical
#   K43 = Hernia ventral
#   K44 = Hernia diafragmática
#   K45 = Otras hernias abdominales
#   K46 = Hernia abdominal, no especificada
#
# La variable objetivo de este taller es ahora "hernia" (cualquier tipo de
# hernia abdominal), es decir, el código CID (ya normalizado en el script
# 1, sin punto y en mayúsculas) debe EMPEZAR por "K4" seguido de un dígito
# entre 0 y 6. 

codigo_cid_objetivo <- "^K4[0-6]"


# Verificación

etiquetas_hernia <- c(
  "K40" = "Hernia inguinal",
  "K41" = "Hernia femoral",
  "K42" = "Hernia umbilical",
  "K43" = "Hernia ventral",
  "K44" = "Hernia diafragmática",
  "K45" = "Otras hernias abdominales",
  "K46" = "Hernia abdominal no especificada"
)

subcodigos_hernia <- datos_limpios |>
  filter(str_detect(CID, codigo_cid_objetivo)) |>
  mutate(tipo_hernia = etiquetas_hernia[str_sub(CID, 1, 3)]) |>
  count(tipo_hernia, CID, name = "n_transacciones") |>
  arrange(desc(n_transacciones))

cat("Subcódigos del bloque de hernias (K40-K46) presentes en la base,",
    "por tipo clínico:\n")
print(subcodigos_hernia)

cat("\nResumen por tipo de hernia (transacciones, no beneficiarios):\n")
print(
  subcodigos_hernia |>
    group_by(tipo_hernia) |>
    summarise(n_transacciones = sum(n_transacciones), .groups = "drop") |>
    mutate(pct = round(n_transacciones / sum(n_transacciones) * 100, 1)) |>
    arrange(desc(n_transacciones))
)

# Construcción de la variable objetivo a nivel de beneficiario
#
variable_objetivo <- datos_limpios |>
  group_by(CHAVE_FUNCIONAL) |>
  summarise(
    hernia = as.integer(any(str_detect(CID, codigo_cid_objetivo), na.rm = TRUE)),
    .groups = "drop"
  )

cat("\nDistribución de la variable objetivo (hernia) a nivel de beneficiario:\n")
print(table(variable_objetivo$hernia))
cat("\nPorcentaje de beneficiarios con alguna hernia (K40-K46):\n")
print(round(prop.table(table(variable_objetivo$hernia)) * 100, 2))

# Verificación de consistencia

n_benef_total     <- n_distinct(datos_limpios$CHAVE_FUNCIONAL)
n_benef_con_hernia <- datos_limpios |>
  filter(str_detect(CID, codigo_cid_objetivo)) |>
  summarise(n = n_distinct(CHAVE_FUNCIONAL)) |>
  pull(n)

cat("\nVerificación de consistencia \n")
cat("Beneficiarios totales en datos_limpios          :", n_benef_total, "\n")
cat("Filas en variable_objetivo                      :", nrow(variable_objetivo), "\n")
cat("Beneficiarios distintos con >=1 transacción hernia:", n_benef_con_hernia, "\n")
cat("Positivos (hernia == 1) en variable_objetivo      :",
    sum(variable_objetivo$hernia == 1), "\n")
stopifnot(nrow(variable_objetivo) == n_benef_total)
stopifnot(sum(variable_objetivo$hernia == 1) == n_benef_con_hernia)
cat("OK: las tres cifras anteriores coinciden, la variable objetivo está",
    "bien construida.\n")


# ¿Cuántos beneficiarios positivos tienen MÁS de un tipo de hernia?


n_tipos_por_benef <- datos_limpios |>
  filter(str_detect(CID, codigo_cid_objetivo)) |>
  mutate(tipo_hernia = etiquetas_hernia[str_sub(CID, 1, 3)]) |>
  group_by(CHAVE_FUNCIONAL) |>
  summarise(n_tipos_distintos = n_distinct(tipo_hernia), .groups = "drop")

cat("\nDe los", nrow(n_tipos_por_benef), "beneficiarios con alguna hernia,",
    sum(n_tipos_por_benef$n_tipos_distintos > 1),
    "tienen más de un tipo de hernia distinto registrado.\n")


# Guardar la variable objetivo


saveRDS(variable_objetivo, "C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/variable_objetivo.rds")
saveRDS(subcodigos_hernia, "C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/desglose_tipos_hernia.rds")
cat("\nVariable objetivo guardada en output/variable_objetivo.rds\n")
cat("Dimensiones:", nrow(variable_objetivo), "x", ncol(variable_objetivo), "\n")
