# Taller 3 - Minería de Datos 
# PARTE 4: Construcción de variables (feature engineering)
# Jorge Andres Sanchez Duarte

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(knitr)

datos_limpios     <- readRDS("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/datos_limpios.rds")
variable_objetivo <- readRDS("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/variable_objetivo.rds")

#Variables demográficas: edad, sexo, tipo de beneficiario

fecha_referencia <- max(datos_limpios$DT_UTILIZACAO, na.rm = TRUE)

vars_demograficas <- datos_limpios |>
  group_by(CHAVE_FUNCIONAL) |>
  summarise(
    sexo               = first(SEXO_BENEFICIARIO),
    tipo_beneficiario  = first(TIPO_BENEFICIARIO),
    fecha_nacimiento   = first(DT_NASCIMENTO_BENEFICIARIO),
    .groups = "drop"
  ) |>
  mutate(
    edad = as.numeric(difftime(fecha_referencia, fecha_nacimiento, units = "days")) / 365.25
  ) |>
  select(-fecha_nacimiento)

cat("Variables demográficas construidas para", nrow(vars_demograficas), "beneficiarios\n")
glimpse(vars_demograficas)

#Variables de volumen: número de utilizaciones y de procedimientos

vars_volumen <- datos_limpios |>
  group_by(CHAVE_FUNCIONAL) |>
  summarise(
    n_procedimientos = n(),                              # filas totales
    n_utilizaciones  = n_distinct(DT_UTILIZACAO),         # fechas distintas de atención
    procedimientos_por_utilizacion = n_procedimientos / n_utilizaciones,
    .groups = "drop"
  )

cat("\nResumen de n_utilizaciones (por beneficiario):\n")
print(summary(vars_volumen$n_utilizaciones))
cat("\nResumen de n_procedimientos (por beneficiario):\n")
print(summary(vars_volumen$n_procedimientos))

# Variables clínicas: internaciones, UCI, porte anestésico, tipo de
#    utilización (CETIPO)

vars_clinicas <- datos_limpios |>
  group_by(CHAVE_FUNCIONAL) |>
  summarise(
    tuvo_internacion   = as.integer(any(INTERNADO == 1, na.rm = TRUE)),
    n_internaciones    = sum(INTERNADO == 1, na.rm = TRUE),
    tuvo_uci           = as.integer(any(UTI == 1, na.rm = TRUE)),
    n_uci              = sum(UTI == 1, na.rm = TRUE),
    porte_anestesico_max     = suppressWarnings(max(PORTE_ANESTESICO, na.rm = TRUE)),
    porte_anestesico_promedio = mean(PORTE_ANESTESICO, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    
    porte_anestesico_max = if_else(is.infinite(porte_anestesico_max), NA_real_, porte_anestesico_max),
    porte_anestesico_promedio = if_else(is.nan(porte_anestesico_promedio), NA_real_, porte_anestesico_promedio)
  )

cat("\nBeneficiarios con al menos una internación:", sum(vars_clinicas$tuvo_internacion),
    sprintf("(%.2f%%)\n", 100 * mean(vars_clinicas$tuvo_internacion)))
cat("Beneficiarios con al menos un paso por UCI:", sum(vars_clinicas$tuvo_uci),
    sprintf("(%.2f%%)\n", 100 * mean(vars_clinicas$tuvo_uci)))

# Distribución de CETIPO (tipo de utilización: Consulta, Exame, Terapia,
# Internação, Pronto Socorro, Outros)
vars_cetipo <- datos_limpios |>
  count(CHAVE_FUNCIONAL, CETIPO) |>
  pivot_wider(
    names_from  = CETIPO,
    values_from = n,
    values_fill = 0,
    names_prefix = "n_cetipo_"
  ) |>
  rename_with(
    ~ .x |>
      str_to_lower() |>
      str_replace_all(" ", "_") |>
      str_replace_all("ã", "a") |>
      str_replace_all("ç", "c"),
    starts_with("n_cetipo_")
  )

cat("\nVariables de CETIPO (conteo por beneficiario) construidas. Columnas:\n")
print(names(vars_cetipo))

# Variables de especialidad consultada
#
top_especialidades <- datos_limpios |>
  filter(!is.na(DESC_ESPECIALIDADE)) |>
  count(DESC_ESPECIALIDADE, sort = TRUE) |>
  slice_head(n = 8) |>
  pull(DESC_ESPECIALIDADE)

cat("\nTop 8 especialidades usadas para las variables de conteo:\n")
print(top_especialidades)

vars_especialidad_top <- datos_limpios |>
  filter(!is.na(DESC_ESPECIALIDADE)) |>
  mutate(
    especialidad_agrupada = if_else(
      DESC_ESPECIALIDADE %in% top_especialidades,
      DESC_ESPECIALIDADE,
      "otras"
    )
  ) |>
  count(CHAVE_FUNCIONAL, especialidad_agrupada) |>
  pivot_wider(
    names_from  = especialidad_agrupada,
    values_from = n,
    values_fill = 0,
    names_prefix = "n_esp_"
  ) |>
  rename_with(
    ~ .x |>
      str_to_lower() |>
    
      str_replace_all(c("á"="a","é"="e","í"="i","ó"="o","ú"="u","ã"="a","ç"="c")) |>
      str_replace_all("[^a-z0-9_]+", "_") |>
      str_replace_all("_+", "_") |>
      str_remove("_$"),
    starts_with("n_esp_")
  )

vars_especialidad_diversidad <- datos_limpios |>
  filter(!is.na(DESC_ESPECIALIDADE)) |>
  group_by(CHAVE_FUNCIONAL) |>
  summarise(n_especialidades_distintas = n_distinct(DESC_ESPECIALIDADE), .groups = "drop")

cat("\nColumnas de especialidad (top + otras):\n")
print(names(vars_especialidad_top))

#Variables de costo: acumulado y promedio

vars_costos <- datos_limpios |>
  group_by(CHAVE_FUNCIONAL) |>
  summarise(
    costo_total          = sum(VALOR_UTILIZACAO, na.rm = TRUE),
    costo_total_sin_neg  = sum(pmax(VALOR_UTILIZACAO, 0), na.rm = TRUE),
    costo_promedio       = mean(VALOR_UTILIZACAO, na.rm = TRUE),
    costo_maximo         = max(VALOR_UTILIZACAO, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nResumen de costo_total por beneficiario:\n")
print(summary(vars_costos$costo_total))

# Variables de estado (UF) y tipo de unidad hospitalaria
#
moda_categoria <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  tab <- table(x)
  names(tab)[which.max(tab)]
}

vars_uf_unidad <- datos_limpios |>
  group_by(CHAVE_FUNCIONAL) |>
  summarise(
    uf_mas_frecuente         = moda_categoria(UF_CNES_PREST_HOSPITALAR),
    n_uf_distintas           = n_distinct(UF_CNES_PREST_HOSPITALAR, na.rm = TRUE),
    tipo_unidad_mas_frecuente = moda_categoria(TIPO_UNIDADE_PREST_HOSPITALAR),
    n_tipos_unidad_distintos  = n_distinct(TIPO_UNIDADE_PREST_HOSPITALAR, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nDistribución de uf_mas_frecuente (top 10):\n")
print(vars_uf_unidad |> count(uf_mas_frecuente, sort = TRUE) |> head(10))
cat("\nDistribución de tipo_unidad_mas_frecuente (top 10):\n")
print(vars_uf_unidad |> count(tipo_unidad_mas_frecuente, sort = TRUE) |> head(10))

# Unión de todas las variables en un único dataset a nivel de beneficiario

dataset_modelamiento <- vars_demograficas |>
  left_join(vars_volumen, by = "CHAVE_FUNCIONAL") |>
  left_join(vars_clinicas, by = "CHAVE_FUNCIONAL") |>
  left_join(vars_cetipo, by = "CHAVE_FUNCIONAL") |>
  left_join(vars_especialidad_top, by = "CHAVE_FUNCIONAL") |>
  left_join(vars_especialidad_diversidad, by = "CHAVE_FUNCIONAL") |>
  left_join(vars_costos, by = "CHAVE_FUNCIONAL") |>
  left_join(vars_uf_unidad, by = "CHAVE_FUNCIONAL") |>
  left_join(variable_objetivo, by = "CHAVE_FUNCIONAL")

cat("\n Dataset de modelamiento construido \n")
cat("Filas (beneficiarios):", nrow(dataset_modelamiento), "\n")
cat("Columnas:", ncol(dataset_modelamiento), "\n")
stopifnot(nrow(dataset_modelamiento) == n_distinct(datos_limpios$CHAVE_FUNCIONAL))


# Los conteos de especialidad (vars_especialidad_top / diversidad) y de
# UCI/internación pueden quedar en NA para beneficiarios cuyas filas eran

cols_conteo_esp <- names(dataset_modelamiento) |>
  (\(x) x[str_starts(x, "n_esp_")])()

dataset_modelamiento <- dataset_modelamiento |>
  mutate(
    across(all_of(cols_conteo_esp), ~ replace_na(.x, 0)),
    n_especialidades_distintas = replace_na(n_especialidades_distintas, 0)
  )

glimpse(dataset_modelamiento)

# Verificación explícita de que NO hay fuga de información

columnas_prohibidas <- c("CID") # el CID crudo nunca debe estar en el dataset final
cat("\n Verificación de fuga de información \n")
cat("¿El dataset de modelamiento contiene la columna CID?",
    any(columnas_prohibidas %in% names(dataset_modelamiento)), "\n")
stopifnot(!any(columnas_prohibidas %in% names(dataset_modelamiento)))
cat("OK: el CID no está presente como variable en el dataset de",
    "modelamiento. La única información relacionada con el diagnóstico",
    "que se conserva es 'hernia_inguinal', y es la ETIQUETA, no un",
    "predictor.\n")

# Guardar el dataset final

saveRDS(dataset_modelamiento, "C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/dataset_modelamiento.rds")
write_csv(dataset_modelamiento, "C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/dataset_modelamiento.csv")

cat("Dimensiones finales:", nrow(dataset_modelamiento), "x", ncol(dataset_modelamiento), "\n")
