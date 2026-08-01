
# Taller 3 - Minería de Datos 
# PARTE 1: Calidad de datos y limpieza
# Jorge Andres Sanchez Duarte


# Librerías

library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(tidyr)
library(ggplot2)
library(knitr)


# datos

ruta_datos <- "C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/db_2026.csv"

datos_raw <- read_csv(
  ruta_datos,
  col_types = cols(
    CID                            = col_character(),
    UTI                            = col_integer(),
    INTERNADO                      = col_integer(),
    PORTE_ANESTESICO               = col_double(),
    DT_UTILIZACAO                  = col_character(), # se parsea manualmente más abajo
    DESC_ESPECIALIDADE             = col_character(),
    TIPO_UNIDADE_PREST_HOSPITALAR  = col_character(),
    UF_CNES_PREST_HOSPITALAR       = col_character(),
    DT_NASCIMENTO_BENEFICIARIO     = col_character(), # idem
    TIPO_BENEFICIARIO              = col_character(),
    SEXO_BENEFICIARIO              = col_character(),
    CETIPO                         = col_character(),
    CD_PROCEDIMENTO                = col_character(), # es un código, no una cantidad
    DESCRICAO_PROCEDIMENTO         = col_character(),
    VALOR_UTILIZACAO               = col_double(),
    CHAVE_FUNCIONAL                = col_character()
  )
)


cat("Dimensiones de la base cruda:", nrow(datos_raw), "filas x", ncol(datos_raw), "columnas\n")
glimpse(datos_raw)

# Diagnóstico inicial de calidad (antes de limpiar)
# Mapa de valores faltantes "reales" ( por columna,
    
resumen_na <- datos_raw |>
  map_df(~ sum(is.na(.x))) |>
  pivot_longer(everything(), names_to = "columna", values_to = "n_na") |>
  mutate(
    pct_na   = round(n_na / nrow(datos_raw) * 100, 1),
    tiene_na = n_na > 0
  ) |>
  filter(tiene_na) |>
  arrange(desc(pct_na))

kable(resumen_na, caption = "Valores NA reales (detectados por is.na) por columna")

# "NA disfrazados": códigos que representan ausencia de información pero
# que R no reconoce como NA porque son strings. 

cat("\n Diagnóstico de codificaciones de 'faltante' disfrazadas de texto \n")
cat("CID == 'N/A':", sum(datos_raw$CID == "N/A", na.rm = TRUE), "\n")
cat("CID == '-':", sum(datos_raw$CID == "-", na.rm = TRUE), "\n")
cat("CID solo espacios en blanco:", sum(str_squish(datos_raw$CID) == "", na.rm = TRUE), "\n")
cat("DESC_ESPECIALIDADE == '-':", sum(datos_raw$DESC_ESPECIALIDADE == "-", na.rm = TRUE), "\n")
cat("TIPO_UNIDADE_PREST_HOSPITALAR == '-':", sum(datos_raw$TIPO_UNIDADE_PREST_HOSPITALAR == "-", na.rm = TRUE), "\n")
cat("UF_CNES_PREST_HOSPITALAR == '-':", sum(datos_raw$UF_CNES_PREST_HOSPITALAR == "-", na.rm = TRUE), "\n")
cat("TIPO_BENEFICIARIO en {Não Informado, IGNORADO}:",
    sum(datos_raw$TIPO_BENEFICIARIO %in% c("Não Informado", "IGNORADO"), na.rm = TRUE), "\n")


cat("\nEjemplo de inconsistencia de formato en CID dentro del bloque K40",
    "(hernia inguinal): el mismo subcódigo aparece escrito de varias formas",
    "-p. ej. 'K409', 'K40.9' y 'K40' compiten por representar la misma idea",
    "clínica (hernia inguinal, con o sin especificar lateralidad/obstrucción):\n")
print(datos_raw |> filter(str_detect(CID, "^K40")) |> count(CID) |> arrange(desc(n)))

# Inconsistencias entre registros del mismo beneficiario.

cat("\nValores no estándar de sexo encontrados (distintos de M/F/Não Informado):\n")
print(datos_raw |>
        filter(!SEXO_BENEFICIARIO %in% c("M", "F", "Não Informado")) |>
        count(SEXO_BENEFICIARIO))


datos_dx <- datos_raw |>
  mutate(SEXO_BENEFICIARIO = case_when(
    SEXO_BENEFICIARIO == "MASCULINO" ~ "M",
    SEXO_BENEFICIARIO == "FEMININO"  ~ "F", 
    TRUE ~ SEXO_BENEFICIARIO
  ))

# Ahora sí, con las categorías ya normalizadas, se mide la inconsistencia
# real de sexo por beneficiario y si la fecha de nacimiento es consistente
# por beneficiario (en teoría debería ser fija para cada persona).
inconsist_sexo <- datos_dx |>
  group_by(CHAVE_FUNCIONAL) |>
  summarise(n_sexos_distintos = n_distinct(SEXO_BENEFICIARIO), .groups = "drop") |>
  filter(n_sexos_distintos > 1)

inconsist_fnac <- datos_raw |>
  group_by(CHAVE_FUNCIONAL) |>
  summarise(n_fnac_distintas = n_distinct(DT_NASCIMENTO_BENEFICIARIO), .groups = "drop") |>
  filter(n_fnac_distintas > 1)

cat("\nBeneficiarios con más de un sexo registrado:", nrow(inconsist_sexo),
    "de", n_distinct(datos_raw$CHAVE_FUNCIONAL), "beneficiarios\n")
cat("Beneficiarios con más de una fecha de nacimiento registrada:", nrow(inconsist_fnac), "\n")

# Duplicados exactos (todas las columnas iguales).
n_duplicados <- sum(duplicated(datos_raw))
cat("\nFilas duplicadas (idénticas en todas las columnas):", n_duplicados,
    sprintf("(%.1f%% del total)\n", 100 * n_duplicados / nrow(datos_raw)))

# 2Categorías finales de SEXO_BENEFICIARIO, ya con "MASCULINO"/"FEMININO"
cat("\nCategorías de SEXO_BENEFICIARIO (tras normalizar MASCULINO/FEMININO):\n")
print(table(datos_dx$SEXO_BENEFICIARIO, useNA = "always"))
rm(datos_dx) # era solo para diagnóstico; la limpieza real ocurre en el pipeline

############################################################################
######################## LIMPIEZA DE DATOS #################################
############################################################################

# Normalización del código CID


normalizar_cid <- function(x) {
  x |>
    na_if("N/A") |>
    na_if("-") |>
    str_squish() |>
    str_remove_all("[.]") |>
    str_to_upper()
}


# Convertir códigos de "faltante" disfrazados en NA reales

limpiar_codigos_faltante <- function(df) {
  df |>
    mutate(
      DESC_ESPECIALIDADE            = na_if(DESC_ESPECIALIDADE, "-"),
      TIPO_UNIDADE_PREST_HOSPITALAR = na_if(TIPO_UNIDADE_PREST_HOSPITALAR, "-"),
      UF_CNES_PREST_HOSPITALAR      = na_if(UF_CNES_PREST_HOSPITALAR, "-"),
      TIPO_BENEFICIARIO              = if_else(
        TIPO_BENEFICIARIO %in% c("Não Informado", "IGNORADO"),
        NA_character_,
        TIPO_BENEFICIARIO
      ),
      SEXO_BENEFICIARIO = case_when(
        SEXO_BENEFICIARIO == "MASCULINO"     ~ "M",
        SEXO_BENEFICIARIO == "FEMININO"      ~ "F",
        SEXO_BENEFICIARIO == "Não Informado" ~ NA_character_,
        TRUE ~ SEXO_BENEFICIARIO
      )
    )
}


# Parseo de fechas y tratamiento de fechas imposibles

parsear_fechas <- function(df) {
  df |>
    mutate(
      DT_UTILIZACAO = ymd(DT_UTILIZACAO),
      DT_NASCIMENTO_BENEFICIARIO = ymd(DT_NASCIMENTO_BENEFICIARIO),
      DT_NASCIMENTO_BENEFICIARIO = if_else(
        DT_NASCIMENTO_BENEFICIARIO == ymd("1900-01-01") |
          DT_NASCIMENTO_BENEFICIARIO > DT_UTILIZACAO,
        as.Date(NA),
        DT_NASCIMENTO_BENEFICIARIO
      )
    )
}


# Resolver inconsistencia de sexo por beneficiario

moda_con_empate_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  tab <- table(x)
  max_frec <- max(tab)
  candidatos <- names(tab[tab == max_frec])
  if (length(candidatos) > 1) return(NA_character_) # empate -> NA
  candidatos
}

resolver_sexo_beneficiario <- function(df) {
  sexo_resuelto <- df |>
    group_by(CHAVE_FUNCIONAL) |>
    summarise(SEXO_RESUELTO = moda_con_empate_na(SEXO_BENEFICIARIO), .groups = "drop")

  df |>
    left_join(sexo_resuelto, by = "CHAVE_FUNCIONAL") |>
    mutate(SEXO_BENEFICIARIO = SEXO_RESUELTO) |>
    select(-SEXO_RESUELTO)
}


# Duplicados exactos: dos alternativas (elegir una)


eliminar_duplicados_exactos <- function(df) {
  df |> distinct()
}

# VALOR_UTILIZACAO: valores negativos, ceros y extremos (se documentan,
#     no se eliminan)

diagnostico_valor_utilizacao <- function(df) {
  df |>
    summarise(
      n_negativos   = sum(VALOR_UTILIZACAO < 0, na.rm = TRUE),
      n_ceros       = sum(VALOR_UTILIZACAO == 0, na.rm = TRUE),
      p50           = median(VALOR_UTILIZACAO, na.rm = TRUE),
      p99           = quantile(VALOR_UTILIZACAO, 0.99, na.rm = TRUE),
      p999          = quantile(VALOR_UTILIZACAO, 0.999, na.rm = TRUE),
      maximo        = max(VALOR_UTILIZACAO, na.rm = TRUE)
    )
}

# PIPELINE COMPLETO

datos_limpios <- datos_raw |>
  mutate(CID = normalizar_cid(CID)) |>
  limpiar_codigos_faltante() |>
  parsear_fechas() |>
  resolver_sexo_beneficiario() |>
  eliminar_duplicados_exactos()
 

cat("\nResultado del pipeline de limpieza \n")
cat("Filas antes  :", nrow(datos_raw), "\n")
cat("Filas después:", nrow(datos_limpios), "\n")
cat("Columnas     :", ncol(datos_limpios), "\n")

cat("\nDiagnóstico de VALOR_UTILIZACAO (se documenta, no se filtra):\n")
print(diagnostico_valor_utilizacao(datos_limpios))

# Verificación de que la limpieza del CID funcionó
cat("\nCódigos K40 después de normalizar (deben verse sin punto, sin N/A, sin '-'):\n")
print(datos_limpios |> filter(str_detect(CID, "^K40")) |> count(CID) |> arrange(desc(n)))

cat("\nBeneficiarios con sexo NA tras resolver inconsistencias por moda",
    "(el empate exacto 50/50 -si lo hay- es el único caso que queda como NA):\n")
cat(sum(is.na(datos_limpios$SEXO_BENEFICIARIO |> unique())), "\n")


# RESUMEN


resumen_limpieza <- tibble::tribble(
  ~problema, ~n_afectado, ~tratamiento,
  "CID = 'N/A' (texto, no NA real)", sum(datos_raw$CID == "N/A", na.rm = TRUE), "Convertido a NA real",
  "CID = '-' (texto, no NA real)", sum(datos_raw$CID == "-", na.rm = TRUE), "Convertido a NA real",
  "CID con formato inconsistente (punto/sin punto)", sum(str_detect(datos_raw$CID, "[.]"), na.rm = TRUE), "Punto eliminado, texto normalizado",
  "DESC_ESPECIALIDADE = '-'", sum(datos_raw$DESC_ESPECIALIDADE == "-", na.rm = TRUE), "Convertido a NA real",
  "TIPO_UNIDADE_PREST_HOSPITALAR = '-'", sum(datos_raw$TIPO_UNIDADE_PREST_HOSPITALAR == "-", na.rm = TRUE), "Convertido a NA real",
  "UF_CNES_PREST_HOSPITALAR = '-'", sum(datos_raw$UF_CNES_PREST_HOSPITALAR == "-", na.rm = TRUE), "Convertido a NA real",
  "TIPO_BENEFICIARIO no informado/ignorado", sum(datos_raw$TIPO_BENEFICIARIO %in% c("Não Informado", "IGNORADO"), na.rm = TRUE), "Convertido a NA real",
  "SEXO_BENEFICIARIO no informado", sum(datos_raw$SEXO_BENEFICIARIO == "Não Informado", na.rm = TRUE), "Convertido a NA real",
  "SEXO_BENEFICIARIO con variante de escritura (MASCULINO/FEMININO)", sum(datos_raw$SEXO_BENEFICIARIO %in% c("MASCULINO", "FEMININO"), na.rm = TRUE), "Normalizado a 'M'/'F'",
  "Beneficiarios con sexo inconsistente (M y F, tras normalizar variantes)", nrow(inconsist_sexo), "Resuelto por moda; empates -> NA",
  "Fecha de nacimiento centinela (1900-01-01)", sum(datos_raw$DT_NASCIMENTO_BENEFICIARIO == "1900-01-01", na.rm = TRUE), "Convertida a NA",
  "Fecha de nacimiento posterior a la utilización", sum(ymd(datos_raw$DT_NASCIMENTO_BENEFICIARIO) > ymd(datos_raw$DT_UTILIZACAO), na.rm = TRUE), "Convertida a NA",
  "Filas duplicadas exactas", n_duplicados, "Eliminadas (alternativa de conservarlas queda comentada en el código)",
  "VALOR_UTILIZACAO negativo", sum(datos_raw$VALOR_UTILIZACAO < 0, na.rm = TRUE), "Conservado y documentado (no se elimina)",
  "VALOR_UTILIZACAO en cero", sum(datos_raw$VALOR_UTILIZACAO == 0, na.rm = TRUE), "Conservado y documentado (no se elimina)"
)

kable(resumen_limpieza, caption = "Resumen de problemas de calidad de datos detectados y su tratamiento")


# GUARDAR BASE LIMPIA 

saveRDS(datos_limpios, "C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/datos_limpios.rds")

cat("Dimensiones finales:", nrow(datos_limpios), "x", ncol(datos_limpios), "\n")
