
# Taller 3 - Minería de Datos
# PARTE 3: Análisis descriptivo
# Jorge Andres Sanchez Duarte

library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(knitr)
library(scales)

datos_limpios     <- readRDS("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/datos_limpios.rds")
variable_objetivo <- readRDS("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/variable_objetivo.rds")


datos <- datos_limpios |>
  left_join(variable_objetivo, by = "CHAVE_FUNCIONAL")


# Conteos básicos: beneficiarios, utilizaciones, procedimientos


n_beneficiarios <- n_distinct(datos$CHAVE_FUNCIONAL)
n_utilizaciones <- datos |> distinct(CHAVE_FUNCIONAL, DT_UTILIZACAO) |> nrow()
n_procedimientos <- nrow(datos)

conteos_basicos <- tibble::tibble(
  concepto = c("Beneficiarios", "Utilizaciones (beneficiario x fecha)", "Procedimientos (filas)"),
  n = c(n_beneficiarios, n_utilizaciones, n_procedimientos)
)
kable(conteos_basicos, caption = "Conteos básicos de la base analizada", format.args = list(big.mark = ","))


#  Distribución de la variable objetivo

tabla_objetivo <- variable_objetivo |>
  count(hernia) |>
  mutate(
    porcentaje = round(n / sum(n) * 100, 3),
    etiqueta   = if_else(hernia == 1, "Con hernia (K40-K46)", "Sin hernia")
  )

kable(tabla_objetivo, caption = "Distribución de la variable objetivo (a nivel de beneficiario)")

cat(sprintf(
  "\nDe %s beneficiarios, %s (%.3f%%) tienen al menos una transacción con CID del bloque K40-K46 (alguna hernia).\n",
  format(n_beneficiarios, big.mark = ","),
  format(sum(variable_objetivo$hernia), big.mark = ","),
  tabla_objetivo$porcentaje[tabla_objetivo$hernia == 1]
))


grafico_objetivo <- ggplot(tabla_objetivo, aes(x = etiqueta, y = n, fill = etiqueta)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(comma(n), " (", porcentaje, "%)")), vjust = -0.6, size = 3.8) +
  scale_fill_manual(values = c("Con hernia (K40-K46)" = "#dc2626", "Sin hernia" = "#2563eb")) +

  scale_y_log10(labels = comma, expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Distribución de la variable objetivo",
    subtitle = "Hernias K40-K46 (bloque completo) a nivel de beneficiario - eje Y en escala logarítmica",
    x = NULL, y = "Número de beneficiarios (escala log10)"
  ) +
  theme_minimal(base_size = 13)

ggsave("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/fig_01_distribucion_objetivo.png", grafico_objetivo, width = 7, height = 5, dpi = 150)
print(grafico_objetivo)


# Composición de la variable objetivo por tipo de hernia

desglose_tipos_hernia <- readRDS("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/desglose_tipos_hernia.rds")

resumen_tipos_hernia <- desglose_tipos_hernia |>
  group_by(tipo_hernia) |>
  summarise(n_transacciones = sum(n_transacciones), .groups = "drop") |>
  mutate(pct_transacciones = round(n_transacciones / sum(n_transacciones) * 100, 1)) |>
  arrange(desc(n_transacciones))

cat("\nComposición de la variable objetivo por tipo de hernia \n")
cat("(en número de TRANSACCIONES, no de beneficiarios; un beneficiario",
    "puede tener varias transacciones o incluso más de un subtipo)\n")
kable(resumen_tipos_hernia, caption = "Transacciones por tipo de hernia dentro del bloque K40-K46")

grafico_tipos_hernia <- ggplot(resumen_tipos_hernia, aes(x = reorder(tipo_hernia, n_transacciones), y = n_transacciones)) +
  geom_col(fill = "#dc2626") +
  geom_text(aes(label = paste0(comma(n_transacciones), " (", pct_transacciones, "%)")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = "Composición de la variable objetivo por tipo de hernia",
    subtitle = "Transacciones con CID en el bloque K40-K46",
    x = NULL, y = "Número de transacciones"
  ) +
  theme_minimal(base_size = 13)

ggsave("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/fig_01b_composicion_tipos_hernia.png", grafico_tipos_hernia, width = 8, height = 5, dpi = 150)
print(grafico_tipos_hernia)

# Distribución por sexo, edad, tipo de beneficiario, estado y especialidad

fecha_referencia <- max(datos_limpios$DT_UTILIZACAO, na.rm = TRUE)
cat("\nFecha de referencia usada para calcular la edad:", as.character(fecha_referencia), "\n")

beneficiarios <- datos_limpios |>
  group_by(CHAVE_FUNCIONAL) |>
  summarise(
    SEXO_BENEFICIARIO   = first(SEXO_BENEFICIARIO),
    TIPO_BENEFICIARIO   = first(TIPO_BENEFICIARIO),
    DT_NASCIMENTO       = first(DT_NASCIMENTO_BENEFICIARIO),
   
    UF_CNES_PREST_HOSPITALAR = {
      tab <- table(UF_CNES_PREST_HOSPITALAR)
      if (length(tab) == 0) NA_character_ else names(tab)[which.max(tab)]
    },
    .groups = "drop"
  ) |>
  mutate(
    edad = as.numeric(difftime(fecha_referencia, DT_NASCIMENTO, units = "days")) / 365.25
  ) |>
  left_join(variable_objetivo, by = "CHAVE_FUNCIONAL")

cat("\nResumen de edad (todos los beneficiarios):\n")
print(summary(beneficiarios$edad))

# 6.1 Sexo 
tabla_sexo <- table(beneficiarios$SEXO_BENEFICIARIO, beneficiarios$hernia, useNA = "ifany")
cat("\nTabla de contingencia: sexo x variable objetivo\n")
print(tabla_sexo)
cat("\nProporciones por fila (dentro de cada sexo, % con hernia):\n")
print(round(prop.table(tabla_sexo, margin = 1) * 100, 3))


test_sexo <- suppressWarnings(chisq.test(tabla_sexo, simulate.p.value = TRUE, B = 2000))
cat("\nPrueba chi-cuadrado (sexo vs. hernia), p-valor simulado:\n")
print(test_sexo)

# Tipo de beneficiario 
tabla_tipo <- table(beneficiarios$TIPO_BENEFICIARIO, beneficiarios$hernia, useNA = "ifany")
cat("\nTabla de contingencia: tipo de beneficiario x variable objetivo\n")
print(tabla_tipo)
cat("\nProporciones por fila:\n")
print(round(prop.table(tabla_tipo, margin = 1) * 100, 3))

# Estado / UF del prestador hospitalario 
tabla_uf <- beneficiarios |>
  count(UF_CNES_PREST_HOSPITALAR, hernia) |>
  group_by(UF_CNES_PREST_HOSPITALAR) |>
  mutate(pct_dentro_uf = round(n / sum(n) * 100, 2)) |>
  ungroup() |>
  arrange(desc(n))

cat("\nDistribución por estado (UF) del prestador más frecuente por beneficiario",
    "(top 10 estados con más beneficiarios):\n")
print(head(tabla_uf |> filter(hernia == 0) |> arrange(desc(n)), 10))

# Especialidad (a nivel de utilización, no de beneficiario)

tabla_especialidad <- datos |>
  filter(!is.na(DESC_ESPECIALIDADE)) |>
  count(hernia, DESC_ESPECIALIDADE) |>
  group_by(hernia) |>
  mutate(pct = round(n / sum(n) * 100, 2)) |>
  ungroup() |>
  arrange(hernia, desc(n))

cat("\nTop 10 especialidades más frecuentes EN BENEFICIARIOS CON hernia:\n")
print(tabla_especialidad |> filter(hernia == 1) |> slice_head(n = 10))
cat("\nTop 10 especialidades más frecuentes EN BENEFICIARIOS SIN hernia:\n")
print(tabla_especialidad |> filter(hernia == 0) |> slice_head(n = 10))

# Visualizaciones: edad y sexo por grupo 

beneficiarios_etiquetado <- beneficiarios |>
  mutate(grupo = if_else(hernia == 1, "Con hernia", "Sin hernia"))

grafico_edad <- ggplot(beneficiarios_etiquetado, aes(x = edad, fill = grupo)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.6, position = "identity") +
  geom_vline(
    data = beneficiarios_etiquetado |> group_by(grupo) |> summarise(media = mean(edad, na.rm = TRUE)),
    aes(xintercept = media, color = grupo), linetype = "dashed", linewidth = 1
  ) +
  scale_fill_manual(values = c("Con hernia" = "#dc2626", "Sin hernia" = "#2563eb")) +
  scale_color_manual(values = c("Con hernia" = "#dc2626", "Sin hernia" = "#2563eb")) +
  labs(
    title = "Distribución de edad por grupo",
    subtitle = "Líneas discontinuas: edad media de cada grupo",
    x = "Edad (años)", y = "Densidad", fill = "Grupo", color = "Grupo"
  ) +
  theme_minimal(base_size = 13)

ggsave("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/fig_02_edad_por_grupo.png", grafico_edad, width = 8, height = 5, dpi = 150)
print(grafico_edad)

# Boxplot de edad por sexo y grupo (combinado), con jitter para ver los 25
# puntos del grupo positivo individualmente.
grafico_edad_sexo <- beneficiarios_etiquetado |>
  filter(!is.na(SEXO_BENEFICIARIO), SEXO_BENEFICIARIO %in% c("M", "F")) |>
  ggplot(aes(x = SEXO_BENEFICIARIO, y = edad, fill = grupo)) +
  geom_boxplot(outlier.alpha = 0.3, position = position_dodge(width = 0.8)) +
  labs(
    title = "Edad por sexo y grupo",
    x = "Sexo", y = "Edad (años)", fill = "Grupo"
  ) +
  theme_minimal(base_size = 13)

ggsave("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/fig_03_edad_por_sexo_grupo.png", grafico_edad_sexo, width = 8, height = 5, dpi = 150)
print(grafico_edad_sexo)


# Análisis de valores faltantes (post-limpieza)

resumen_na_post <- datos_limpios |>
  purrr::map_df(~ sum(is.na(.x))) |>
  pivot_longer(everything(), names_to = "columna", values_to = "n_na") |>
  mutate(pct_na = round(n_na / nrow(datos_limpios) * 100, 1)) |>
  filter(n_na > 0) |>
  arrange(desc(pct_na))

cat("\n Valores faltantes en la base ya limpia \n")
kable(resumen_na_post, caption = "Valores NA reales tras la limpieza (script 1)")

cat("\nFilas completas (sin ningún NA) tras la limpieza:",
    sum(complete.cases(datos_limpios)),
    sprintf("(%.1f%% del total)\n", 100 * sum(complete.cases(datos_limpios)) / nrow(datos_limpios)))

# Beneficiarios sin ninguna fecha de nacimiento válida (se perdió por ser
# centinela 1900-01-01 o por ser posterior a la utilización, ver script 1),
# y por tanto sin edad calculable:
cat("\nBeneficiarios sin edad calculable:", sum(is.na(beneficiarios$edad)),
    sprintf("de %s (%.2f%%)\n", n_beneficiarios,
            100 * sum(is.na(beneficiarios$edad)) / n_beneficiarios))


# Identificación de inconsistencias en los datos


n_sexo_inconsist <- beneficiarios |>
  select(CHAVE_FUNCIONAL) |>
  inner_join(
    datos_limpios |>
      group_by(CHAVE_FUNCIONAL) |>
      summarise(n_sexos = n_distinct(SEXO_BENEFICIARIO), .groups = "drop") |>
      filter(n_sexos > 1),
    by = "CHAVE_FUNCIONAL"
  ) |>
  nrow()


cat("- El código CID venía en formatos inconsistentes (con/sin punto decimal",
    "de la notación CIE-10, y con variantes de texto como 'N/A' o '-');",
    "se normalizó antes de construir la variable objetivo.\n")
cat(sprintf(
  "- Se detectaron fechas de nacimiento centinela (1900-01-01) y fechas de\n  nacimiento posteriores a la fecha de la utilización (lógicamente\n  imposible); ambas se convirtieron a NA. Como resultado, %s beneficiarios\n  (%.2f%%) de esta corrida quedan sin edad calculable.\n",
  format(sum(is.na(beneficiarios$edad)), big.mark = ","),
  100 * sum(is.na(beneficiarios$edad)) / n_beneficiarios
))
cat("- Se detectaron y eliminaron filas duplicadas exactas (idénticas en",
    "las 16 columnas); ver la tabla resumen del script 1 para la cifra",
    "exacta en la corrida actual (la alternativa de conservarlas queda",
    "documentada y comentada en ese script).\n")

cat("\nEdad de los", sum(beneficiarios$hernia == 1), "beneficiarios con hernia (verificación de",
    "plausibilidad clínica):\n")
print(beneficiarios |> filter(hernia == 1) |> pull(edad) |> summary())


# Análisis de valores extremos en VALOR_UTILIZACAO

q <- quantile(datos$VALOR_UTILIZACAO, probs = c(0.25, 0.5, 0.75, 0.9, 0.99, 0.999), na.rm = TRUE)
iqr_valor <- IQR(datos$VALOR_UTILIZACAO, na.rm = TRUE)
limite_superior_iqr <- q["75%"] + 1.5 * iqr_valor

cat("\n VALOR_UTILIZACAO: percentiles y límite de outlier (regla 1.5*IQR) -\n")
print(q)
cat("IQR:", round(iqr_valor, 2), "\n")
cat("Límite superior (Q3 + 1.5*IQR):", round(limite_superior_iqr, 2), "\n")
cat("Filas por encima del límite superior:",
    sum(datos$VALOR_UTILIZACAO > limite_superior_iqr, na.rm = TRUE),
    sprintf("(%.2f%% del total)\n",
            100 * sum(datos$VALOR_UTILIZACAO > limite_superior_iqr, na.rm = TRUE) / nrow(datos)))


grafico_valor <- datos |>
  filter(VALOR_UTILIZACAO > 0) |> # log no está definido en 0 o negativos
  mutate(grupo = if_else(hernia == 1, "Con hernia", "Sin hernia")) |>
  ggplot(aes(x = grupo, y = VALOR_UTILIZACAO, fill = grupo)) +
  geom_boxplot(outlier.alpha = 0.15, show.legend = FALSE) +
  scale_y_log10(labels = comma) +
  scale_fill_manual(values = c("Con hernia" = "#dc2626", "Sin hernia" = "#2563eb")) +
  labs(
    title = "VALOR_UTILIZACAO por grupo (procedimientos con valor > 0)",
    subtitle = "Eje Y en escala logarítmica por el fuerte sesgo a la derecha de los costos",
    x = NULL, y = "Valor de la utilización (log10)"
  ) +
  theme_minimal(base_size = 13)

ggsave("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/fig_04_valor_utilizacao_por_grupo.png", grafico_valor, width = 8, height = 5, dpi = 150)
print(grafico_valor)


