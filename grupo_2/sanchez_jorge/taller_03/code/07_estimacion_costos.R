
# Taller 3 - Minería de Datos 
# PARTE 7: Estimación de costos
# Jorge Andres Sanchez Duarte

library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(scales)
library(knitr)

datos_limpios     <- readRDS("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/datos_limpios.rds")
variable_objetivo <- readRDS("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/variable_objetivo.rds")

datos <- datos_limpios |>
  left_join(variable_objetivo, by = "CHAVE_FUNCIONAL")


codigo_cid_objetivo <- "^K4[0-6]"

procedimientos_hernia <- datos |>
  filter(str_detect(CID, codigo_cid_objetivo))

cat("Procedimientos asociados directamente a un CID de hernia (K40-K46):",
    nrow(procedimientos_hernia), "\n")
cat("Total de procedimientos en la base:", nrow(datos), "\n")

#Costo promedio observado y su variabilidad
resumen_costo_hernia <- procedimientos_hernia |>
  summarise(
    n                = n(),
    n_negativos      = sum(VALOR_UTILIZACAO < 0),
    n_ceros          = sum(VALOR_UTILIZACAO == 0),
    costo_promedio   = mean(VALOR_UTILIZACAO),
    costo_mediana    = median(VALOR_UTILIZACAO),
    costo_sd         = sd(VALOR_UTILIZACAO),
    costo_min        = min(VALOR_UTILIZACAO),
    costo_p25        = quantile(VALOR_UTILIZACAO, 0.25),
    costo_p75        = quantile(VALOR_UTILIZACAO, 0.75),
    costo_p95        = quantile(VALOR_UTILIZACAO, 0.95),
    costo_max         = max(VALOR_UTILIZACAO)
  )

resumen_costo_general <- datos |>
  summarise(
    n                = n(),
    costo_promedio   = mean(VALOR_UTILIZACAO),
    costo_mediana    = median(VALOR_UTILIZACAO),
    costo_sd         = sd(VALOR_UTILIZACAO),
    costo_p95        = quantile(VALOR_UTILIZACAO, 0.95)
  )

cat("\n Costo por procedimiento: hernia (K40-K46) \n")
kable(resumen_costo_hernia |> mutate(across(where(is.numeric), ~round(.x, 2))),
      caption = "Estadísticos de VALOR_UTILIZACAO en procedimientos de hernia")

cat("\nCosto por procedimiento: toda la base (referencia) \n")
kable(resumen_costo_general |> mutate(across(where(is.numeric), ~round(.x, 2))),
      caption = "Estadísticos de VALOR_UTILIZACAO en toda la base")

cat(sprintf(
  "\nEl costo promedio de un procedimiento asociado a hernia (%s) es %.1fx el",
  comma(round(resumen_costo_hernia$costo_promedio, 2)),
  resumen_costo_hernia$costo_promedio / resumen_costo_general$costo_promedio
))
cat(sprintf(" costo promedio de un procedimiento típico en la base (%s).\n",
            comma(round(resumen_costo_general$costo_promedio, 2))))

# Coeficiente de variación (SD/media) 
cv_hernia  <- resumen_costo_hernia$costo_sd / resumen_costo_hernia$costo_promedio
cv_general <- resumen_costo_general$costo_sd / resumen_costo_general$costo_promedio
cat(sprintf("\nCoeficiente de variación - hernia: %.2f | general: %.2f\n", cv_hernia, cv_general))

# Visualización de la distribución del costo (hernia vs. general)

datos_comparacion_costo <- bind_rows(
  procedimientos_hernia |> select(VALOR_UTILIZACAO) |> mutate(grupo = "Procedimientos de hernia"),
  datos |> select(VALOR_UTILIZACAO) |> mutate(grupo = "Todos los procedimientos")
)

grafico_costo_hernia <- datos_comparacion_costo |>
  filter(VALOR_UTILIZACAO > 0) |>
  ggplot(aes(x = VALOR_UTILIZACAO, fill = grupo)) +
  geom_density(alpha = 0.5) +
  geom_vline(
    data = datos_comparacion_costo |> filter(VALOR_UTILIZACAO > 0) |> group_by(grupo) |> summarise(media = mean(VALOR_UTILIZACAO)),
    aes(xintercept = media, color = grupo), linetype = "dashed", linewidth = 1
  ) +
  scale_x_log10(labels = comma) +
  scale_fill_manual(values = c("Procedimientos de hernia" = "#dc2626", "Todos los procedimientos" = "#2563eb")) +
  scale_color_manual(values = c("Procedimientos de hernia" = "#dc2626", "Todos los procedimientos" = "#2563eb")) +
  labs(
    title = "Distribución del costo por procedimiento",
    subtitle = "Hernia (K40-K46) vs. todos los procedimientos -- eje X en escala log10",
    x = "VALOR_UTILIZACAO (log10)", y = "Densidad", fill = "Grupo", color = "Grupo"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", legend.direction = "vertical")

ggsave("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/fig_11_costo_hernia_vs_general.png", grafico_costo_hernia, width = 8, height = 6, dpi = 150)
print(grafico_costo_hernia)

# Estrategia para estimar el costo esperado bajo distintos perfiles

datos_modelo_costo <- procedimientos_hernia |>
  filter(VALOR_UTILIZACAO > 0) |>
  mutate(
    log_costo = log(VALOR_UTILIZACAO),
    porte_anestesico_cat = if_else(is.na(PORTE_ANESTESICO), "sin_dato", as.character(PORTE_ANESTESICO)),
    sexo = if_else(is.na(SEXO_BENEFICIARIO), "sin_dato", SEXO_BENEFICIARIO),
    edad = as.numeric(difftime(DT_UTILIZACAO, DT_NASCIMENTO_BENEFICIARIO, units = "days")) / 365.25,
    tipo_unidad_agrupado = case_when(
      is.na(TIPO_UNIDADE_PREST_HOSPITALAR) ~ "sin_dato",
      TIPO_UNIDADE_PREST_HOSPITALAR == "HOSPITAL GERAL" ~ "HOSPITAL GERAL",
      TRUE ~ "otro"
    )
  )

cat("\nCasos disponibles para el modelo de costo (con VALOR_UTILIZACAO > 0):",
    nrow(datos_modelo_costo), "\n")
cat("Edad: NA en", sum(is.na(datos_modelo_costo$edad)), "casos (se excluyen del modelo)\n")

modelo_costo <- lm(
  log_costo ~ porte_anestesico_cat + sexo + edad + tipo_unidad_agrupado,
  data = datos_modelo_costo
)

cat("\nModelo de regresión lineal: log(costo) ~ perfil del procedimiento\n")
print(summary(modelo_costo))



# Tabla de coeficientes con su interpretación en porcentaje
coef_costo <- summary(modelo_costo)$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column("variable") |>
  filter(variable != "(Intercept)") |>
  mutate(cambio_pct = round((exp(Estimate) - 1) * 100, 1)) |>
  arrange(`Pr(>|t|)`)

cat("\n Efecto de cada variable sobre el costo, en porcentaje \n")
kable(coef_costo |> select(variable, Estimate, `Pr(>|t|)`, cambio_pct) |>
        mutate(across(where(is.numeric), ~round(.x, 4))),
      caption = "Cambio porcentual estimado en el costo por cada variable")

# Estimación del costo esperado bajo perfiles concretos

perfiles <- tibble::tribble(
  ~descripcion, ~porte_anestesico_cat, ~sexo, ~edad, ~tipo_unidad_agrupado,
  "Sin cirugía mayor, mujer adulta, hospital general",   "sin_dato", "F", 40, "HOSPITAL GERAL",
  "Cirugía menor (porte 2), hombre adulto, hospital general", "2", "M", 40, "HOSPITAL GERAL",
  "Cirugía compleja (porte 5), hombre adulto, hospital general", "5", "M", 40, "HOSPITAL GERAL",
  "Cirugía compleja (porte 5), mujer adulta, hospital general", "5", "F", 40, "HOSPITAL GERAL",
  "Cirugía compleja (porte 5), hombre mayor (70 años), hospital general", "5", "M", 70, "HOSPITAL GERAL",
  "Cirugía compleja (porte 5), hombre adulto, otro tipo de unidad", "5", "M", 40, "otro"
)

pred_log <- predict(modelo_costo, newdata = perfiles, se.fit = TRUE)

# Corrección de Duan (smearing estimator)

factor_duan <- mean(exp(residuals(modelo_costo)))
cat("\nFactor de corrección de Duan (smearing estimator):", round(factor_duan, 3), "\n")

perfiles <- perfiles |>
  mutate(
    costo_esperado          = exp(pred_log$fit) * factor_duan,
    costo_esperado_ic_inf   = exp(pred_log$fit - 1.96 * pred_log$se.fit) * factor_duan,
    costo_esperado_ic_sup   = exp(pred_log$fit + 1.96 * pred_log$se.fit) * factor_duan
  )

cat("\n Costo esperado bajo distintos perfiles (con corrección de Duan) -\n")
kable(perfiles |> select(descripcion, costo_esperado, costo_esperado_ic_inf, costo_esperado_ic_sup) |>
        mutate(across(where(is.numeric), ~round(.x, 0))),
      caption = "Costo esperado por procedimiento bajo distintos perfiles clínicos")

grafico_perfiles <- ggplot(perfiles, aes(x = reorder(descripcion, costo_esperado), y = costo_esperado)) +
  geom_col(fill = "#dc2626") +
  geom_errorbar(aes(ymin = costo_esperado_ic_inf, ymax = costo_esperado_ic_sup), width = 0.3) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Costo esperado por procedimiento de hernia, según perfil",
    subtitle = "Barras de error: intervalo de confianza del 95%",
    x = NULL, y = "Costo esperado (VALOR_UTILIZACAO)"
  ) +
  theme_minimal(base_size = 12)

ggsave("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/fig_12_costo_esperado_perfiles.png", grafico_perfiles, width = 9, height = 5, dpi = 150)
print(grafico_perfiles)
