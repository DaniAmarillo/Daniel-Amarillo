library(tidyverse)
library(purrr)

# Carga de datos 
house   <- read_csv("C:/Users/Johan/Desktop/house_prices.csv",  show_col_types = FALSE)
titanic <- read_csv("C:/Users/Johan/Desktop/titanic.csv",       show_col_types = FALSE)
moviles <- read_csv("C:/Users/Johan/Desktop/mobile_data.csv",   show_col_types = FALSE)


# Resumen de NA por columna
resumen_na_house <- tibble(
  columna = names(house),
  n_na    = map_int(house, ~ sum(is.na(.x))),
  pct_na  = round(n_na / nrow(house) * 100, 1)
) |>
  arrange(desc(pct_na))

top5 <- resumen_na_house |> slice_head(n = 5)
print(top5)

cat("\nJustificación de decisiones:\n")
top5 |>
  mutate(decision = case_when(
    pct_na > 50 ~ "ELIMINAR  — más del 50% de datos faltantes",
    pct_na > 0  ~ "IMPUTAR   — porcentaje manejable",
    TRUE        ~ "OK"
  )) |>
  select(columna, pct_na, decision) |>
  print()

# 1.2 Imputación y recuento de filas completas 

# Función moda
moda <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}
# se carga como string "NA" — tratarlo como verdadero NA
house_limpio <- house |>
  mutate(across(where(is.character), ~ na_if(.x, "NA")))

# Eliminar columnas con > 50% NA
umbral <- 0.50
cols_eliminar <- resumen_na_house |>
  filter(pct_na > umbral * 100) |>
  pull(columna)

cat("Columnas eliminadas (> 50% NA):", paste(cols_eliminar, collapse = ", "), "\n")

house_limpio <- house_limpio |>
  select(-any_of(cols_eliminar))

# Imputar numéricas con mediana, categóricas con moda
house_limpio <- house_limpio |>
  mutate(
    across(where(is.numeric),
           ~ if_else(is.na(.x), median(.x, na.rm = TRUE), .x)),
    across(where(is.character),
           ~ if_else(is.na(.x), moda(.x), .x))
  )

filas_completas <- sum(complete.cases(house_limpio))
cat("Filas completamente completas después de imputar:",
    filas_completas, "de", nrow(house_limpio), "\n")

# ── 1.3 Variable antigüedad y discretización 

house_limpio <- house_limpio |>
  mutate(antiguedad = 2024 - YearBuilt)

cuartiles <- quantile(house_limpio$antiguedad, probs = c(0, 0.25, 0.5, 0.75, 1))
cat("Cuartiles de antigüedad:", round(cuartiles), "\n")

house_limpio <- house_limpio |>
  mutate(
    grupo_antiguedad = cut(
      antiguedad,
      breaks         = cuartiles,
      labels         = c("Reciente", "Moderada", "Antigua", "Muy antigua"),
      include.lowest = TRUE,
      right          = TRUE
    )
  )

resumen_grupos <- house_limpio |>
  count(grupo_antiguedad) |>
  mutate(pct = round(n / sum(n) * 100, 1))
print(resumen_grupos)

# Detección de outliers por IQR en fare

detectar_outliers_iqr <- function(x, k = 1.5) {
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x < (q1 - k * iqr) | x > (q3 + k * iqr)
}

titanic <- titanic |>
  mutate(outlier_fare = detectar_outliers_iqr(fare))

n_outliers <- sum(titanic$outlier_fare, na.rm = TRUE)
pct_out    <- round(n_outliers / nrow(titanic) * 100, 1)

cat("Outliers en fare:", n_outliers, "pasajeros —", pct_out, "%\n")
cat("Límites IQR:\n")
cat("  Q1:", quantile(titanic$fare, 0.25, na.rm = TRUE), "\n")
cat("  Q3:", quantile(titanic$fare, 0.75, na.rm = TRUE), "\n")
q1_f  <- quantile(titanic$fare, 0.25, na.rm = TRUE)
q3_f  <- quantile(titanic$fare, 0.75, na.rm = TRUE)
iqr_f <- q3_f - q1_f
cat("  Límite inferior:", q1_f - 1.5 * iqr_f, "\n")
cat("  Límite superior:", q3_f + 1.5 * iqr_f, "\n")

# 2.2 Tres versiones de fare
cat("\n── 2.2  Distribuciones: original, winsorizada, log ───\n")

titanic_fare <- titanic |>
  mutate(
    fare_winsor = pmin(
      pmax(fare, quantile(fare, 0.05, na.rm = TRUE)),
      quantile(fare, 0.95, na.rm = TRUE)
    ),
    fare_log = log1p(fare)
  )

stats_fare <- titanic_fare |>
  summarise(across(c(fare, fare_winsor, fare_log),
                   list(media = ~ round(mean(.x, na.rm = TRUE), 2),
                        sd    = ~ round(sd(.x, na.rm = TRUE), 2),
                        min   = ~ round(min(.x, na.rm = TRUE), 2),
                        max   = ~ round(max(.x, na.rm = TRUE), 2)),
                   .names = "{.col}__{.fn}")) |>
  pivot_longer(everything(),
               names_to  = c("variable", "estadistico"),
               names_sep = "__") |>
  pivot_wider(names_from = estadistico, values_from = value)

print(stats_fare)

# Gráfico comparativo
p2 <- titanic_fare |>
  select(fare, fare_winsor, fare_log) |>
  pivot_longer(everything(), names_to = "version", values_to = "valor") |>
  mutate(version = factor(version,
                          levels = c("fare", "fare_winsor", "fare_log"),
                          labels = c("Original", "Winsorizada (P5–P95)", "Log(1 + fare)"))) |>
  ggplot(aes(x = valor, fill = version)) +
  geom_density(alpha = 0.7, color = "white") +
  facet_wrap(~ version, scales = "free", nrow = 1) +
  scale_fill_manual(values = c("#2c7bb6", "#d7191c", "#1a9641"), guide = "none") +
  labs(
    title    = "Ejercicio 2 — Distribución de fare según tratamiento de outliers",
    subtitle = "Titanic | Winsorización P5–P95 vs. transformación logarítmica",
    x = "Valor", y = "Densidad"
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))

ggsave("/mnt/user-data/outputs/ej2_fare_distribuciones.png",
       p2, width = 11, height = 4, dpi = 150)
cat("Gráfico guardado: ej2_fare_distribuciones.png\n")

# ─ 2.3 Dataset final: fare winsorizada + age por grupo ────
cat("\n── 2.3  Dataset final — fare winsorizada & age por pclass\n")

titanic_final <- titanic |>
  mutate(
    fare_winsor = pmin(
      pmax(fare, quantile(fare, 0.05, na.rm = TRUE)),
      quantile(fare, 0.95, na.rm = TRUE)
    )
  ) |>
  group_by(pclass) |>
  mutate(
    age_imp = if_else(is.na(age), median(age, na.rm = TRUE), age)
  ) |>
  ungroup()

cat("NA en fare_winsor:", sum(is.na(titanic_final$fare_winsor)), "\n")
cat("NA en age_imp    :", sum(is.na(titanic_final$age_imp)), "\n")

titanic_final |>
  group_by(pclass) |>
  summarise(mediana_age = round(median(age, na.rm = TRUE), 1),
            n_imputados = sum(is.na(age)),
            .groups = "drop") |>
  print()


cols_num <- c("ram", "battery_power", "px_height")

# ── 3.1 Min-Max y Z-score 
cat("── 3.1  Min-Max y Z-score — verificación de propiedades\n")

min_max      <- function(x) (x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T))
estandarizar <- function(x) (x - mean(x, na.rm=T)) / sd(x, na.rm=T)

moviles_scaled <- moviles |>
  mutate(
    across(all_of(cols_num), list(mm  = min_max),      .names = "{.col}_{.fn}"),
    across(all_of(cols_num), list(std = estandarizar), .names = "{.col}_{.fn}")
  )

cat("\nVerificación Min-Max — rango debe ser [0, 1]:\n")
moviles_scaled |>
  select(ends_with("_mm")) |>
  map_df(~ tibble(min = min(.x), max = max(.x)), .id = "variable") |>
  print()

cat("\nVerificación Z-score — media ≈ 0, sd = 1:\n")
moviles_scaled |>
  select(ends_with("_std")) |>
  map_df(~ tibble(media = round(mean(.x), 6),
                  sd    = round(sd(.x), 6)), .id = "variable") |>
  print()

# ── 3.2 Scatter plot ram vs. battery_power 

scatter_data <- bind_rows(
  moviles |>
    select(ram, battery_power) |>
    mutate(escala = "Original"),
  moviles_scaled |>
    select(ram = ram_mm, battery_power = battery_power_mm) |>
    mutate(escala = "Min-Max"),
  moviles_scaled |>
    select(ram = ram_std, battery_power = battery_power_std) |>
    mutate(escala = "Z-score")
) |>
  mutate(escala = factor(escala, levels = c("Original", "Min-Max", "Z-score")))

p3 <- ggplot(scatter_data, aes(x = ram, y = battery_power)) +
  geom_point(alpha = 0.25, size = 0.8, color = "#2c7bb6") +
  geom_smooth(method = "lm", se = FALSE, color = "#d7191c", linewidth = 0.8) +
  facet_wrap(~ escala, scales = "free", nrow = 1) +
  labs(
    title    = "Ejercicio 3",
    subtitle = "La forma de la nube NO cambia; solo cambian los ejes",
    x = "RAM", y = "Battery Power"
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))

ggsave("/mnt/user-data/outputs/ej3_scatter_escalado.png",
       p3, width = 11, height = 4, dpi = 150)
cat("Gráfico guardado: ej3_scatter_escalado.png\n")

cat("\nRespuesta: La forma de la nube NO cambia (correlación preservada).\n")
cat("Solo se transforman las escalas de los ejes.\n")

# ── 3.3 Outlier extremo y su efecto en Min-Max ─────────────
cat("\n── 3.3  Ejemplo sintético — efecto de outlier en Min-Max\n")

set.seed(42)
x_normal  <- c(rnorm(20, mean = 50, sd = 10), 1000)   # outlier extremo

x_mm <- (x_normal - min(x_normal)) / (max(x_normal) - min(x_normal))

cat("Vector original (resumen):\n")
cat("  min:", min(x_normal), "| max:", max(x_normal), "| outlier:", 1000, "\n")

cat("\nMin-Max con outlier extremo (primeros 10 valores normales):\n")
cat(" ", round(x_mm[1:10], 4), "\n")
cat("  → Los 20 valores normales quedan comprimidos entre",
    round(min(x_mm[1:20]), 4), "y", round(max(x_mm[1:20]), 4), "\n")
cat("  → El outlier ocupa el valor 1.0000\n")
cat("\nConclusión: Un solo outlier extremo comprime todos los demás\n")
cat("valores en un rango muy pequeño, perdiendo discriminación útil.\n")
cat("Solución: winsorizarlos antes de aplicar Min-Max, o usar Z-score.\n")



moda <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}

