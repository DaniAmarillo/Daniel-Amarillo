
library(ggplot2)
library(patchwork)
library(purrr)
library(tidyr)


### Clase 7: Limpieza, transformación y normalización de datos.

## Ejercicio 1 Usando el dataset house_prices.csv:

house_prices <- read_csv("C:/Users/danie/Downloads/house_prices.csv", show_col_types = FALSE)
house_prices

# 1. Identifica las 5 columnas con mayor porcentaje de NA. ¿Cuáles eliminarías y cuáles imputarías? Justifica.

porcentaje_na <- house_prices |> 
  summarise(across(everything(),~ mean(is.na(.)) * 100)) |>
  pivot_longer(everything(), names_to = "columna", values_to = "pct_na") |>
  arrange(desc(pct_na)) |>
  head(5)

porcentaje_na

# Eliminaría las primeras 3 porque tienen demasiados NA para ser útiles e imputaría las últimas 2 porque tienen suficientes datos.

# 2. Imputa las columnas numéricas con NA usando la mediana y las categóricas con la moda. ¿Cuántas filas quedan completas (sin ningún NA) después?

moda <- function(x) names(sort(table(x), decreasing = TRUE))[1]

house_prices_limpio<- house_prices |>
  select(-c(PoolQC, MiscFeature, Alley)) |>
  mutate(
    across(where(is.numeric), ~ replace_na(., median(., na.rm = TRUE))),
    across(where(is.character), ~ replace_na(., moda(.)))
  )

sum(complete.cases(house_prices_limpio))
house_prices_limpio

# Quedaron 2919 filas completas sin NA's

# 3. Crea una variable antiguedad = 2024 - YearBuilt. Discretízala en 4 grupos con cut usando los cuartiles como puntos de corte.

house_prices_limpio <- house_prices_limpio |>
  mutate(
    antiguedad = 2024 - YearBuilt,
    antiguedad_grupo = cut(antiguedad,
                           breaks = quantile(antiguedad, probs = c(0, 0.25, 0.5, 0.75, 1)),
                           include.lowest = TRUE,
                           labels = c("Nueva", "Reciente", "Antigua", "Muy antigua"))
  )

house_prices_limpio |> count(antiguedad_grupo)

## Ejercicio 2: Usando titanic.csv:

titanic <- read_csv("C:/Users/danie/Downloads/titanic.csv", show_col_types = FALSE)
titanic


# 1. Aplica el criterio IQR para detectar outliers en fare. ¿Qué porcentaje de pasajeros son outliers?

umbral_cols <- 0.50
titanic_limpio <- titanic |>
  select(where(~ mean(is.na(.x)) <= umbral_cols))

detectar_outliers_iqr <- function(x, k = 1.5) {
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x < (q1 - k * iqr) | x > (q3 + k * iqr)
}

titanic_limpio <- titanic_limpio |>
  mutate(outlier_fare = detectar_outliers_iqr(fare))

cat("Columnas limpias:", ncol(titanic_limpio), "\n")
cat("Outliers en fare:", sum(titanic_limpio$outlier_fare, na.rm = TRUE), "\n")
cat("Porcentaje de pasajeros que son outliers", round(mean(titanic_limpio$outlier_fare, na.rm = TRUE) * 100,2), "\n")

# El porcentaje de pasjeros que son outliers es del 13.07%

# 2. Comapra tres versiones de fare: original, winsorizada al percentil 5–95, y transformación log(1 + fare). Grafica sus distribuciones.

titanic_limpio <- titanic_limpio |>
  mutate(
    # Winsorización: recortar en percentiles 5 y 95
    fare_winsor = pmin(pmax(fare,
                            quantile(fare, 0.05, na.rm = TRUE)),
                       quantile(fare, 0.95, na.rm = TRUE)),
    
    # Transformación logarítmica: comprime la escala de valores extremos
    fare_log    = log1p(fare)
  )

titanic_limpio |>
  summarise(
    across(c(fare, fare_winsor, fare_log),
           list(media = ~ round(mean(.x, na.rm = TRUE), 2),
                sd    = ~ round(sd(.x,   na.rm = TRUE), 2),
                max   = ~ round(max(.x,  na.rm = TRUE), 2)),
           .names = "{.col}__{.fn}")
  ) |>
  pivot_longer(everything(),
               names_to  = c("variable", "estadistico"),
               names_sep = "__") |>
  pivot_wider(names_from = estadistico, values_from = value) |>
  print()

titanic_limpio |>
  select(fare, fare_winsor, fare_log) |>
  pivot_longer(everything(), names_to = "version", values_to = "valor") |>
  ggplot(aes(x = valor, fill = version)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ version, scales = "free") +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(title = "Distribución de fare según tratamiento de outliers",
       x = "Valor", y = "Densidad") +
  theme_minimal(base_size = 12)

# 3. Crea un nuevo dataset donde fare esté winsorizada y age esté imputada con la mediana por grupo de pclass. Verifica que no queden NA en esas columnas.

titanic_final <- titanic_limpio |>
  mutate(
    fare = pmin(pmax(fare,
                     quantile(fare, 0.05, na.rm = TRUE)),
                quantile(fare, 0.95, na.rm = TRUE)),
    fare = replace_na(fare, median(fare, na.rm = TRUE)),
    age = if_else(
      is.na(age),
      median(age, na.rm = TRUE),
      age
    )
  )

# Verificar que no queden NA
cat("NA en fare:", sum(is.na(titanic_final$fare)), "\n")
cat("NA en age: ", sum(is.na(titanic_final$age)),  "\n")

## Ejercicio 3: Usando mobile_data.csv:

mobile <- read_csv("C:/Users/danie/Downloads/mobile_data.csv", show_col_types = FALSE)
mobile

# 1. Aplica Min-Max y Z-score a las variables ram, battery_power y px_height. Verifica propiedades: ¿el rango de Min-Max es [0,1]? ¿La media de Z-score es 0?

# Apicar Min-Max

min_max <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

mobile_norm <- mobile |>
  mutate(
    across(c(ram, battery_power, px_height),
           list(mm = min_max),
           .names = "{.col}_{.fn}")
  )

# Verificar rango [0,1]
mobile_norm |>
  select(ends_with("_mm")) |>
  summarise(across(everything(), list(min = min, max = max))) |>
  print()

# Aplicar Z-score 

estandarizar <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

mobile_std <- mobile |>
  mutate(
    across(c(ram, battery_power, px_height),
           list(std = estandarizar),
           .names = "{.col}_{.fn}")
  )

# Verificar media ≈ 0
mobile_std |>
  select(ends_with("_std")) |>
  summarise(across(everything(), list(media = ~ round(mean(.x, na.rm = TRUE), 4),
                                      sd    = ~ round(sd(.x,   na.rm = TRUE), 4)))) |>
  print()

# 2. Grafica un scatter plot de ram vs. battery_power para las tres versiones (original, Min-Max, Z-score). ¿Cambia la forma de la nube de puntos?


# Unir normalizadas y estandarizadas
mobile_plot <- mobile_norm |>
  select(ram_mm, battery_power_mm) |>
  bind_cols(mobile_std |> select(ram_std, battery_power_std)) |>
  bind_cols(mobile |> select(ram, battery_power))

# Gráfico original
p1 <- ggplot(mobile_plot, aes(x = ram, y = battery_power)) +
  geom_point(alpha = 0.3) +
  labs(title = "Original", x = "ram", y = "battery_power")

# Gráfico Min-Max
p2 <- ggplot(mobile_plot, aes(x = ram_mm, y = battery_power_mm)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  labs(title = "Min-Max", x = "ram_mm", y = "battery_power_mm")

# Gráfico Z-score
p3 <- ggplot(mobile_plot, aes(x = ram_std, y = battery_power_std)) +
  geom_point(alpha = 0.3, color = "tomato") +
  labs(title = "Z-score", x = "ram_std", y = "battery_power_std")

p1 + p2 + p3

# No, la forma de la nube no cambia.
  
# 3. ¿En qué situación el Min-Max da resultados problemáticos? Genera un ejemplo sintético con un outlier extremo y muestra el efecto.

ejemplo <- c(10, 12, 11, 13, 10, 12, 1000)  # 1000 es el outlier
ejemplo_mm <- min_max(ejemplo)

data.frame(
  original = ejemplo,
  min_max  = round(ejemplo_mm, 3)
)

# El problema es que todos los valores normales quedan comprimidos cerca de 0 y el outlier se lleva todo el rango hasta 1. 

## Ejercicio 4: Construye un pipeline funcional que procese el dataset adult.csv end-to-end:

adultos <- read_csv("C:/Users/danie/Downloads/adult.csv", show_col_types = FALSE)
adultos

# 1. Reemplaza "?" por NA en todas las columnas character.

adultos <- adultos |>
  mutate(across(where(is.character), ~ na_if(., "?")))
                
# 2. Elimina columnas con > 10 % de NA.

adultos <- adultos |> 
  select(where(~ mean (is.na(.)) <= 0.10))
  
# 3. Imputa las columnas numéricas con la mediana y las categóricas con la moda.

moda <- function(x) names(sort(table(x), decreasing = TRUE))[1]

adultos_limpio<- adultos |>
  mutate(
    across(where(is.numeric), ~ replace_na(., median(., na.rm = TRUE))),
    across(where(is.character), ~ replace_na(., moda(.)))
  )

sum(complete.cases(adultos_limpio))
adultos_limpio


# 4. Codifica income como variable binaria: >50K = 1, <=50K = 0.

adultos_limpio <- adultos_limpio |>
  mutate(income = if_else(income == ">50k", 1, 0))

table(adultos_limpio["income"])

# 5. Aplica Z-score a todas las columnas numéricas.

estandar <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

adultos_limpio <- adultos_limpio |>
  mutate(across(where(is.numeric), estandar))

adultos_limpio

# 6. Usa purrr::map_df para generar una tabla resumen con media y sd de cada columna numérica antes y después del Z-score. ¿Qué observas?

# Antes del Z-score

antes <- adultos |> 
  select(where(is.numeric)) |>
  map_df(~ tibble(
    media   = round(mean(.x, na.rm = TRUE), 2),
    sd      = round(sd(.x,   na.rm = TRUE), 2)
  ), .id = "variable")

# Después de Z-score

despues <- adultos_limpio |> 
  select(where(is.numeric)) |>
  map_df(~ tibble(
    media   = round(mean(.x, na.rm = TRUE), 2),
    sd      = round(sd(.x,   na.rm = TRUE), 2)
  ), .id = "variable")

cat("=== ANTES ===\n");  print(antes)
cat("=== DESPUÉS ===\n"); print(despues)

#  Antes cada variable tiene su propia media y sd distintos y despues todas las medias son 0 y todos los sd son 1 
