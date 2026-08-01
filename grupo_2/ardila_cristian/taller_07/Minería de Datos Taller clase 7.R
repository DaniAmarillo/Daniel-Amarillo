#------------------------------------------------------------
# Taller clase 7
#------------------------------------------------------------

# Paquetes
paquetes <- c("tidyverse", "knitr")

instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) install.packages(pendientes)

library(tidyverse)
library(knitr)

# Función auxiliar para mostrar tablas
mostrar_tabla <- function(x, n = Inf, caption = NULL) {
  if (is.infinite(n)) {
    knitr::kable(x, caption = caption)
  } else {
    knitr::kable(head(x, n), caption = caption)
  }
}

# Funciones auxiliares usadas en varios ejercicios

moda <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}

detectar_outliers_iqr <- function(x, k = 1.5) {
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  x < (q1 - k * iqr) | x > (q3 + k * iqr)
}

min_max <- function(x) {
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

estandarizar <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Carga de datos
precios <- read_csv("house_prices.csv", show_col_types = FALSE)
titanic <- read_csv("titanic.csv", show_col_types = FALSE)
moviles <- read_csv("mobile_data.csv", show_col_types = FALSE)
adult   <- read_csv("adult.csv", show_col_types = FALSE)


#------------------------------------------------------------
# EJERCICIO 1
# Dataset: house_prices.csv
#------------------------------------------------------------

# 1.1 Identificar las 5 columnas con mayor porcentaje de NA

resumen_na_precios <- precios |>
  map_df(
    ~ tibble(
      tipo   = class(.x)[1],
      n_na   = sum(is.na(.x)),
      pct_na = round(sum(is.na(.x)) / nrow(precios) * 100, 1)
    ),
    .id = "columna"
  ) |>
  arrange(desc(pct_na))

top5_na_precios <- resumen_na_precios |>
  slice(1:5) |>
  mutate(
    decision = case_when(
      pct_na > 50 ~ "Eliminar",
      TRUE        ~ "Imputar"
    ),
    justificacion = case_when(
      pct_na > 50 ~ "Tiene más de 50% de NA; aporta poca información útil.",
      tipo %in% c("numeric", "integer", "double") ~ "Variable numérica; se puede imputar con la mediana.",
      TRUE ~ "Variable categórica; se puede imputar con la moda."
    )
  )

mostrar_tabla(
  top5_na_precios,
  caption = "Top 5 columnas con mayor porcentaje de NA — house_prices"
)

# 1.2 Imputar numéricas con mediana y categóricas con moda

precios_imp <- precios |>
  mutate(
    across(
      where(is.numeric),
      ~ if_else(is.na(.x), median(.x, na.rm = TRUE), .x)
    )
  ) |>
  mutate(
    across(
      where(is.character),
      ~ if_else(is.na(.x), moda(.x), .x)
    )
  )

filas_completas_precios <- sum(complete.cases(precios_imp))

cat(
  "Filas completas después de imputar house_prices:",
  filas_completas_precios,
  "\n"
)

# 1.3 Crear antiguedad = 2024 - YearBuilt y discretizar en 4 grupos con cut

precios_imp <- precios_imp |>
  mutate(
    antiguedad = 2024 - YearBuilt
  )

cortes_antiguedad <- quantile(
  precios_imp$antiguedad,
  probs = c(0, 0.25, 0.50, 0.75, 1),
  na.rm = TRUE
)

precios_imp <- precios_imp |>
  mutate(
    grupo_antiguedad = cut(
      antiguedad,
      breaks = cortes_antiguedad,
      labels = c(
        "Q1 (más nuevas)",
        "Q2",
        "Q3",
        "Q4 (más antiguas)"
      ),
      right = TRUE,
      include.lowest = TRUE
    )
  )

precios_imp |>
  count(grupo_antiguedad) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  mostrar_tabla(
    caption = "Distribución de viviendas por grupo de antigüedad"
  )


#------------------------------------------------------------
# EJERCICIO 2
# Dataset: titanic.csv
#------------------------------------------------------------

# 2.1 Detectar outliers en fare usando criterio IQR

titanic_outliers <- titanic |>
  mutate(
    outlier_fare = detectar_outliers_iqr(fare)
  )

resumen_outliers_fare <- titanic_outliers |>
  summarise(
    n_pasajeros       = sum(!is.na(fare)),
    n_outliers        = sum(outlier_fare, na.rm = TRUE),
    pct_outliers_fare = round(mean(outlier_fare, na.rm = TRUE) * 100, 2)
  )

mostrar_tabla(
  resumen_outliers_fare,
  caption = "Porcentaje de pasajeros outliers en fare"
)

# 2.2 Comparar fare original, winsorizada y log(1 + fare)

titanic_fare <- titanic |>
  mutate(
    fare_winsor = pmin(
      pmax(
        fare,
        quantile(fare, 0.05, na.rm = TRUE)
      ),
      quantile(fare, 0.95, na.rm = TRUE)
    ),
    fare_log = log1p(fare)
  )

titanic_fare |>
  summarise(
    across(
      c(fare, fare_winsor, fare_log),
      list(
        media = ~ round(mean(.x, na.rm = TRUE), 2),
        sd    = ~ round(sd(.x,   na.rm = TRUE), 2),
        min   = ~ round(min(.x,  na.rm = TRUE), 2),
        max   = ~ round(max(.x,  na.rm = TRUE), 2)
      ),
      .names = "{.col}__{.fn}"
    )
  ) |>
  pivot_longer(
    everything(),
    names_to  = c("variable", "estadistico"),
    names_sep = "__"
  ) |>
  pivot_wider(
    names_from = estadistico,
    values_from = value
  ) |>
  mostrar_tabla(
    caption = "Comparación de fare original, winsorizada y logarítmica"
  )

titanic_fare |>
  select(fare, fare_winsor, fare_log) |>
  pivot_longer(
    everything(),
    names_to = "version",
    values_to = "valor"
  ) |>
  filter(!is.na(valor)) |>
  ggplot(aes(x = valor, fill = version)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ version, scales = "free") +
  labs(
    title = "Distribución de fare según tratamiento",
    x = "Valor",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 12)

# 2.3 Dataset con fare winsorizada y age imputada por mediana de pclass

limites_fare <- quantile(
  titanic$fare,
  probs = c(0.05, 0.95),
  na.rm = TRUE
)

mediana_fare <- median(titanic$fare, na.rm = TRUE)

titanic_procesado <- titanic |>
  mutate(
    fare_imp = if_else(
      is.na(fare),
      mediana_fare,
      fare
    ),
    fare_winsor = pmin(
      pmax(fare_imp, limites_fare[1]),
      limites_fare[2]
    )
  ) |>
  group_by(pclass) |>
  mutate(
    age_imp = if_else(
      is.na(age),
      median(age, na.rm = TRUE),
      age
    )
  ) |>
  ungroup()

titanic_procesado |>
  summarise(
    na_fare_winsor = sum(is.na(fare_winsor)),
    na_age_imp     = sum(is.na(age_imp))
  ) |>
  mostrar_tabla(
    caption = "Verificación de NA en fare_winsor y age_imp"
  )


#------------------------------------------------------------
# EJERCICIO 3
# Dataset: mobile_data.csv
#------------------------------------------------------------

# 3.1 Aplicar Min-Max y Z-score a ram, battery_power y px_height

vars_moviles <- c("ram", "battery_power", "px_height")

moviles_escalados <- moviles |>
  mutate(
    across(
      all_of(vars_moviles),
      list(
        mm  = min_max,
        std = estandarizar
      ),
      .names = "{.col}_{.fn}"
    )
  )

# Verificar rango [0,1] en Min-Max

moviles_escalados |>
  select(ends_with("_mm")) |>
  map_df(
    ~ tibble(
      minimo = round(min(.x, na.rm = TRUE), 4),
      maximo = round(max(.x, na.rm = TRUE), 4)
    ),
    .id = "variable"
  ) |>
  mostrar_tabla(
    caption = "Verificación: rango [0,1] para variables Min-Max"
  )

# Verificar media 0 y sd 1 en Z-score

moviles_escalados |>
  select(ends_with("_std")) |>
  map_df(
    ~ tibble(
      media = round(mean(.x, na.rm = TRUE), 4),
      sd    = round(sd(.x,   na.rm = TRUE), 4)
    ),
    .id = "variable"
  ) |>
  mostrar_tabla(
    caption = "Verificación: media≈0 y sd=1 para variables Z-score"
  )

# 3.2 Scatter plot ram vs battery_power para las tres versiones

moviles_scatter <- bind_rows(
  moviles_escalados |>
    transmute(
      ram = ram,
      battery_power = battery_power,
      version = "Original"
    ),
  
  moviles_escalados |>
    transmute(
      ram = ram_mm,
      battery_power = battery_power_mm,
      version = "Min-Max"
    ),
  
  moviles_escalados |>
    transmute(
      ram = ram_std,
      battery_power = battery_power_std,
      version = "Z-score"
    )
)

moviles_scatter |>
  ggplot(aes(x = ram, y = battery_power)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ version, scales = "free") +
  labs(
    title = "ram vs. battery_power: original, Min-Max y Z-score",
    x = "ram",
    y = "battery_power"
  ) +
  theme_minimal(base_size = 12)

cat(
  "La forma de la nube no cambia: Min-Max y Z-score cambian la escala, no la relación entre variables.\n"
)

# 3.3 Ejemplo sintético: Min-Max con outlier extremo

ejemplo_outlier <- tibble(
  id = 1:21,
  x  = c(1:20, 1000)
) |>
  mutate(
    x_mm  = min_max(x),
    x_std = estandarizar(x)
  )

mostrar_tabla(
  ejemplo_outlier,
  caption = "Efecto de un outlier extremo sobre Min-Max"
)

ejemplo_outlier |>
  pivot_longer(
    c(x, x_mm, x_std),
    names_to = "version",
    values_to = "valor"
  ) |>
  ggplot(aes(x = id, y = valor)) +
  geom_point(size = 2) +
  geom_line() +
  facet_wrap(~ version, scales = "free_y") +
  labs(
    title = "Min-Max frente a un outlier extremo",
    x = "Observación",
    y = "Valor"
  ) +
  theme_minimal(base_size = 12)

cat(
  "Min-Max puede ser problemático cuando hay outliers extremos, porque comprime casi todos los valores cerca de 0.\n"
)


#------------------------------------------------------------
# EJERCICIO 4
# Dataset: adult.csv
# Pipeline funcional end-to-end
#------------------------------------------------------------

# 4.1 Funciones del pipeline

reemplazar_interrogacion <- function(df) {
  df |>
    mutate(
      across(
        where(is.character),
        ~ na_if(str_trim(.x), "?")
      )
    )
}

eliminar_cols_alta_na <- function(df, umbral = 0.10) {
  df |>
    select(
      where(~ mean(is.na(.x)) <= umbral)
    )
}

imputar_numericas <- function(df) {
  df |>
    mutate(
      across(
        where(is.numeric),
        ~ if_else(
          is.na(.x),
          median(.x, na.rm = TRUE),
          .x
        )
      )
    )
}

imputar_categoricas <- function(df) {
  df |>
    mutate(
      across(
        where(is.character),
        ~ if_else(
          is.na(.x),
          moda(.x),
          .x
        )
      )
    )
}

codificar_income <- function(df) {
  df |>
    mutate(
      income = if_else(
        str_detect(income, ">50K"),
        1,
        0
      )
    )
}

aplicar_zscore <- function(df) {
  df |>
    mutate(
      across(
        where(is.numeric),
        ~ estandarizar(.x)
      )
    )
}

# 4.2 Dataset antes del Z-score

adult_pre_z <- adult |>
  reemplazar_interrogacion() |>
  eliminar_cols_alta_na(umbral = 0.10) |>
  imputar_numericas() |>
  imputar_categoricas() |>
  codificar_income()

# 4.3 Dataset después del Z-score

adult_z <- adult_pre_z |>
  aplicar_zscore()

# 4.4 Verificaciones de NA

adult_z |>
  summarise(
    total_na = sum(is.na(across(everything())))
  )

cat(
  "Filas con NA después del pipeline adult:",
  sum(!complete.cases(adult_z)),
  "\n"
)

cat(
  "Dimensiones finales adult:",
  nrow(adult_z),
  "×",
  ncol(adult_z),
  "\n"
)

# 4.5 Tabla resumen con purrr::map_df antes y después del Z-score

resumen_adult_antes <- adult_pre_z |>
  select(where(is.numeric)) |>
  map_df(
    ~ tibble(
      media = round(mean(.x, na.rm = TRUE), 4),
      sd    = round(sd(.x,   na.rm = TRUE), 4)
    ),
    .id = "variable"
  ) |>
  mutate(momento = "Antes del Z-score")

resumen_adult_despues <- adult_z |>
  select(where(is.numeric)) |>
  map_df(
    ~ tibble(
      media = round(mean(.x, na.rm = TRUE), 4),
      sd    = round(sd(.x,   na.rm = TRUE), 4)
    ),
    .id = "variable"
  ) |>
  mutate(momento = "Después del Z-score")

resumen_adult_zscore <- bind_rows(
  resumen_adult_antes,
  resumen_adult_despues
) |>
  select(variable, momento, media, sd) |>
  arrange(variable, momento)

mostrar_tabla(
  resumen_adult_zscore,
  caption = "Media y sd antes y después del Z-score — adult"
)

cat(
  "Después del Z-score, las variables numéricas quedan con media cercana a 0 y desviación estándar cercana a 1.\n"
)




