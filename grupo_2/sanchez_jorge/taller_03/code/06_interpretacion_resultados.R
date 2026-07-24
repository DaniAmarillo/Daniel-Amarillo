
# Taller 3 - Minería de Datos 
# PARTE 6: Interpretación de resultados
# Jorge Andres Sanchez Duarte

library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(scales)
library(knitr)
library(randomForest)
library(gbm)
library(pROC)

modelo_logistico <- readRDS("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/modelo_logistico.rds")
modelo_rf        <- readRDS("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/modelo_rf.rds")
modelo_gbm       <- readRDS("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/modelo_gbm.rds")
resultados       <- readRDS("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/resultados_modelamiento.rds")
dataset_modelamiento <- readRDS("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/dataset_modelamiento.rds")

train               <- resultados$train
test                <- resultados$test
prob_logistico_test <- resultados$prob_logistico_test
prob_rf_test        <- resultados$prob_rf_test
prob_gbm_test       <- resultados$prob_gbm_test
mejor_ntrees_gbm    <- resultados$mejor_ntrees_gbm
tabla_comparativa   <- resultados$tabla_comparativa
variables_logistica <- resultados$variables_logistica

cat("Objetos cargados correctamente.\n")
cat("Train:", nrow(train), "- Test:", nrow(test), "\n")
print(tabla_comparativa)

# Variables más importantes o más influyentes en cada modelo

#  Regresión logística: coeficientes (en escala de log-odds)

coef_logistico <- summary(modelo_logistico)$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column("variable") |>
  filter(variable != "(Intercept)") |>
  arrange(desc(abs(`z value`)))

cat("\nRegresión logística: variables ordenadas por |z value| \n")
kable(coef_logistico |> mutate(across(where(is.numeric), ~round(.x, 4))),
      caption = "Coeficientes del modelo de regresión logística")

#  Random Forest: importancia 
importancia_rf <- importance(modelo_rf) |>
  as.data.frame() |>
  tibble::rownames_to_column("variable") |>
  arrange(desc(MeanDecreaseGini))

cat("\n Random Forest: importancia de variables (top 10) \n")
kable(head(importancia_rf, 10) |> mutate(across(where(is.numeric), ~round(.x, 2))),
      caption = "Importancia de variables - Random Forest")

# GBM: influencia relativa (equivalente a xgb)

importancia_gbm <- summary(modelo_gbm, n.trees = mejor_ntrees_gbm, plotit = FALSE) |>
  arrange(desc(rel.inf))

cat("\nGBM: influencia relativa de variables (top 10) \n")
kable(head(importancia_gbm, 10) |> mutate(rel.inf = round(rel.inf, 2)),
      caption = "Influencia relativa de variables - GBM")

# tabla unificada
top10_logistico <- coef_logistico$variable[1:min(10, nrow(coef_logistico))]
top10_rf        <- importancia_rf$variable[1:10]
top10_gbm       <- importancia_gbm$var[1:10]

tabla_consenso <- tibble::tibble(
  variable = union(union(top10_logistico, top10_rf), top10_gbm)
) |>
  mutate(
    en_logistico = variable %in% top10_logistico,
    en_rf        = variable %in% top10_rf,
    en_gbm       = variable %in% top10_gbm,
    n_modelos    = en_logistico + en_rf + en_gbm
  ) |>
  arrange(desc(n_modelos), variable)

cat("\n Variables que aparecen en el top 10 de más de un modelo \n")
kable(tabla_consenso |> filter(n_modelos > 1),
      caption = "Variables importantes en consenso entre modelos")

# Relación entre las variables más influyentes y la enfermedad

calcular_pdp <- function(modelo, datos, variable, n_puntos = 20, tipo_pred = "prob") {
  if (is.numeric(datos[[variable]])) {
    valores <- seq(
      quantile(datos[[variable]], 0.02, na.rm = TRUE),
      quantile(datos[[variable]], 0.98, na.rm = TRUE),
      length.out = n_puntos
    )
  } else {
    valores <- levels(datos[[variable]])
  }

  purrr::map_df(valores, function(v) {
    datos_mod <- datos
    datos_mod[[variable]] <- v
    if (tipo_pred == "prob") {
      pred <- predict(modelo, newdata = datos_mod, type = "prob")[, "Si"]
    } else {
      pred <- predict(modelo, newdata = datos_mod, type = tipo_pred)
    }
    tibble::tibble(valor = v, prediccion_promedio = mean(pred, na.rm = TRUE))
  })
}

# PDP de porte_anestesico_max (consenso en los 3 modelos)
pdp_porte <- calcular_pdp(modelo_rf, test, "porte_anestesico_max")

grafico_pdp_porte <- ggplot(pdp_porte, aes(x = as.numeric(valor), y = prediccion_promedio)) +
  geom_line(color = "#dc2626", linewidth = 1) +
  geom_point(color = "#dc2626", size = 2) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "PDP: porte anestésico máximo",
    subtitle = "Efecto marginal sobre la probabilidad predicha de hernia (Random Forest)",
    x = "Porte anestésico máximo observado", y = "Probabilidad promedio predicha"
  ) +
  theme_minimal(base_size = 13)

ggsave("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/fig_09_pdp_porte_anestesico.png", grafico_pdp_porte, width = 7, height = 5, dpi = 150)
print(grafico_pdp_porte)

# PDP de costo_total (consenso en los 3 modelos)
pdp_costo <- calcular_pdp(modelo_rf, test, "costo_total")

grafico_pdp_costo <- ggplot(pdp_costo, aes(x = as.numeric(valor), y = prediccion_promedio)) +
  geom_line(color = "#dc2626", linewidth = 1) +
  geom_point(color = "#dc2626", size = 2) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "PDP: costo total acumulado por beneficiario",
    subtitle = "Efecto marginal sobre la probabilidad predicha de hernia (Random Forest)",
    x = "Costo total acumulado", y = "Probabilidad promedio predicha"
  ) +
  theme_minimal(base_size = 13)

ggsave("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/fig_10_pdp_costo_total.png", grafico_pdp_costo, width = 7, height = 5, dpi = 150)
print(grafico_pdp_costo)

# Discusión clínica de las variables de consenso


# Comparación entre los modelos entrenados

cat("\nTabla comparativa completa (de nuevo, para referencia) \n")
kable(tabla_comparativa |> mutate(across(where(is.numeric), ~round(.x, 4))),
      caption = "Comparación de los tres modelos (conjunto de prueba)")


# Análisis de errores del modelo

nombre_mejor_modelo <- tabla_comparativa$modelo[1] 
prob_mejor_modelo <- switch(
  nombre_mejor_modelo,
  "Regresión logística"     = prob_logistico_test,
  "Random Forest"           = prob_rf_test,
  "Gradient Boosting (GBM)" = prob_gbm_test
)

cat("\nAnálisis de errores: modelo con mejor PR-AUC =", nombre_mejor_modelo, "\n")

test_con_pred <- test |>
  mutate(
    prob_pred  = prob_mejor_modelo,
    clase_pred = if_else(prob_pred >= 0.5, "Si", "No")
  )

falsos_negativos <- test_con_pred |> filter(hernia == "Si", clase_pred == "No")
falsos_positivos <- test_con_pred |> filter(hernia == "No", clase_pred == "Si")
verdaderos_positivos <- test_con_pred |> filter(hernia == "Si", clase_pred == "Si")

cat("Falsos negativos (positivos reales NO detectados):", nrow(falsos_negativos), "\n")
cat("Verdaderos positivos (detectados correctamente):", nrow(verdaderos_positivos), "\n")
cat("Falsos positivos (negativos reales marcados como positivos):", nrow(falsos_positivos), "\n")

if (nrow(falsos_negativos) > 0) {
  cat("\nPerfil de los falsos negativos (los casos que el modelo no detectó):\n")
  print(falsos_negativos |>
          select(edad, sexo, tuvo_internacion, porte_anestesico_max, costo_total, prob_pred))
  cat("\nComparación con el perfil promedio de los verdaderos positivos:\n")
  print(verdaderos_positivos |>
          summarise(across(c(edad, tuvo_internacion, porte_anestesico_max, costo_total), ~mean(.x, na.rm = TRUE))))
}

cat("\nMuestra de 5 falsos positivos (para inspeccionar si comparten algún",
    "patrón con los verdaderos positivos):\n")
print(falsos_positivos |>
        select(edad, sexo, tuvo_internacion, porte_anestesico_max, costo_total, prob_pred) |>
        arrange(desc(prob_pred)) |>
        head(5))