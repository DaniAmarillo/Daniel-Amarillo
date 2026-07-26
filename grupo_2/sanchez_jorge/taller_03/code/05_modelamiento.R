
# Taller 3 - Minería de Datos 
# PARTE 5: Modelamiento predictivo
# Jorge Andres Snachez Duarte

library(dplyr)
library(readr)
library(stringr)
library(caret)        # createDataPartition
library(randomForest)  # Clase 15
library(gbm)           # Clase 16 (boosting nativo de R)
library(pROC)          # Clase 13 (curva ROC / AUC)
library(MLmetrics)     # PRAUC, F1_Score, Sensitivity, Specificity
library(knitr)
library(ggplot2)

set.seed(1010093359) # semilla f

dataset_modelamiento <- readRDS("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/dataset_modelamiento.rds")

#Preparación 

# Selección de columnas predictoras

datos_modelo <- dataset_modelamiento |>
  select(-CHAVE_FUNCIONAL)

cat("Variables candidatas a predictoras:", ncol(datos_modelo) - 1, "\n")
cat("Variable objetivo: hernia\n")

# Tratamiento de variables categóricas de alta cardinalidad

agrupar_categorias_raras <- function(x, min_n = 200) {
  x <- if_else(is.na(x), "sin_dato", x)
  freq <- table(x)
  raras <- names(freq[freq < min_n])
  if_else(x %in% raras, "otro", x)
}

datos_modelo <- datos_modelo |>
  mutate(
    sexo                      = if_else(is.na(sexo), "sin_dato", sexo),
    tipo_beneficiario         = if_else(is.na(tipo_beneficiario), "sin_dato", tipo_beneficiario),
    uf_mas_frecuente          = agrupar_categorias_raras(uf_mas_frecuente),
    tipo_unidad_mas_frecuente = agrupar_categorias_raras(tipo_unidad_mas_frecuente)
  ) |>
  mutate(across(where(is.character), as.factor)) |>
  mutate(hernia = factor(hernia, levels = c(0, 1), labels = c("No", "Si")))

cat("\nNiveles de las variables categóricas tras agrupar categorías raras:\n")
cat("sexo:", nlevels(datos_modelo$sexo), "niveles\n")
cat("tipo_beneficiario:", nlevels(datos_modelo$tipo_beneficiario), "niveles\n")
cat("uf_mas_frecuente:", nlevels(datos_modelo$uf_mas_frecuente), "niveles\n")
cat("tipo_unidad_mas_frecuente:", nlevels(datos_modelo$tipo_unidad_mas_frecuente), "niveles\n")

# Tratamiento de valores faltantes en variables numéricas

cat("\nNA antes de imputar:\n")
print(colSums(is.na(datos_modelo)))

imputar_mediana_con_indicador <- function(df, columna) {
  col_faltante <- paste0(columna, "_faltante")
  mediana <- median(df[[columna]], na.rm = TRUE)
  df[[col_faltante]] <- as.integer(is.na(df[[columna]]))
  df[[columna]][is.na(df[[columna]])] <- mediana
  df
}

datos_modelo <- datos_modelo |>
  imputar_mediana_con_indicador("edad") |>
  imputar_mediana_con_indicador("porte_anestesico_max") |>
  imputar_mediana_con_indicador("porte_anestesico_promedio")

cat("\nNA después de imputar (debe ser todo cero):\n")
print(colSums(is.na(datos_modelo)))

# Partición train/test (80/20 estratificada)
#
idx_train <- createDataPartition(datos_modelo$hernia, p = 0.8, list = FALSE)
train <- datos_modelo[idx_train, ]
test  <- datos_modelo[-idx_train, ]

cat("\n Partición train/test\n")
cat("Train:", nrow(train), "beneficiarios,", sum(train$hernia == "Si"), "positivos\n")
cat("Test :", nrow(test), "beneficiarios,", sum(test$hernia == "Si"), "positivos\n")


# Entrenamiento de modelos

n_neg_train <- sum(train$hernia == "No")
n_pos_train <- sum(train$hernia == "Si")
ratio_desbalance <- n_neg_train / n_pos_train
cat("\nRatio de desbalance en train (negativos / positivos):", round(ratio_desbalance, 1), "\n")

# Modelo 1: Regresión logística 

pesos_train <- if_else(train$hernia == "Si", ratio_desbalance, 1)


variables_logistica <- c(
  "hernia",
  "tuvo_internacion", "porte_anestesico_max", "edad",
  "n_procedimientos", "costo_total", "n_especialidades_distintas",
  "sexo", "tuvo_uci", "n_utilizaciones"
)

modelo_logistico <- glm(
  hernia ~ .,
  data    = train |> select(all_of(variables_logistica)),
  family  = binomial(link = "logit"),
  weights = pesos_train
)

cat("\n Resumen del modelo de regresión logística (variables seleccionadas) \n")
print(summary(modelo_logistico)$coefficients |> round(4))
cat("\n¿Convergió?", modelo_logistico$converged, "\n")

prob_logistico_test <- predict(modelo_logistico, newdata = test, type = "response")

# Modelo 2: Random Forest

n_pos_train_rf <- sum(train$hernia == "Si")

set.seed(42)
modelo_rf <- randomForest(
  hernia ~ .,
  data       = train,
  ntree      = 300,
  mtry       = floor(sqrt(ncol(train) - 1)), # regla por defecto para clasificación, Clase 15
  sampsize   = c(No = n_pos_train_rf * 3, Si = n_pos_train_rf),
  importance = TRUE
)

cat("\n Resumen del modelo Random Forest \n")
print(modelo_rf)

prob_rf_test <- predict(modelo_rf, newdata = test, type = "prob")[, "Si"]

# Modelo 3: Gradient Boosting 

gbm_train <- train |>
  mutate(hernia_num = as.integer(hernia == "Si")) |>
  select(-hernia)

set.seed(42)
modelo_gbm <- gbm(
  hernia_num ~ .,
  data              = gbm_train,
  distribution      = "bernoulli",
  n.trees           = 300,
  interaction.depth = 3,      # profundidad moderada, análoga a max_depth en XGBoost
  shrinkage         = 0.05,   # tasa de aprendizaje, análoga a eta en XGBoost
  bag.fraction      = 0.8,    # análogo a subsample en XGBoost
  weights           = pesos_train,
  train.fraction    = 0.8,    # 80% interno de train para ajustar, 20% como validación de early stopping
  n.cores           = 1,
  verbose           = FALSE
)



mejor_ntrees_gbm <- gbm.perf(modelo_gbm, method = "test", plot.it = FALSE)
cat("\nResumen del modelo GBM \n")
cat("Número óptimo de árboles (según holdout interno):", mejor_ntrees_gbm, "de", modelo_gbm$n.trees, "\n")

prob_gbm_test <- predict(modelo_gbm, newdata = test, n.trees = mejor_ntrees_gbm, type = "response")

# 4. Comparación de modelos con métricas adecuadas para clases desbalanceadas

calcular_metricas <- function(prob_pred, y_real, nombre_modelo, umbral = 0.5) {
  clase_pred <- factor(if_else(prob_pred >= umbral, "Si", "No"), levels = c("No", "Si"))
  y_real     <- factor(y_real, levels = c("No", "Si"))

  matriz_confusion <- table(Predicho = clase_pred, Real = y_real)

  y_real_num <- as.integer(y_real == "Si")

  roc_obj <- suppressMessages(roc(y_real_num, prob_pred, quiet = TRUE))

  tibble::tibble(
    modelo        = nombre_modelo,
    accuracy      = mean(clase_pred == y_real),
    sensibilidad  = Sensitivity(y_pred = clase_pred, y_true = y_real, positive = "Si"),
    especificidad = Specificity(y_pred = clase_pred, y_true = y_real, positive = "Si"),
    f1_score      = F1_Score(y_pred = clase_pred, y_true = y_real, positive = "Si"),
    roc_auc       = as.numeric(auc(roc_obj)),
    pr_auc        = PRAUC(y_pred = prob_pred, y_true = y_real_num),
    vp = matriz_confusion["Si", "Si"], fp = matriz_confusion["Si", "No"],
    fn = matriz_confusion["No", "Si"], vn = matriz_confusion["No", "No"]
  )
}

metricas_logistico <- calcular_metricas(prob_logistico_test, test$hernia, "Regresión logística")
metricas_rf        <- calcular_metricas(prob_rf_test,        test$hernia, "Random Forest")
metricas_gbm        <- calcular_metricas(prob_gbm_test,       test$hernia, "Gradient Boosting (GBM)")

tabla_comparativa <- bind_rows(metricas_logistico, metricas_rf, metricas_gbm) |>
  arrange(desc(pr_auc))

n_pos_test <- sum(test$hernia == "Si")

cat("\nComparación de modelos (conjunto de prueba, umbral = 0.5) \n")
kable(tabla_comparativa |> mutate(across(where(is.numeric), ~round(.x, 3))),
      caption = "Comparación de modelos: hernias abdominales (K40-K46)")


# 4.1 Curvas ROC comparadas

roc_logistico <- roc(as.integer(test$hernia == "Si"), prob_logistico_test, quiet = TRUE)
roc_rf        <- roc(as.integer(test$hernia == "Si"), prob_rf_test, quiet = TRUE)
roc_gbm       <- roc(as.integer(test$hernia == "Si"), prob_gbm_test, quiet = TRUE)

datos_roc <- bind_rows(
  tibble::tibble(fpr = 1 - roc_logistico$specificities, tpr = roc_logistico$sensitivities,
                 modelo = sprintf("Regresión logística (AUC=%.3f)", auc(roc_logistico))),
  tibble::tibble(fpr = 1 - roc_rf$specificities, tpr = roc_rf$sensitivities,
                 modelo = sprintf("Random Forest (AUC=%.3f)", auc(roc_rf))),
  tibble::tibble(fpr = 1 - roc_gbm$specificities, tpr = roc_gbm$sensitivities,
                 modelo = sprintf("GBM (AUC=%.3f)", auc(roc_gbm)))
)

grafico_roc <- ggplot(datos_roc, aes(x = fpr, y = tpr, color = modelo)) +
  geom_line(linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  labs(
    title = "Curvas ROC comparadas",
    subtitle = "Hernias abdominales (K40-K46) -- conjunto de prueba",
    x = "1 - Especificidad (tasa de falsos positivos)",
    y = "Sensibilidad (tasa de verdaderos positivos)",
    color = "Modelo"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", legend.direction = "vertical")

ggsave("C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/fig_05_curvas_roc.png", grafico_roc, width = 7, height = 6, dpi = 150)
print(grafico_roc)

# 4.2 Curvas Precision-Recall comparadas 

curva_pr <- function(prob_pred, y_real) {
  y_real_num <- as.integer(y_real == "Si")
  umbrales <- sort(unique(prob_pred), decreasing = TRUE)
  purrr::map_df(umbrales, function(u) {
    pred <- as.integer(prob_pred >= u)
    vp <- sum(pred == 1 & y_real_num == 1)
    fp <- sum(pred == 1 & y_real_num == 0)
    fn <- sum(pred == 0 & y_real_num == 1)
    tibble::tibble(
      umbral    = u,
      precision = if_else(vp + fp == 0, 1, vp / (vp + fp)),
      recall    = vp / (vp + fn)
    )
  })
}

datos_pr <- bind_rows(
  curva_pr(prob_logistico_test, test$hernia) |>
    mutate(modelo = sprintf("Regresión logística (PR-AUC=%.3f)", metricas_logistico$pr_auc)),
  curva_pr(prob_rf_test, test$hernia) |>
    mutate(modelo = sprintf("Random Forest (PR-AUC=%.3f)", metricas_rf$pr_auc)),
  curva_pr(prob_gbm_test, test$hernia) |>
    mutate(modelo = sprintf("GBM (PR-AUC=%.3f)", metricas_gbm$pr_auc))
)

prevalencia_test <- mean(test$hernia == "Si")

grafico_pr <- ggplot(datos_pr, aes(x = recall, y = precision, color = modelo)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = prevalencia_test, linetype = "dashed", color = "gray60") +
  annotate("text", x = 0.15, y = prevalencia_test, vjust = -0.8, size = 3,
           label = sprintf("Línea base (modelo aleatorio): %.4f", prevalencia_test), color = "gray40") +
  labs(
    title = "Curvas Precisión-Recall comparadas",
    subtitle = "Hernias abdominales (K40-K46) -- métrica principal de este taller, dado el desbalance extremo",
    x = "Recall (sensibilidad)", y = "Precisión", color = "Modelo"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", legend.direction = "vertical")

ggsave("output/fig_06_curvas_pr.png", grafico_pr, width = 7, height = 6, dpi = 150)
print(grafico_pr)

# Importancia de variables

#  Importancia en Random Forest 

cat("\n Importancia de variables: Random Forest \n")
importancia_rf <- importance(modelo_rf)
print(round(importancia_rf[order(-importancia_rf[, "MeanDecreaseGini"]), ], 2) |> head(15))

png("output/fig_07_importancia_rf.png", width = 900, height = 700, res = 120)
varImpPlot(modelo_rf, main = "Importancia de variables - Random Forest", n.var = 15)
dev.off()

#Importancia en GBM (equivalente a xgb.importance/xgb.plot.importance
# de la Clase 16, usando la función nativa de summary.gbm)

cat("\n Importancia de variables: GBM \n")
png("output/fig_08_importancia_gbm.png", width = 900, height = 700, res = 120)
importancia_gbm <- summary(modelo_gbm, n.trees = mejor_ntrees_gbm, plotit = TRUE,
                            main = "Importancia de variables - GBM")
dev.off()
print(head(importancia_gbm, 15))

# Guardado de modelos y resultados para el script de interpretación

saveRDS(modelo_logistico, "C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/modelo_logistico.rds")
saveRDS(modelo_rf,        "C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/modelo_rf.rds")
saveRDS(modelo_gbm,       "C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/modelo_gbm.rds")
saveRDS(
  list(
    train = train, test = test,
    prob_logistico_test = prob_logistico_test,
    prob_rf_test         = prob_rf_test,
    prob_gbm_test         = prob_gbm_test,
    mejor_ntrees_gbm      = mejor_ntrees_gbm,
    tabla_comparativa     = tabla_comparativa,
    variables_logistica   = variables_logistica
  ),
  "C:/Users/jorge/OneDrive/Escritorio/Materias/Mineria/dm_2016325/grupo_2/sanchez_jorge/taller_03/output/resultados_modelamiento.rds"
)


