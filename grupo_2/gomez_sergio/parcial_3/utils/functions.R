# ------------------------------------------------------------
# Función auxiliar: moda con detección de empate
# Retorna el valor más frecuente, o NA si hay empate
# ------------------------------------------------------------
moda_unica <- function(x) {
  
  tab <- sort(table(x), decreasing = TRUE)
  
  # Empate si los dos valores más frecuentes tienen la misma frecuencia
  if (length(tab) > 1 && tab[1] == tab[2]) return(NA)
  
  names(tab)[1]
}

# ------------------------------------------------------------
# Función auxiliar: prediccion sobre el dataset de test segun modelo
# Retorna el dataframe completo del modelo con sus predicciones
# ------------------------------------------------------------
predecir <- function(modelo_fit, nombre) {
  predict(modelo_fit, test, type = "prob") %>%
    bind_cols(predict(modelo_fit, test)) %>%
    bind_cols(test %>% select(OBJETIVO)) %>%
    mutate(modelo = nombre)
}

# ------------------------------------------------------------
# Función auxiliar: calculo metricas de los modelos en test
# dataframe con las metricas sobre test para modelo dado
# ------------------------------------------------------------
calcular_metricas <- function(preds, nombre) {
  bind_rows(
    preds %>% pr_auc(truth  = OBJETIVO, .pred_positivo, event_level = "second"),
    preds %>% roc_auc(truth = OBJETIVO, .pred_positivo, event_level = "second"),
    preds %>% f_meas(truth  = OBJETIVO, estimate = .pred_class, event_level = "second"),
    preds %>% sens(truth    = OBJETIVO, estimate = .pred_class, event_level = "second"),
    preds %>% yardstick::spec(truth    = OBJETIVO, estimate = .pred_class, event_level = "second"),
    preds %>% accuracy(truth = OBJETIVO, estimate = .pred_class, event_level = "second")
  ) %>% mutate(modelo = nombre)
}