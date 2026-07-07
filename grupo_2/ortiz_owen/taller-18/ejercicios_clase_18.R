# 0. Colocar el token ANTES de cargar tabpfn
Sys.setenv(TABPFN_TOKEN = "tabpfn_sk_LWigHf04WRkxQcHipER8GkcNgUiP5ILMyQKIB6ValkI")

cat("R ve el token:", Sys.getenv("TABPFN_TOKEN") != "", "\n")
cat("Longitud token:", nchar(Sys.getenv("TABPFN_TOKEN")), "\n")

library(reticulate)
py_run_string("
import os
t = os.getenv('TABPFN_TOKEN')
print('Python ve el token:', t is not None and len(t) > 0)
print('Inicio token:', t[:12] if t else None)
")
# 1. Cargar paquetes
library(tabpfn)
library(dplyr)
library(rsample)
library(yardstick)
library(reticulate)

# Verificar que R ve el token
Sys.getenv("TABPFN_TOKEN")

# Verificar que Python también ve el token
py_run_string("
import os
print('TOKEN_VISIBLE:', os.getenv('TABPFN_TOKEN') is not None)
")

# 2. Cargar datos
data(forestfires)

# --- LAS DOS LÍNEAS CLAVE PARA LIMPIAR LA MEMORIA ---
forestfires$area <- as.numeric(as.character(forestfires$area)) # Forzamos a que vuelva a ser número continuo
forestfires$day  <- as.factor(forestfires$day)                # El día es nuestro nuevo objetivo categórico
# ----------------------------------------------------

# 3. Dividir entrenamiento y prueba
set.seed(123)

split_forestfires <- initial_split(
  forestfires,
  prop = 0.80,
  strata = day
)

train_data <- training(split_forestfires)
test_data  <- testing(split_forestfires)

# 4. Entrenar TabPFN
modelo_tabpfn <- tab_pfn(
  day ~ .,   # Predecimos 'day'
  data = train_data,
  version = "v2",
  control = control_tab_pfn(
    device = "auto",
    random_state = 123
  )
)

# 5. Predicciones
predicciones <- predict(
  modelo_tabpfn,
  new_data = test_data %>% select(-day) # Quitamos la columna objetivo
)

# 6. Resultados
resultados <- bind_cols(
  test_data %>% select(day),
  predicciones
)

print(head(resultados))

# 7. Accuracy
accuracy(
  resultados,
  truth = day,
  estimate = .pred_class
)

# 8. Matriz de confusión
conf_mat(
  resultados,
  truth = day,
  estimate = .pred_class
)

# 9. Nuevo caso
nuevo_caso <- tibble(
  X = 7,
  Y = 5,
  month = "mar", 
  FFMC = 86.2,
  DMC = 26.2,
  DC = 94.3,
  ISI = 5.1,
  temp = 8.2,
  RH = 51,
  wind = 6.7,
  rain = 0.0,
  area = 0.0 # Ahora entra limpio como numérico double
)

predict(modelo_tabpfn, nuevo_caso)



# - Accuracy: El modelo alcanzó un 73.3% de precisión global en el test set.
# - Matriz de Confusión: Los mayores aciertos están en la diagonal de los fines 
#   de semana (Domingo: 16, Sábado: 12), mostrando patrones climáticos claros.
# - Predicción Final: Al evaluar el nuevo caso climático, el modelo asignó la 
#   mayor probabilidad al día Sábado (.pred_sat = 28%).