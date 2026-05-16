
install.packages("naniar")
install.packages("tidyverse")
install.packages(c("rlang", "cli"))
install.packages("tidymodels")

library(naniar)
library(tidyverse)
library(tidymodels)


## Ejercicio 1: Diagnóstico de valores faltantes

# Dado el siguiente conjunto de datos simulado en R, responde las preguntas analíticas.

set.seed(42)
n <- 200
datos <- data.frame(
  edad    = c(sample(18:65, 160,  replace = TRUE), rep(NA, 40)),
  ingreso = c(rnorm(100, 3e6, 5e5), rep(NA, 100)),
  ciudad  = sample(c("Bogotá","Medellín","Cali", NA), n, replace = TRUE,
                   prob = c(0.4, 0.3, 0.2, 0.1)),
  compra  = rbinom(n, 1, 0.5)
)



# 1. Calcula el porcentaje de faltantes por variable con colMeans(is.na(datos)).

porcentaje <- colMeans(is.na(datos))*100
porcentaje

# 2.¿Cuántas filas tienen al menos un NA? Usa sum(!complete.cases(datos)). 

filas_na <- sum(!complete.cases(datos))
filas_na 

# 3. Visualiza el patrón con naniar::gg_miss_var(datos) y visdat::vis_miss(datos).

naniar::gg_miss_var(datos) # Este  gráfico muestra el número de Na por variable

# La variable ingreso tiene 100 Na's, la variable "edad"  tiene aproximadamente 40 Na's 
# y ciudad tiene alrededor de 20 Na's, mientras que la varibale compra esta completa.

visdat::vis_miss(datos) # Este gráfico muestra el patrón NA por fila y columna

# El 19.8% del total de datos son faltantes. Se ve que ingreso tiene un bloque grande de 
# NA consecutivos, lo que sugiere que los NA no son aleatorios sino que están concentrados 
# en ciertas filas, a diferencia de ciudad que si muestra un patrón aleatorio.

# 4. Clasifica el mecanismo probable (MCAR, MAR o MNAR) para ingreso y edad. Justifica tu respuesta


# Edad: MAR (Missing At Random) ya que los NA están concentrados en un bloque, lo que sugiere que la 
# ausencia podría estar relacionada con otra variable observable,

# Ingreso:MNAR (Missing Not At Random) dado que los NA no son aleatorios porque están concentrados 
# en un bloque consecutivo como se ve en vis_miss. Esto sugiere que la ausencia del dato está 
# relacionada con el valor mismo, por ejemplo personas con ingresos muy altos o muy bajos tienden 
# a no reportarlo.

# 5. Imputa edad con la mediana e ingreso con la media. ¿Cambia la distribución? Compara con summary() 
# antes y después.

# Summary antes de la imputación
summary(datos)

# Imputación de los datos

datos_imputados <- datos |>
  mutate(
    edad_imp = ifelse(is.na(edad), median(edad, na.rm = TRUE), edad),
    ingreso_imp = ifelse(is.na(ingreso), mean(ingreso, na.rm = TRUE), ingreso),
  )


# Summary después de la imputación
summary(datos_imputados)

# La media y la mediana se acercan mucho después de imputar, significa que la distribución 
# se concentra alrededor del valor imputado, perdiendo variabilidad.

## Ejercicio 2: Pipeline reproducible con recipes

# Construye un pipeline de imputación correcto que evite fuga de información.

# Separa datos
set.seed(123)
split <- initial_split(datos, prop = 0.75)
train <- training(split)
test  <- testing(split)

# Tu tarea: completa el pipeline
receta <- recipe(compra ~ ., data = train) |>
  step_impute_median(all_numeric_predictors()) |>   # imputa numéricas
  step_impute_mode(all_nominal_predictors())  |>    # imputa categóricas
  prep()

train_proc <- bake(receta, new_data = train)
test_proc  <- bake(receta, new_data = test)

# 1.Completa los espacios en blanco usando los selectores correctos de recipes.

# 2. Verifica que no quedan NAs en train_proc ni en test_proc.

sum(is.na(train_proc))
sum(is.na(test_proc))


# 3. ¿Por qué es incorrecto hacer prep() sobre el dataset completo antes del split?

# En la vida real el test representa datos futuros que no existen cuando entrenas el modelo. 
# Si usas esos datos para calcular la mediana estás haciendo trampa, porque el modelo tiene 
# información que no debería tener.

## Ejercicio 3: Razonamiento conceptual

# Responde brevemente sin código, sólo aplicando los conceptos de valores faltantes vistos en clase.

# 1. En una encuesta de salud, las personas con peor estado de salud tienden a no responder la pregunta sobre su propia salud. 
# ¿Qué mecanismo de ausencia aplica (MCAR, MAR o MNAR)? ¿Cómo afecta esto a un modelo predictivo?

# MNAR (Missing Not At Random) Porque la ausencia del dato está relacionada con el valor mismo que no se observó. Las personas 
# con peor salud son las que no responden la pregunta sobre su salud, es decir, el dato falta precisamente porque es malo.
# El modelo quedaría sesgado porque aprende principalmente de personas sanas que sí respondieron, y cuando encuentre casos de 
# personas enfermas en producción, predecirá mal porque nunca los vio durante el entrenamiento.

# 2. Un pipeline imputa la media de ingreso usando el dataset completo y luego separa train/test. ¿Cuál es el error? ¿Cómo lo corregirías?

# E error es que la media incluye valores del conjunto de test, que en la vida real no estarían disponibles al momento de entrenar el modelo. 
# Para corregirlo, primero separar los datos en train y test, luego calcular la media únicamente con los datos de train usando prep(), y 
# finalmente aplicar esa media aprendida al test con bake(). Así el test nunca contamina el entrenamiento y el modelo es una representación 
# honesta de cómo se comportaría con datos nuevos.

# 3. Una variable tiene 65% de valores faltantes. ¿La eliminarías directamente? ¿Qué considerarías antes de decidir?

# No la eliminaría directamente. Primero evaluaría el mecanismo de ausencia, si la variable es importante para el modelo, 
# y si la ausencia misma aporta información. Con 65% de faltantes la imputación sería muy riesgosa, pero antes de eliminarla 
# consideraría crear una variable indicadora que registre si el dato faltaba o no. Solo la eliminaría si no es importante 
# para el modelo y los faltantes parecen aleatorios.

# 4. ¿Cuándo conviene agregar un indicador binario de ausencia (ingreso_faltante = 1) además de imputar el valor? Da un ejemplo concreto.

# Conviene cuando el mecanismo es MNAR, es decir cuando la ausencia del dato en sí misma contiene información útil para el modelo.
# or ejemplo en una encuesta de crédito bancario, si una persona no reporta su ingreso probablemente es porque es muy bajo o tiene deudas, 
# y esa ausencia podría predecir que es un mal pagador. Si solo imputas con la media pierdes esa señal, pero si además creas la variable 
# ingreso_faltante = 1 el modelo puede aprender que las personas que no reportaron ingreso tienen un comportamiento diferente a las que sí 
# lo reportaron.

# 5. ¿En qué situación preferirías imputación múltiple con mice sobre imputación simple con la mediana?

# Preferiría mice cuando tienes un porcentaje alto de faltantes y el mecanismo es MAR, porque la imputación simple con la mediana reemplaza 
# todos los NA con el mismo valor, lo que reduce artificialmente la varianza y distorsiona las relaciones entre variables. Por ejemplo si 
# tienes un dataset médico donde el 30% de los pacientes no tienen registrado su nivel de glucosa, imputar con la mediana crearía un pico 
# artificial en esa variable. mice en cambio genera múltiples versiones plausibles del dato faltante basándose en las otras variables del 
# paciente como edad, peso y presión arterial, preservando la variabilidad y las correlaciones naturales entre variables.



