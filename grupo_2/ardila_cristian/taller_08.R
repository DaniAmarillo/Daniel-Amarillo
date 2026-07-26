# ============================================================
# Minería de Datos - Taller Clase 8
# ============================================================


# ------------------------------------------------------------
# 0. Paquetes
# ------------------------------------------------------------

paquetes <- c(
  "tidyverse", "knitr", "naniar", "visdat", "rsample", "recipes"
)

instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  install.packages(pendientes)
}

library(tidyverse)
library(knitr)
library(naniar)
library(visdat)
library(rsample)
library(recipes)


# ------------------------------------------------------------
# Función auxiliar para mostrar tablas si se desea
# ------------------------------------------------------------

mostrar_tabla <- function(x, n = Inf, caption = NULL) {
  if (is.infinite(n)) {
    knitr::kable(x, caption = caption)
  } else {
    knitr::kable(head(x, n), caption = caption)
  }
}


# ============================================================
# Ejercicio 1. Diagnóstico de valores faltantes
# ============================================================

set.seed(42)

n <- 200

datos <- data.frame(
  edad    = c(sample(18:65, 160, replace = TRUE), rep(NA, 40)),
  ingreso = c(rnorm(100, 3e6, 5e5), rep(NA, 100)),
  ciudad  = sample(
    c("Bogotá", "Medellín", "Cali", NA),
    n,
    replace = TRUE,
    prob = c(0.4, 0.3, 0.2, 0.1)
  ),
  compra  = rbinom(n, 1, 0.5)
)


# ------------------------------------------------------------
# 1. Porcentaje de faltantes por variable
# ------------------------------------------------------------

faltantes_directo <- colMeans(is.na(datos))

porcentaje_faltantes <- datos |>
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "porcentaje_faltante"
  ) |>
  mutate(porcentaje_faltante = round(porcentaje_faltante, 2)) |>
  arrange(desc(porcentaje_faltante))

# Revisar manualmente
faltantes_directo
porcentaje_faltantes


# ------------------------------------------------------------
# 2. Filas con al menos un NA
# ------------------------------------------------------------

filas_con_na_directo <- sum(!complete.cases(datos))

filas_incompletas <- tibble(
  indicador = "Filas con al menos un NA",
  valor = filas_con_na_directo
)

# Revisar manualmente
filas_con_na_directo
filas_incompletas


# ------------------------------------------------------------
# 3. Visualizar patrón de faltantes con naniar
# ------------------------------------------------------------

grafico_naniar <- naniar::gg_miss_var(datos) +
  coord_flip() +
  labs(
    title = "Valores faltantes por variable",
    x = NULL,
    y = "Número de valores faltantes"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    plot.margin = margin(8, 8, 8, 8)
  )

# Visualizar manualmente
 grafico_naniar


# ------------------------------------------------------------
# 4. Visualizar patrón de faltantes con visdat
# ------------------------------------------------------------

grafico_visdat <- visdat::vis_miss(datos) +
  labs(
    title = "Mapa de valores faltantes"
  ) +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 8
    ),
    axis.text.y = element_text(size = 7),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    plot.margin = margin(8, 8, 8, 8)
  )

# Visualizar manualmente
grafico_visdat


# ------------------------------------------------------------
# 5 y 6. Mecanismo probable de ausencia para ingreso
# ------------------------------------------------------------

mecanismo_ingreso <- tibble(
  variable = "ingreso",
  mecanismo_probable = "MNAR",
  justificacion = paste(
    "El ingreso es una variable sensible.",
    "Es razonable sospechar que la ausencia puede depender del propio valor no observado,",
    "por ejemplo si personas con ingresos muy altos o muy bajos prefieren no reportarlo."
  )
)

# Revisar manualmente
mecanismo_ingreso


# ------------------------------------------------------------
# 7 y 8. Mecanismo probable de ausencia para edad
# ------------------------------------------------------------

mecanismo_edad <- tibble(
  variable = "edad",
  mecanismo_probable = "MAR u operacional",
  justificacion = paste(
    "En los datos simulados la ausencia de edad aparece como un bloque construido artificialmente.",
    "No hay evidencia directa de que dependa del valor real de edad.",
    "En un caso aplicado se revisaría si depende de ciudad, compra u otra variable observada."
  )
)

# Revisar manualmente
mecanismo_edad

# ------------------------------------------------------------
# 9 y 10. Imputar edad con mediana e ingreso con media
# ------------------------------------------------------------

datos_imputados <- datos |>
  mutate(
    edad = if_else(
      is.na(edad),
      median(edad, na.rm = TRUE),
      edad
    ),
    ingreso = if_else(
      is.na(ingreso),
      mean(ingreso, na.rm = TRUE),
      ingreso
    )
  )

# Revisar manualmente
head(datos_imputados)
summary(datos_imputados)

# ------------------------------------------------------------
# 11 y 12. Comparar antes y después de imputar
# ------------------------------------------------------------

summary_datos <- summary(datos)
summary_datos_imputados <- summary(datos_imputados)

comparacion_imputacion <- tibble(
  variable = c(
    "edad antes",
    "edad después",
    "ingreso antes",
    "ingreso después"
  ),
  media = c(
    mean(datos$edad, na.rm = TRUE),
    mean(datos_imputados$edad, na.rm = TRUE),
    mean(datos$ingreso, na.rm = TRUE),
    mean(datos_imputados$ingreso, na.rm = TRUE)
  ),
  mediana = c(
    median(datos$edad, na.rm = TRUE),
    median(datos_imputados$edad, na.rm = TRUE),
    median(datos$ingreso, na.rm = TRUE),
    median(datos_imputados$ingreso, na.rm = TRUE)
  ),
  desviacion = c(
    sd(datos$edad, na.rm = TRUE),
    sd(datos_imputados$edad, na.rm = TRUE),
    sd(datos$ingreso, na.rm = TRUE),
    sd(datos_imputados$ingreso, na.rm = TRUE)
  ),
  faltantes = c(
    sum(is.na(datos$edad)),
    sum(is.na(datos_imputados$edad)),
    sum(is.na(datos$ingreso)),
    sum(is.na(datos_imputados$ingreso))
  )
)

# Revisar manualmente:
summary_datos
summary_datos_imputados
comparacion_imputacion


# ============================================================
# Ejercicio 2. Pipeline reproducible con recipes
# ============================================================

set.seed(123)

split <- initial_split(datos, prop = 0.75)

train <- training(split)
test  <- testing(split)


# ------------------------------------------------------------
# Completar el pipeline solicitado en el enunciado
# ------------------------------------------------------------

receta <- recipe(compra ~ ., data = train) |>
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  prep()

train_proc <- bake(receta, new_data = train)
test_proc  <- bake(receta, new_data = test)

# Revisar manualmente
head(train_proc)
head(test_proc)

# ------------------------------------------------------------
# Verificar que no quedan NA en train_proc ni test_proc
# ------------------------------------------------------------

verificacion_na <- tibble(
  conjunto = c("train_proc", "test_proc"),
  total_na = c(
    sum(is.na(train_proc)),
    sum(is.na(test_proc))
  )
)

# Revisar manualmente:
verificacion_na

# ------------------------------------------------------------
# Pregunta sobre prep() antes del split
# ------------------------------------------------------------

#' ¿Por qué es incorrecto hacer prep() sobre todo el dataset antes del split?
#'
#' Cuando usamos prep(), R no solo prepara los datos: también aprende reglas a
#' partir de ellos. Por ejemplo, calcula medianas para rellenar valores
#' faltantes o modas para completar categorías.
#'
#' Si prep() ve todo el dataset antes de separar entrenamiento y prueba,
#' también usa información de los datos que supuestamente íbamos a reservar
#' para evaluar el modelo.
#'
#' Eso produce fuga de información. El conjunto de prueba deja de ser
#' completamente nuevo, porque ya ayudó indirectamente a construir las reglas
#' de preprocesamiento.
#'
#' Por eso el resultado puede parecer mejor de lo que realmente es: la
#' evaluación ya no mide de forma honesta cómo funcionaría el modelo con datos
#' nuevos.
#'
#' La secuencia correcta es:
#'
#' 1. Separar los datos en train y test.
#' 2. Con train aprender las reglas: medianas, modas e imputaciones.
#' 3. Aplicar esas reglas a train.
#' 4. Aplicar esas mismas reglas a test.


# ============================================================
# Ejercicio 3. Razonamiento conceptual
# ============================================================

#' 1. Personas con peor salud no responden la pregunta sobre su propia salud
#' 
#' El mecanismo probable es MNAR, porque la ausencia depende del valor no
#' observado: las personas con peor salud son precisamente quienes tienden a
#' no responder.
#'
#' 2. Efecto sobre un modelo predictivo
#'
#' El modelo puede quedar sesgado, porque aprende con una muestra que no
#' representa bien a la población. En este caso podría subestimar la mala
#' salud y producir predicciones demasiado optimistas.
#'
#' 3. Imputar ingreso usando todo el dataset antes del split
#'
#' El error es fuga de información. Al imputar con todo el dataset, el conjunto
#' de prueba participa indirectamente en el cálculo de la media, mediana o moda
#' usada en el preprocesamiento.
#'
#' 4. Corrección del error anterior
#'
#' La corrección es separar primero entrenamiento y prueba. Luego se calculan
#' los valores de imputación solo con train y se aplican esas mismas reglas
#' tanto a train como a test.
#'
#' 5. Variable con 65% de valores faltantes
#'
#' No la eliminaría automáticamente. Un 65% de faltantes es alto, pero la
#' decisión depende de la importancia de la variable, del mecanismo de ausencia
#' y de su relación con la variable objetivo.
#'
#' 6. Aspectos a revisar antes de eliminarla
#'
#' Revisaría si la variable es sustantivamente importante, si los faltantes son
#' aleatorios o informativos, si se relaciona con la respuesta y si puede
#' imputarse o resumirse mediante un indicador de ausencia.
#'
#' 7. Cuándo agregar un indicador binario de ausencia
#'
#' Conviene agregar un indicador binario cuando el hecho de que falte el dato
#' tiene significado analítico propio o puede aportar información predictiva.
#'
#' 8. Ejemplo concreto de indicador de ausencia
#'
#' Por ejemplo, si una persona no reporta ingreso, la variable
#' ingreso_faltante = 1 puede capturar un patrón asociado con privacidad,
#' informalidad laboral o nivel socioeconómico.
#'
#' 9. Cuándo preferir imputación múltiple con mice
#'
#' Preferiría imputación múltiple con mice cuando hay varias variables con
#' faltantes, se busca inferencia más cuidadosa y se quiere reflejar la
#' incertidumbre generada por no observar los datos reales.
