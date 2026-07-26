#'-----------------------------------------------------------------------------
#' Ejercicio 1. Clasificación de tipos de datos
#'-----------------------------------------------------------------------------

#' 1. Microdatos GEIH del DANE (.csv de 300.000 filas)
#' - Tipo: Estructurada.
#' - Razón: Corresponde a datos con esquema fijo, organizados en filas
#' (observaciones) y columnas (variables) con tipos definidos. En otras palabras,
#' los datos se pueden organizar en una tabla
#' - Preprocesamiento mínimo: Importar el archivo como tibble (con funciones como
#' read_csv()), verificar tipos de variables, realizar la limpieza y adecuación
#' de los datos según corresponda
#'
#' 2. Respuesta JSON de la API pública datos.gov.co con indicadores de calidad 
#' del aire
#' - Tipo: Semi-estructurada.
#' - Razón: Los archivos tipos JSON posee etiquetas y estructura jerárquica, pero 
#' no un esquema tabular estricto; puede contener objetos anidados, listas o campos 
#' ausentes.
#' - Preprocesamiento mínimo: Revisar y parsear el JSON y aplanar estructuras 
#' anidadas para convertirlas en columnas de un tibble.
#'
#' 3. Grabaciones de audio de audiencias judiciales de la Rama Judicial
#' - Tipo: No estructurada.
#' - Razón: El audio carece de un esquema tabular predefinido y no es directamente
#' consumible por algoritmos estadísticos convencionales.
#' - Preprocesamiento mínimo: Mediante alguna técnica conocida transformar los
#' datos de audio en secuencias numéricas o alguna otra estructura similar que
#' sea organizable en un tibble
#'
#' 4. Factura electrónica emitida por la DIAN (XML)
#' - Tipo: Semi-estructurada.
#' - Razón: XML tiene etiquetas y jerarquías, pero la estructura puede variar
#' entre registros y requiere revisión y parseo para obtener una representación 
#' tabular.
#' - Preprocesamiento mínimo: Revisar y parsear el XML, extraer atributos 
#' relevantes y convertirlos en variables de un tibble.
#'
#' 5. Tabla HTML de Wikipedia scrapeada con rvest
#' - Tipo: Semi-estructurada.
#' - Razón: HTML es un tipo de archivo semi-estructurado por su organización en 
#' etiquetas; aunque una tabla HTML puede contener estructura tabular, requiere 
#' de parseo para extraerse.
#' - Preprocesamiento mínimo: usar rvest para extraer la tabla (html_table()),
#' hacer limpieza y adecuación de los datos antes de convertirla a tibble.


#'-----------------------------------------------------------------------------
#' Ejercicio 2: JSON anidado a DataFrame
#'-----------------------------------------------------------------------------

library(jsonlite); library(dplyr); library(tibble)
url  <- "https://jsonplaceholder.typicode.com/users"
raw  <- fromJSON(url)

# 1. Explorar la estructura del objeto
str(raw)

#' Columnas anidadas:
#' - address
#' - company
#' Dentro de address, el subcampo geo también está anidado.
#'
#' 2. Aplanar el JSON a un tibble con una fila por usuario
raw_flat <- flatten(raw)

users_tbl <- as_tibble(raw_flat) |>
  transmute(
    name = name,
    email = email,
    address.city = address.city,
    address.geo.lat = address.geo.lat,
    address.geo.lng = address.geo.lng,
    company.name = company.name
  )

users_tbl

#' Tipo de dato de la fuente:
#' Es una fuente semi-estructurada.
#'
#' 3. ¿Por qué fromJSON no produce directamente un tibble plano?
#' - Porque el JSON conserva una jerarquía interna: Se evidencia que address y 
#' company son objetos anidados, y dentro de address aparece geo como otro nivel
#' de anidamiento adicional del archivo. fromJSON interpreta esa estructura, 
#' pero no se convierte automáticamente en columnas atómicas. Por eso hay que
#' aplanar el objeto antes de dejarlo listo para análisis.


#'-----------------------------------------------------------------------------
#' Ejercicio 3: matriz de diseño
#'-----------------------------------------------------------------------------

library(titanic)
library(dplyr)
library(Matrix)

# 1. Cargar los datos y conservar únicamente las variables solicitadas
data("titanic_train")

datos_modelo <- titanic_train |>
  select(Age, Fare, SibSp, Parch) |>
  na.omit()

# 2. Construir la matriz de diseño X centrada y escalada
X <- scale(as.matrix(datos_modelo))

# Dimensiones de X y memoria que ocupa
dim(X)
format(object.size(X), units = "auto")

#' X es la matriz de diseño estandarizada:
#' - filas: pasajeros
#' - columnas: variables numéricas (age, fare, sibsp, parch)
#' scale() centra cada variable en 0 y la escala a desviación estándar 1.

# 3. Calcular X'X
XTX <- crossprod(X) # Esto es equivalente a t(X) %*% X

XTX

# Verificar si crossprod(X) es idéntico a t(X) %*% X
identical(crossprod(X), t(X) %*% X)

#' La diagonal de X'X, cuando X está escalada, representa la suma de cuadrados
#' de cada variable estandarizada. En este caso, cada elemento diagonal es
#' n - 1, donde n es el número de observaciones.

diag(XTX)

# 4. Construir la versión dispersa
X_sparse <- Matrix(X, sparse = TRUE)

# Comparar tamaño en memoria
format(object.size(X), units = "auto")
format(object.size(X_sparse), units = "auto")

#' En este caso no tiene sentido usar formato disperso. La razón es que age,
#' fare, sibsp y parch son variables numéricas densas, y por tanto la matriz 
#' resultante no tiene una cantidad considerable de cero. El formato disperso 
#' es útil cuando la mayoría de entradas son cero.

#' 5. Estructura de datos para modelar las relaciones familiares entre pasajeros
#' Se usaría un un grafo.
#' Razón: sibsp y parch describen relaciones entre pasajeros. Un grafo permite
#' representar pasajeros como nodos y y relaciones familiares como aristas, 
#' que resulta más práctico que una tabla cuando el objeto de estudio son las
#' conexiones.


#'-----------------------------------------------------------------------------
#' Ejercicio 4: concentración de distancias
#'-----------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)

set.seed(42)

# Valores de p solicitados
p_vals <- c(1, 2, 5, 10, 50, 100, 500, 1000, 5000)

# Número de puntos y número de distancias a calcular por cada p
n_puntos <- 500
m_pares  <- 200

# Distancia euclidiana entre dos vectores
dist_euclidiana <- function(x, y) {
  sqrt(sum((x - y)^2))
}

# Distancia coseno entre dos vectores
dist_coseno <- function(x, y) {
  1 - sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
}

# Genera un conjunto de 200 pares distintos de índices
pares_aleatorios <- function(n, m) {
  pares <- t(combn(n, 2))
  idx <- sample.int(nrow(pares), m)
  pares[idx, , drop = FALSE]
}

# Calcula media y CV para un valor dado de p
resumen_por_p <- function(p) {
  X <- matrix(runif(n_puntos * p), nrow = n_puntos, ncol = p) #' Esto garantiza
  #' la contenencia de los puntos en el producto cartesiano [0,1]^p
  pares <- pares_aleatorios(n_puntos, m_pares)
  
  d_euclid <- apply(pares, 1, function(ii) {
    dist_euclidiana(X[ii[1], ], X[ii[2], ])
  })
  
  d_cos <- apply(pares, 1, function(ii) {
    dist_coseno(X[ii[1], ], X[ii[2], ])
  })
  
  tibble(
    p = p,
    media_euclidiana = mean(d_euclid),
    cv_euclidiana = sd(d_euclid) / mean(d_euclid),
    media_coseno = mean(d_cos),
    cv_coseno = sd(d_cos) / mean(d_cos)
  )
}

# Ejecutar la simulación para todos los p
resultados <- bind_rows(lapply(p_vals, resumen_por_p))

resultados

#' 1. Reportar distancia media y coeficiente de variación (CV)
#'    para euclidiana y coseno para c(1, 2, 5, 10, 50, 100, 500, 1000, 5000) 
#'    respectivamente.
#'    - Distancia media euclidiana: c(0.38,0.56,0.88,1.3,2.87,4.09,9.14,12.9,
#'                                    28.9)
#'    - CV Distancia media euclidiana: c(0.67,0.45,0.29,0.19,0.09,0.06,0.03,0.02,
#'                                       0,01)
#'    - Distiancia media coseno: c(0,0.19,0.23,0.25,0.25,0.25,0.25,0.25,
#'                                 0.25)
#'    - CV Distancia media coseno: c(NaN,1.0,0.6,0.39,0.19,0.13,0.06,0.04,0.02)


# 2. Gráficas de distancia media y CV vs. p, con escala log en x
grafico_datos <- resultados |>
  select(p, media_euclidiana, cv_euclidiana, media_coseno, cv_coseno) |>
  pivot_longer(
    cols = -p,
    names_to = "estadistico",
    values_to = "valor"
  ) |>
  mutate(
    tipo = case_when(
      grepl("euclidiana", estadistico) ~ "Euclidiana",
      grepl("coseno", estadistico) ~ "Coseno"
    ),
    medida = case_when(
      grepl("media", estadistico) ~ "Distancia media",
      grepl("cv", estadistico) ~ "CV de la distancia media"
    )
  )

ggplot(grafico_datos, aes(x = p, y = valor, color = tipo)) +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~ medida, scales = "free_y") +
  labs(
    x = "p (escala log10)",
    y = "Valor",
    color = "Distancia",
  )

#' 3. ¿A partir de qué p el CV cae por debajo del 5%?
#' Para euclidiana:
p_umbral_euclidiana <- resultados |>
  filter(cv_euclidiana < 0.05) |>
  summarise(p_min = min(p, na.rm = TRUE)) |>
  pull(p_min)

p_umbral_euclidiana

p_umbral_coseno <- resultados |>
  filter(cv_coseno < 0.05) |>
  summarise(p_min = min(p, na.rm = TRUE)) |>
  pull(p_min)

p_umbral_coseno


#' Para distancia euclidiana el CV cae por debajo del 5% en p = 500 y para la
#' distancia coseno ocurre en p = 1000.

#' Interpretación para k-NN:
#' Si el CV de variación es muy pequeño significa que no hay mucha variabilidad
#' al rededor de la distancia media, es decir que la distancia entre puntos empieza
#' a verse cada ves más alejada y uniforme entre ellos, es menos probable encontrar
#' puntos cercanos antes de recorrer la distancia media, afectando la manera como
#' k-NN se vuelve eventualmente ineficaz en etiquetar qué puntos son cercanos entre
#' sí y cuáles no. Esto ocurre en ambos casos euclidiano y por distancia coseno

#'-----------------------------------------------------------------------------
#' Ejercicio 5: PCA y reducción de dimensionalidad
#'-----------------------------------------------------------------------------

library(titanic)
library(dplyr)
library(ggplot2)
library(tibble)

#' 1. Preparar los datos del Titanic
#' Se conservan las variables numéricas pedidas y la variable survived para
#' comparar luego los grupos de K-Means.

# Seleccionar y preparar datos
data("titanic_train")

datos <- titanic_train |>
  select(Survived, Age, Fare, SibSp, Parch, Pclass) |>
  na.omit()

y <- datos$Survived
X <- datos |>
  select(Age, Fare, SibSp, Parch, Pclass)

# 2. Aplicar PCA con escalamiento interno
pca <- prcomp(X, scale. = TRUE)

# Varianza explicada y varianza acumulada
var_exp <- (pca$sdev^2) / sum(pca$sdev^2) # Var(X_i)/Σ_i(Var(X_i))
var_acum <- cumsum(var_exp)

res_pca <- tibble(
  componente = seq_along(var_exp),
  var_explicada = var_exp,
  var_acumulada = var_acum
)

res_pca

# Número mínimo de componentes para retener al menos 80% y 95% de la varianza
n80 <- which(var_acum >= 0.80)[1]
n95 <- which(var_acum >= 0.95)[1]

n80 # Se necesitan 3 componentes para retener un 80% de la varianza
n95 # Se necesitan 3 componentes para retener un 95% de la varianza

# 3. Scree plot: varianza acumulada vs. número de componente
ggplot(res_pca, aes(x = componente, y = var_acum)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = res_pca$componente) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Número de componente",
    y = "Varianza acumulada",
    title = "Scree Plot de Varianza acumulada vs componente en Titanic"
  )

# Varianza explicada vs. número de componente
ggplot(res_pca, aes(x = componente, y = var_explicada)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = res_pca$componente) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Número de componente",
    y = "Varianza acumulada",
    title = "Scree plot de Varianza explicada vs componente en Titanic"
  )

# 4. Examinar los loadings de PC1 y PC2
loadings <- as.data.frame(pca$rotation[, 1:2]) |>
  rownames_to_column("variable")

loadings # Ver las cargas de las componentes 1 y 2

# Variables dominantes en PC1 según valor absoluto (Ordenado)
loadings |>
  mutate(abs_PC1 = abs(PC1)) |>
  arrange(desc(abs_PC1)) |>
  select(variable, PC1)

# Variables dominantes en PC2 según valor absoluto (Ordenado)
loadings |>
  mutate(abs_PC2 = abs(PC2)) |>
  arrange(desc(abs_PC2)) |>
  select(variable, PC2)

#' Interpretación:
#' - Para PC1 notamos que las variables más dominantes son Pclass y Age, en el
#' contexto de los datos de Titanic significa que PC1 parece estar explicado
#' mayoritariamente por la clase social y la edad.
#' 
#' - Para PC2 vemos que las variantes dominantes son Parch y SibSp en donde la
#' priomera variable se refiere a si el pasajero viajaba con sus padres o hijos
#' a bordo y la segunda si viajaba con hermanos o cónyuge. Por lo que la inter-
#' pretación sería que la segunda componente está bien explicada por variables
#' que explican la carga de acompañamiento familiar o de grupo.
#' 
#' Para ambas componentes podemos ver que la variable Fare parece aportar infor-
#' mación de manera importante tanto a PC1 como a PC2, en el contexto obtenido
#' nos permite dar una noción que tanto la clase social y la edad, como la can-
#' dad de acompañantes conocidos del pasajero influyen en la tarifa que este
#' pagaba.


# 5. K-Means sobre la representación original escalada
X_scaled <- scale(as.matrix(X))

set.seed(42)
km_X <- kmeans(X_scaled, centers = 2, nstart = 25)

# K-Means sobre los dos primeros componentes principales
scores_2pc <- pca$x[, 1:2]

set.seed(42)
km_PC <- kmeans(scores_2pc, centers = 2, nstart = 25)

#' Comparar los grupos resultantes con survived
#' Como las etiquetas de K-Means son arbitrarias, se evalúan ambas
#' correspondencias posibles y se toma la mejor.
acc_binaria <- function(cluster, y) {
  pred_1 <- ifelse(cluster == 1, 0, 1)
  pred_2 <- 1 - pred_1
  max(mean(pred_1 == y), mean(pred_2 == y))
}

acc_X <- acc_binaria(km_X$cluster, y)
acc_PC <- acc_binaria(km_PC$cluster, y)

acc_X   # % de sobrevivientes en la agrupación sobre los datos originales
acc_PC  # % de sobrevivientes en la agrupación sobre los datos transformados (PCA)

# Tablas de contingencia para inspección visual
table(km_X$cluster, y)
table(km_PC$cluster, y)

#' Conclusión:
#' Al comparar los datos originales y las dos primeras componentes principales
#' mediante k-means con k=2, podemos ver que hay una alta similitud si se comparan
#' los clústeres generados en la variable original como en la transformación
#' mediante PCA siendo PCA,ligeramente mejor, lo que sugiere que el algoritmo
#' k-means en este caso está detectando una tendencia similar en los grupos
#' generados sobre los datos pero no explica completamente a la variable survived
#' en ninguno de los dos casos de agrupamiento.
#' 
#' Hay que destacar que en ambos casos parece que el algoritmo k-means parece
#' captar patrones altamente similares de cercanía tanto en los datos originales
#' como en los transformados mediante PCA.


 







