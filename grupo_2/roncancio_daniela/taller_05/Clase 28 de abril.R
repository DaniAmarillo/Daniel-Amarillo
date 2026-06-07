
# Sesión 6

##  Ejercicio 1: Clasificación de tipos de datos

## Para cada fuente de datos, indique: (a) si es estructurada, semi-estructurada o no estructurada;
## (b) la razón; (c) el paso de preprocesamiento mínimo para convertirla en un tibble analizable.

# 1. Microdatos GEIH del DANE (archivo .csv de 300.000 filas)

# a. ¿Qué tipo de estructura es? Estructurados
# b. Razón: Presentan un esquema fijo (filas y columnas con tipos definidos), además son direcetamente consultables
# con SQL y están listos para análisis estadístico.
# c. El paso de preprocesamiento mínimo para convertirla en un tibble analizable: Limpieza de datos


# 2. Respuesta JSON de la API pública datos.gov.co con indicadores de calidad del aire

# a. ¿Qué tipo de estructura es? Semi-estructurados
# b. Razón: Presentan jerarquía y etiquetas, pero sin esquema tabular rígido. La estructura varía entre registros.
# c. El paso de preprocesamiento mínimo para convertirla en un tibble analizable: Aplanar o normalizar la estrutura anidada

# 3. Grabaciones de audio de audiencias judiciales de la Rama Judicial

# a. ¿Qué tipo de estructura es? No estructurados
# b. Razón: No tienen formato predefinido.Requieren feature engineering para convertirse en representación numérica que los algoritmos puedan consumir.
# c. El paso de preprocesamiento mínimo para convertirla en un tibble analizable: Transcripción automática del audio a texto mediante reconocimiento de voz (STT - Speech to Text)

# 4. Factura electrónica emitida por la DIAN (formato XML)

# a. ¿Qué tipo de estructura es? Semi-estructurados
# b. Razón:Presentan jerarquía y etiquetas, pero sin esquema tabular rígido. La estructura varía entre registros.
# c. El paso de preprocesamiento mínimo para convertirla en un tibble analizable:Parsear y aplanar el XML

# 5. Tabla HTML de Wikipedia scrapeada con rvest en las clases 2–3

# a. ¿Qué tipo de estructura es? Semi-estructurados
# b. Razón:Presentan jerarquía y etiquetas, pero sin esquema tabular rígido. La estructura varía entre registros.
# c. El paso de preprocesamiento mínimo para convertirla en un tibble analizable:Extraer y limpiar la tabla

## Ejercicio 2: JSON anidado a DataFrame

# Descargue el siguiente JSON público con datos de usuarios (incluye campos anidados address y company):

library(Matrix)
library(jsonlite)
library(dplyr)
library(tibble)
library(tidyverse)

url <- "https://jsonplaceholder.typicode.com/users"
raw <- fromJSON(url)

# 1. Explore la estructura con str(raw). ¿Qué columnas son listas anidadas?

str(raw)

# address: Contiene 5 variables dentro -> street, suite, city, zipcode y geo
## geo: contiene lat y lng
# company: Contiene 3 variables dentro -> name, catchPhrase y bs


# 2. Aplane el objeto a un tibble con una fila por usuario, extrayendo al menos: 
# name, email, address.city, address.geo.lat, address.geo.lng y company.name.

tibble_usuarios <- tibble(
  name            = raw$name,    
  email           = raw$email,
  address_city    = raw$address$city,
  adress_geo_lat  = raw$address$geo$lat,
  adress_geo_lng  = raw$address$geo$lng,
  company_name    = raw$company$name
)

tibble_usuarios 

# 3. ¿Qué tipo de dato es esta fuente? ¿Por qué fromJSON no produce directamente un tibble plano?

# Al ser una fuente semi-estructurada, fromJSON convierte el JSON a R pero respeta la jerarquía original, 
# dejando address y company como data frames anidados dentro del principal. Para obtener un tibble plano 
# hay que extraer manualmente cada campo anidado con $.

## Ejercicio 3: matriz de diseño

# Con titanic.csv y las variables age, fare, sibsp, parch (elimine NA)
# 1. Construya X centrada y escalada con scale(). Reporte sus dimensiones y la memoria que ocupa.

X <- titanic_train |>
  select(Age, Fare, SibSp, Parch) |> 
  drop_na() |>
  scale()

dim(X)
object.size(X)

#  La matriz X tiene 714 observaciones (filas) y 4 variables (columnas): Age, Fare, SibSpy Parch

# 2. Calcule XTX ¿Qué representa la diagonal cuando X está escalada?

XtX <- t(X) %*% X
XtX

# Cuando X está escalada, la diagonal representa el número de observaciones menos uno para
# cada variable, esto ocurre porque al escalar la matriz se centro y se estandarizo, luego 
# la varianza es 1, entonces X es proporcionañ a la matriz de correlaciones.

# 3. Construya la versión dispersa con Matrix(X, sparse = TRUE). Compare el tamaño en memoria con object.size(). ¿Tiene sentido usar formato disperso aquí?

X_dispersa = Matrix(X, sparse = TRUE)

object.size(X)
object.size(X_dispersa)

# En este contexto no serviría mucho ya que X no tiene ceros, el formato disperso ahorra memoria cuando la matriz tiene muchos ceros.
# Aquí ocupa más memoria.

# 4. ¿Qué estructura de datos usaría para modelar las relaciones familiares entre pasajeros (sibsp, parch)? Justifique.

#Usaría una matriz dispersa o un grafo, porque sibsp y parch representan conexiones entre pasajeros, no valores continuos. 
# Una matriz de adyacencia donde cada fila y columna es un pasajero y el valor indica si están relacionados sería lo más apropiado. 
# Como la mayoría de pasajeros no están relacionados entre sí, la matriz tendría muchos ceros, haciendo el formato disperso ideal.

## Ejercicio 4: Concentración de distancias

# Replique la demostración de la clase para p∈{1,2,5,10,50,100,500,1000,5000} (use set.seed(42)):

# 1. Para cada p, genere 500 puntos uniformes en [0,1]p y calcule 200 distancias euclidianas entre pares. 
# Reporte la distancia media y el coeficiente de variación (CV).

library(tidyverse)

set.seed(42)

ps <- c(1, 2, 5, 10, 50, 100, 500, 1000, 5000)

resumen <- sapply(ps, function(p) {
  puntos <- matrix(runif(500 * p), nrow = 500, ncol = p)
  dists  <- sapply(1:200, function(i) {
    sqrt(sum((puntos[i, ] - puntos[i + 200, ])^2))
  })
  c(media = mean(dists), cv = sd(dists) / mean(dists))
})

df <- data.frame(
  p          = ps,
  dist_media = round(resumen["media", ], 4),
  cv_pct     = round(resumen["cv", ] * 100, 2)
)
# 2. Grafique distancia media y CV vs. p (escala log en x) con ggplot2.

ggplot(df, aes(x = p, y = dist_media)) +
  geom_line() + geom_point() +
  scale_x_log10() +
  labs(title = "Distancia media vs p", x = "p (escala log)", y = "Distancia media")

ggplot(df, aes(x = p, y = cv_pct)) +
  geom_line() + geom_point() +
  scale_x_log10() +
  geom_hline(yintercept = 5, color = "red", linetype = "dashed") +
  labs(title = "CV vs p", x = "p (escala log)", y = "CV (%)")


# 3. A partir de qué p el CV cae por debajo del 5%? ¿Qué implica esto para K-NN?

df[df$cv_pct < 5, ]

# Cuando el CV cae por debajo del 5% significa que todas las distancias son casi iguales.
# Para K-NN esto es un problema porque si todos los puntos están a la misma distancia, 
# el algoritmo no puede distinguir vecinos cercanos de lejanos.

# 4. Repita el punto 1 usando distancia coseno en lugar de euclidiana. ¿Se concentra igual de rápido?

set.seed(42)

ps <- c(1, 2, 5, 10, 50, 100, 500, 1000, 5000)

resumen_coseno <- sapply(ps, function(p) {
  puntos <- matrix(runif(500 * p), nrow = 500, ncol = p)
  dists  <- sapply(1:200, function(i) {
    a <- puntos[i, ]
    b <- puntos[i + 200, ]
    1 - sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
  })
  c(media = mean(dists), cv = sd(dists) / mean(dists))
})

data.frame(
  p          = ps,
  dist_media = round(resumen_coseno["media", ], 4),
  cv_pct     = round(resumen_coseno["cv", ] * 100, 2)
)

# ¿Se concentra igual de rápido? Sí, el CV cae igualmente con p, pero la distancia 
# media converge a 1 en lugar de crecer, porque la similitud coseno entre vectores 
# aleatorios tiende a 0 en altas dimensiones.

# Ejercicio 5: PCA y reducción de dimensionalidad

# Con las variables numéricas del Titanic escaladas (age, fare, sibsp, parch, pclass):
  

# 1. Aplique PCA con prcomp(..., scale. = TRUE). ¿Cuántos componentes son necesarios para retener ≥ 80% de la varianza? ¿Y para ≥ 95%?

pca <- prcomp(X, scale. = TRUE)
summary(pca)

# Varianza acumulada
varianza_acum <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
varianza_acum 

# ¿Cuántos componentes para 80% y 95%?
which(varianza_acum >= 0.80)[1]
which(varianza_acum >= 0.95)[1]

# Para retener ≥ 80% de la varianza son necesarios 3 componentes y para ≥ 95% son necesrias 4 componentes.

# 2. Grafique el scree plot (varianza acumulada vs. número de componente) con ggplot2.

data.frame(
  componente = 1:length(varianza_acum),
  varianza   = varianza_acum
) |>
  ggplot(aes(x = componente, y = varianza)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = c(0.80, 0.95), color = "red", linetype = "dashed") +
  labs(title = "Scree plot - Varianza acumulada",
       x = "Número de componentes",
       y = "Varianza acumulada")

# 3. Examine los loadings de PC1 y PC2. ¿Qué variables dominan cada componente? Proponga una interpretación en el contexto del Titanic.

pca$rotation[, 1:2]

# PC1: Este componente principal esá dominado por SibSp (|-0.625|) y Parch (|-0.591|), el valor negativo indica que a más familiares a bordo, 
# menor es e valor de PC1.
## Interpretación: PC1 representa el tamaño del grupo familiar. Es decir, que pasajeros con valores bajos en PC1 viajaban con más familia, 
# los de valores altos viajaban solos.

#PC2: Fare tiene el valor más alto en valor absoluto.  PC2 representa el estatus socioeconómico y edad. Pasajeros con valores bajos en PC2 
# eran mayores y pagaron tarifas más altas (primera clase), los de valores altos eran jóvenes y pagaron menos (tercera clase).

# 4. Reto: aplique K-Means (k=2) sobre X original y sobre los 2 primeros PCs por separado. Compare los grupos resultantes con la variable survived. 
# ¿En cuál representación los grupos coinciden mejor con la supervivencia?


survived <- titanic_train |>
  select(Age, Fare, SibSp, Parch, Survived) |>
  drop_na() |>
  pull(Survived)

# K-Means sobre X original
km_original <- kmeans(scale(X), centers = 2, nstart = 25)

# K-Means sobre los 2 primeros PCs
km_pca <- kmeans(pca$x[, 1:2], centers = 2, nstart = 25)

# Comparar con survived
table(km_original$cluster, survived)
table(km_pca$cluster,      survived)

# K-Means sobre X original:

#      0    1
# 1  366  217   ← grupo grande, mayoría no sobrevivió
# 2   58   73   ← grupo pequeño, mayoría sobrevivió

# Grupo 1: 366 muertos vs 217 sobrevivientes → no sobrevivió
# Grupo 2: 58 muertos vs 73 sobrevivientes → sobrevivió

# K-Means sobre los 2 primeros PCs:
#      0    1
# 1   60   67   ← grupo pequeño, casi mitad y mitad
# 2  364  223   ← grupo grande, mayoría no sobrevivió

# Grupo 1: 60 muertos vs 67 sobrevivientes → casi mitad y mitad
# Grupo 2: 364 muertos vs 223 sobrevivientes → no sobrevivió

# ¿Cuál coincide mejor con la supervivencia? X original coincide mejor porque:
  
## - El grupo 2 tiene más sobrevivientes que muertos (73 vs 58)
## - La separación entre grupos es más clara
