#####------Ejercicio 1: Clasificación tipo de datos------#####

##1. Microdatos GEIH del DANE  
#a Son tipo de datos estructurados
#b Son representandos de forma tabular 
#c Realmente ya está listo para análisis de datos

##2. Respuesta JSON de la API pública con indicadores de calidad de aire
#a Semi-estructurado 
#b Está en forma de texto 
#c Requiere de un preprocesamiento para representarlo en formato tibble

##3. Grabaciones de audio de audiencias judiciales de la Rama Judicial
#a No estructurado
#b Al ser una grabación no representa una estructura fija
#c Se implementa una representación numérica para luego convertirlos en una matriz densa/dispersa

##4. Factura electrónica emitida por la DIAN
#a Semi-estructurado
#b Formato de archivo de texto plano
#c Aplanamiento

##5. Tabla HTML de Wikipedia scrapeada en clase
#a Estructurada
#b se usó la función rvest
#c ya está lista para limpieza de datos

#####------Ejercicio 2: JSON anidado a DF------#####

library(jsonlite); library(dplyr); library(tibble); library(tidyverse); library(ggplot2)
url  <- "https://jsonplaceholder.typicode.com/users"
raw  <- fromJSON(url)

##1. ¿Qué columnas son anidadas?
str(raw)
# Las columnas anidadas son adress,geo$adress y company

##2. Aplanamiento
datos_tbl <- as_tibble(raw) %>%
  mutate(
    city = address$city,
    lat  = address$geo$lat,
    lng  = address$geo$lng,
    company_name = company$name
  ) %>%
  select(name, email, city, lat, lng, company_name)

View(datos_tbl)

##3. Tipo de dato de fuente. ¿Por qué fromJSON no produce directamente un tibble plano?
str(datos_tbl)
#la fuente original es un json pero al aplanarlo se convierte en un DF
#no produce un tibble plano directamente por el anidamiento y columnas repetidas

#####------Ejercicio 3: Matriz de diseño------#####

##1

data_matriz <- titanic |>
  select(survived, pclass, age, fare, sibsp, parch) |>
  na.omit()
  as.matrix()
  
matriz_escalada<-scale(data_matriz, T, T)

#reporte
cat(
  "Dimensiones X:", dim(matriz_escalada), "|",
  "Tipo:", class(matriz_escalada)[1], "|",
  "Memoria:", format(object.size(matriz_escalada), units = "auto"),
  "\n"
)

##2

XtX <- crossprod(matriz_escalada)
#obtuvimos la matriz de cov estandarizada sin dividir entre n-1 (1044). Al hacer este paso obtenemos
#la matriz de correlación ya que estandarizamos la matriz centrada

##3

versión_dispersa<-Matrix(matriz_escalada, sparse= T)
format(object.size(matriz_escalada), units = "auto")
format(object.size(versión_dispersa), units = "auto")
#no tiene sentido emplear la versión dispersa ya que ocupa más espacio que la escalada y no hay muchos ceros

##4
#podría usar variables indicadoras.Sin embargo una regresión logística sería más ideal para
#analizar las probabilidades de sobreviviencia de las familias

#####------Ejercicio 4: Concentración de distancias------#####

##1
set.seed(42)
dimensiones <- c(1, 2, 5, 10, 50, 100, 500, 1000)

resumen <- sapply(dimensiones, function(p) {
  puntos <- matrix(runif(500 * p), nrow = 500, ncol = p)
  dists  <- sapply(1:200, function(i)
    sqrt(sum((puntos[i, ] - puntos[i + 200, ])^2)))
  c(media = mean(dists), cv = sd(dists) / mean(dists))
})

data.frame(
  p          = dimensiones,
  dist_media = round(resumen["media", ], 4),
  cv_pct     = round(resumen["cv", ] * 100, 2)   # coeficiente de variación
)

##2

df_resultados <- data.frame(
  p          = dimensiones,
  dist_media = resumen["media", ],
  cv         = resumen["cv", ]
)

# Graficamos
df_resultados %>%
  pivot_longer(cols = c(dist_media, cv), names_to = "metrica") %>%
  ggplot(aes(x = p, y = value, color = metrica)) +
  geom_line(size = 1) +
  geom_point() +
  scale_x_log10(breaks = dimensiones) + # Escala logarítmica en X
  facet_wrap(~metrica, scales = "free_y") +
  theme_minimal() +
  labs(title = "Concentración de Distancias",
       x = "Dimensión (p) en escala log",
       y = "Valor de la métrica")

##3
#después de p=500 el CV es menor a 0.05 y KNN se rompe por la maldición de la dimensionalidad

##4
set.seed(42)
dimensiones <- c(1, 2, 5, 10, 50, 100, 500, 1000, 5000)

# Tu función
dist_coseno <- function(a, b) {
  1 - (sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2))))
}

resumen_cos <- sapply(dimensiones, function(p) {
  puntos <- matrix(runif(500 * p), nrow = 500, ncol = p)
  
  
  dists <- sapply(1:200, function(i) {
    dist_coseno(puntos[i, ], puntos[i + 200, ])
  })
  
  c(media = mean(dists), cv = sd(dists) / mean(dists))
})

df_coseno <- data.frame(
  p          = dimensiones,
  dist_media = round(resumen_cos["media", ], 4),
  cv_pct     = round(resumen_cos["cv", ] * 100, 2)
)

print(df_coseno)

#para altas dimensiones la distancia del coseno es más informativa que la euclídea

#####------Ejercicio 5: PCA y reducción de dimensionalidad------#####

##1
pca_titanic<-prcomp(matriz_escalada, scale. = T)

varianza <- pca_titanic$sdev^2
var_relativa <- varianza / sum(varianza)
var_acumulada <- cumsum(var_relativa)

# resumen
resumen_pca <- data.frame(
  PC = 1:length(var_acumulada),
  Varianza_Acumulada = var_acumulada
)
print(resumen_pca)

#el 4to componente para retener >=0.8 de la varianza
#el 6to componente para retener >=0.95 de la varianza
##2
ggplot(resumen_pca, aes(x = PC, y = Varianza_Acumulada)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = c(0.8, 0.95), linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = 1:5) +
  labs(title = "Scree Plot: Varianza Acumulada",
       x = "Número de Componente", y = "Proporción de Varianza") +
  theme_minimal()
##3
pca_titanic$rotation[, 1:2]
#pclass es más dominante en valor absoluto en PC1 - podría ser el componente del
#estatus socioeconómico
#sibsp es más dominante es PC2 - identifica las familias grandes 

##4























