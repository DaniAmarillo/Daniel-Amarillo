library(jsonlite)
library(dplyr)
library(tibble)
##
#Ejercicio 1: clasificación de tipos de datos
#Para cada fuente de datos, indique: (a) si es estructurada,
#semi-estructurada o no estructurada; (b) la razón; (c) el
#paso de preprocesamiento mínimo para convertirla en un
#tibble analizable.
#1. Microdatos GEIH del DANE (archivo .csv de 300.000 filas) estructurado
#2. Respuesta JSON de la API pública datos.gov.co con semiestructurado
#indicadores de calidad del aire
#3. Grabaciones de audio de audiencias judiciales de la Rama no estructurado
#Judicial
#4. Factura electrónica emitida por la DIAN (formato XML) semiestructurado
#5. Tabla HTML de Wikipedia scrapeada con rvest en las semiestructurado
##

# Descargar JSON
url <- "https://jsonplaceholder.typicode.com/users"
raw <- fromJSON(url)

# Aplanar a tibble
users_tbl <- as_tibble(raw) %>%
  mutate(
    city = address$city,
    lat = as.numeric(address$geo$lat),
    lng = as.numeric(address$geo$lng),
    company_name = company$name
  ) %>%
  select(name, email, city, lat, lng, company_name)

# Ver resultado
users_tbl

#1. Explore la estructura con str(raw). ¿Qué columnas son listas anidadas?
# address, address$geo, company

#3. ¿Qué tipo de dato es esta fuente? ¿Por qué fromJSON no produce directamente un tibble plano?
# Es un dato semiestructurado
#fromJson no produce directamente un tibble plano porque respeta la estructura del JSON 

