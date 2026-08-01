library(httr)
library(jsonlite)
library(dplyr)
library(purrr)


hashid <- "f48603fdbfcadbd5082b9bd798a30d16"
palabra_clave <- "jabon"


extraer_pagina <- function(numero_pagina) {
  
  url_api <- paste0("https://us1-search.doofinder.com/5/search?hashid=", hashid,
                    "&query=", palabra_clave,
                    "&page=", numero_pagina)
  
  respuesta <- GET(
    url_api,
    add_headers(
      `Origin` = "https://www.drogueriascafam.com.co",
      `Referer` = "https://www.drogueriascafam.com.co/",
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/120.0.0.0 Safari/537.36"
    )
  )
  
  texto_json <- content(respuesta, as = "text", encoding = "UTF-8")
  datos_crudos <- fromJSON(texto_json)
  
  # Extraemos la tabla interna
  tabla <- datos_crudos$results
  
  
  if(length(tabla) == 0) return(NULL)
  
  
  tibble(
    producto = if("title" %in% names(tabla)) tabla$title else NA,
    precio = if("price" %in% names(tabla)) tabla$price else NA,
    precio_normal = if("best_price" %in% names(tabla)) tabla$best_price else NA,
    enlace = if("link" %in% names(tabla)) tabla$link else NA
  )
}

tabla_maestra_jabones <- map_dfr(1:16, extraer_pagina)

tabla_maestra_jabones <- distinct(tabla_maestra_jabones)


cat("Total Jabones", nrow(tabla_maestra_jabones), "\n\n")


head(tabla_maestra_jabones, 15)

