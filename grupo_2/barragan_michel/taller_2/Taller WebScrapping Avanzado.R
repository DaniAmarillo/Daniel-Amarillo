# ------------------------------- CLASE 3: CASO REAL WERBSCRAPPING ---------------------------------
# Autor: Michel Mendivenson Barragán Zabala
# Materia: Minería de datos. 2026-1
# --------------------------------------------------------------------------------------------------

library(rvest)             # Scrapping
library(stringr)           # Manipulación de texto

# ========================================== EJERCICIO 1 ===========================================
# Desarrolle una función que reciba como parámetro un término de búsqueda (por ejemplo, “jabón”) y 
# retorne un data frame con la información de precio, marca y descripción obtenida del sitio web 
# https://www.drogueriascafam.com.co/

# NOTAS: 
# - Desafíos: Página renderizada de forma dinámica y Lazy loading
# - Además de los desafíos propios del proyecto, RSelenium no corre en mi PC personal sino hasta que 
#   se levanta el servicio de chomedrive de forma manual desde la terminal.

# DESARROLLO DE LA FUNCIÓN: 
# La función se desarrollará usando la función experimental rvest::read_html_live que facilita el
# acceso y extracción de datos de este estilo, y que además funciona con este sitio. Con los siguientes
# pasos:
# 
#   1. Realizar la búsqueda
#   2. Reconocer la cantidad de productos que la búsqueda devolvió. 
#   3. Hacer scroll hasta que todos los productos estén cargados. 
#      Criterio de parada: Ya se trajeron todos los elementos que se supone se recuperan en la búsqueda)
#   4. Extraer la información necesaria. 

cafam_search <- function(product = 'Jabón', live = FALSE){
  
  # PASO 1. Realizar la búsqueda
  
  url <- paste0('https://www.drogueriascafam.com.co/3-medicamentos#2fce/fullscreen/m=and&q=', 
                product |>
                  str_to_lower() |> 
                  str_replace_all(c(
                    "á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u"
                  )))
  
  search = read_html_live(url)
  Sys.sleep(5)
  
  if (live) search$view()   # Visualizar la página
  
  # PASO 2. Cantidad de productos que devolvió la búsqueda 
  
  all <- search |> 
    html_elements('.dfd-meta strong') |> 
    html_text2() |> 
    as.numeric()
  
  # PASO 3. Hacer scroll hasta que todos los productos estén cargados. 
  
  current <- search |> 
    html_elements('.dfd-card-title') |> 
    html_text2() |> 
    length()
  
  cat('Retrieving products:\n')
  pb = txtProgressBar(min = 0, max = all, initial = current, style = 3)
  
  while (current < all){
    search$session$Runtime$evaluate("
    var container = document.querySelector('.dfd-content');
    container.scrollTop = container.scrollHeight;
    ")
    Sys.sleep(2)
    
    current <- search |> 
      html_elements('.dfd-card-title') |> 
      html_text2() |> 
      length()
    
    setTxtProgressBar(pb = pb, value = current)
  }
  
  close(pb)
  
  # PASO 4: Extraer la información necesaria.
  
  products <- data.frame(
    product = search |>
      html_elements('.dfd-card-content .dfd-card-title') |> 
      html_text2(), 
    
    description = search |>
      html_elements('.dfd-card-content') |> 
      html_element('.dfd-card-description') |>                       # Algunos productos no cuentan con descripción
      html_text2(), 
    
    price = search |>
      html_elements('.dfd-card-content .dfd-card-price--sale') |>    # Por como se presentan los productos, el price 
      html_text2() |>                                                # sale es el que todos los productos están a la venta
      str_remove_all('\\$|,') |> 
      as.numeric(), 
    
    sale.price = search |>                                         # Estos son los precios normales de los productos, 
      html_elements('.dfd-card-content') |>                          # hay que hacer cambios entre esta columna y price 
      html_element('.dfd-card-price') |>                             # para que en price queden los precios usuales de 
      html_text2() |>                                                # los productos y en sale price queden los de promoción 
      str_remove_all('\\$|,') |>                                     # en caso de que el producto esté en promoción
      as.numeric(),
    
    currency = search |>  
      html_elements('input[name="currency"]') |> 
      html_attr("value"),
    
    PPU = search |> 
      html_elements('.dfd-card-content .dfd-card-pum') |>
      html_text2() |> 
      str_remove_all('\\$|,|PUM:') |>
      str_trim() |> 
      str_extract("^\\S+") |> 
      as.numeric(),
    
    unit = search |> 
      html_elements('.dfd-card-content .dfd-card-pum') |>
      html_text2() |> 
      str_remove_all('\\$|,|PUM:') |>
      str_trim() |> 
      str_extract("(?<=\\s).*$"), 
    
    rating = search |>
      html_elements('.dfd-review .dfd-rating__count') |> 
      html_text2() |> 
      str_remove_all('\\(|\\)') |> 
      as.numeric(),
    
    category = search |>  
      html_elements('input[name="item_category"]') |> 
      html_attr("value"),
    
    brand = search |>  
      html_elements('input[name="item_brand"]') |> 
      html_attr("value")
  )
  
  search$session$close()
  
  # Poner el precio original en price para los productos en promoción y el precio de venta en sale.price 
  placeholder <- products$price
  products$price[!is.na(products$sale.price)] = products$sale.price[!is.na(products$sale.price)]
  products$sale.price[!is.na(products$sale.price)] = placeholder[!is.na(products$sale.price)]
  
  return(products)
}

jabon <- cafam_search(product = 'JABÓN', live = T)

# A partir de los resultados obtenidos, responda las siguientes preguntas:
#   
#   ¿Cuántos productos logró extraer por página?
#     ANSWER: Todos los productos son cargados en una sola página, pero cada vez que se hace scroll
#             cargan de a 20 productos,

nrow(jabon)

#   ¿Qué porcentaje de los productos cuenta con un precio visible?
#     ANSWER: Por como se accede a la info todos los productos parecen traer precios visibles

sum(!is.na(jabon$price))/nrow(jabon)

#   ¿Qué selectores dejaron de funcionar o requirieron ajustes durante el proceso?
#     ANSWER: Como todo se consulta de la misma página de búsuqeda ninguno, pero fue díficil lograr 
#             hacer scroll porque el selector no es window (El header siempre está visible).

#   ¿Qué diferencias identificó entre el scraping realizado con rvest y con RSelenium?
#     ANSWER: RSelenium en general es más lento y requiere más condiciones para poder acceder a la
#             página pero a cambio deja hacer scrapping de págionas algo más complejas.

# ==================================================================================================

# EOF