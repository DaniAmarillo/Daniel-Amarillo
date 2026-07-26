library(rvest)
library(stringr)

# =============================================================================
# JSS.vol()
# Recolecta metadata de los volúmenes listados en el archivo del Journal of
# Statistical Software.
# Retorna un data.frame con: id (numérico), order (posición dentro del año),
# year y url.
# =============================================================================
JSS.Vol <- function(){
  
  # Leer el HTML de la página de archivo
  archive <- read_html('https://www.jstatsoft.org/issue/archive')
  
  # Extraer título y URL de cada volumen
  vol <- data.frame(
    id  = archive |> html_elements(".media-heading .title") |> html_text2(), 
    url = archive |> html_elements(".media-heading .title") |> html_attr('href')
  )
  
  # Extraer el año eliminando el título del volumen del texto completo del encabezado
  vol$year <- stringr::str_remove(
    archive |> html_elements(".media-heading") |> html_text2(),
    paste0(vol$id, collapse = '|', end = ' , ')
  ) |> as.numeric()
  
  # Convertir id a numérico eliminando el prefijo "Volume "
  vol$no <- vol$id |> stringr::str_remove('Volume ') |> as.numeric()
  
  # Ordenar por año e id
  vol <- vol |> dplyr::arrange(year, id)
  
  # Posición del volumen dentro de su año de publicación
  vol$order <- sequence(table(vol$year))
  
  return(vol[, c("no", "order", "year", "url")])
}

# =============================================================================
# JSS.Art()
# Extrae los artículos de un volumen dado su URL.
# Reintenta hasta 5 veces ante errores de conexión con espera creciente.
# Retorna un data.frame con: title, issue y url de cada artículo.
# =============================================================================
JSS.Art <- function(url, max.tries = 5){
  
  for (attempt in 1:max.tries){
    
    result <- tryCatch({
      
      page <- read_html(url)
      
      data.frame(
        title = page |> html_elements('.media-heading a') |> html_text2() |> str_trim(),
        issue = page |> html_elements('.col-sm-3')        |> html_text2() |>
          str_trim() |>
          (\(x) sub(".*,\\s*", "", x))() |>  # Conservar solo lo que sigue a la última coma
          str_remove('Issue '),
        url   = page |> html_elements('.media-heading a') |> html_attr('href')
      )
      
    }, error = \(e){
      message(sprintf('\nIntento %d/%d fallido para: %s', attempt, max.tries, url))
      Sys.sleep(attempt * 2)
      NULL
    })
    
    if (!is.null(result)) return(result)
  }
  
  message(sprintf('\nNo se pudo recuperar: %s', url))
  data.frame(title = NA, issue = NA, url = NA)
}


# =============================================================================
# JSS.Art.Info()
# Extrae la metadata de un artículo dado su URL.
# Reintenta hasta 5 veces ante errores de conexión con espera creciente.
# Retorna un data.frame con: url, abstract, date y DOI.
# =============================================================================
JSS.Art.Info <- function(url, max.tries = 5){
  
  for (attempt in 1:max.tries){
    
    result <- tryCatch({
      
      article <- read_html(url)
      meta    <- article |> html_elements(".col-sm-8") |> html_text2()
      
      data.frame(
        url      = url,
        abstract = article |> html_element('.article-abstract') |> html_text2(),
        date     = meta[[2]],
        DOI      = meta[[3]]
      )
      
    }, error = \(e){
      message(sprintf('\nIntento %d/%d fallido para: %s', attempt, max.tries, url))
      Sys.sleep(attempt * 2)  # Espera creciente: 2s, 4s, 6s, 8s, 10s
      NULL
    })
    
    if (!is.null(result)) return(result)
  }
  
  # Si los 5 intentos fallan, retorna NAs
  message(sprintf('\nNo se pudo recuperar: %s', url))
  data.frame(url = url, abstract = NA, date = NA, DOI = NA)
}