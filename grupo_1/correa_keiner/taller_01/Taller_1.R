library(rvest)
library(tidyverse)
library(lubridate)
library(httr)

extraer_temperaturas <- function(anio, mes_nombre) {
  url <- paste0("https://www.accuweather.com/es/co/bogota/107487/", 
                mes_nombre, "-weather/107487?year=", anio)
  
  print(paste("Extrayendo:", mes_nombre, anio))
  
  respuesta <- GET(url, add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/119.0.0.0 Safari/537.36"
  ))
  
  if (status_code(respuesta) != 200) {
    message(paste("Error en", mes_nombre, "-", status_code(respuesta)))
    return(NULL)
  }
  
  pagina <- read_html(respuesta)
  dias <- pagina %>% html_nodes(".monthly-daypanel")
  
  # Mapeo de nombre de mes a número
  mes_num <- match(mes_nombre, c("january", "february", "march", "april", "may", "june",
                                 "july", "august", "september", "october", "november", "december"))
  
  # Cuántos días tiene el mes real
  dias_en_mes <- days_in_month(make_date(anio, mes_num, 1))
  
  datos_mes <- map_df(dias, function(dia) {
    dia_num <- dia %>% html_node(".date") %>% html_text(trim = TRUE) %>% as.integer()
    high    <- dia %>% html_node(".high") %>% html_text(trim = TRUE) %>% parse_number()
    low     <- dia %>% html_node(".low")  %>% html_text(trim = TRUE) %>% parse_number()
    
    # Filtrar: solo días válidos del mes (descarta días de meses adyacentes)
    if (!is.na(high) && !is.na(dia_num) && dia_num >= 1 && dia_num <= dias_en_mes) {
      tibble(
        fecha = make_date(anio, mes_num, dia_num),
        high  = high,
        low   = low
      )
    }
  })
  
  Sys.sleep(2)
  return(datos_mes)
}

meses <- c("january", "february", "march", "april", "may", "june", 
           "july", "august", "september", "october", "november", "december")

historico_2025 <- map_df(meses, ~extraer_temperaturas(2025, .x))
historico_2026 <- map_df(meses, ~extraer_temperaturas(2026, .x))

resultado <- bind_rows(historico_2025, historico_2026) %>% arrange(fecha)
print(resultado)
