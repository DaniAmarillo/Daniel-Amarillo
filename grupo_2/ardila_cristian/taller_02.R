#-------------------------------------
#            MÉTODO 1
#-------------------------------------


library(rvest)
library(dplyr)
library(httr)
library(stringr)
library(purrr)
library(lubridate)

meses <- setNames(1:12, c("january","february","march","april","may","june",
                          "july","august","september","october","november","december"))

extraer_mes <- function(num, nombre, year = 2025) {
  
  url <- paste0("https://www.accuweather.com/es/co/bogota/107487/",
                nombre, "-weather/107487?year=", year)
  
  html <- GET(url, add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"
  )) %>% read_html()
  paneles <- html %>% html_nodes(".monthly-daypanel")
  
  tibble(
    clase = paneles %>% html_attr("class"),
    dia   = paneles %>% html_node(".date") %>% html_text(trim = TRUE),
    high  = paneles %>% html_node(".high") %>% html_text(trim = TRUE),
    low   = paneles %>% html_node(".low") %>% html_text(trim = TRUE)
  ) %>%
    filter(!str_detect(clase, "outside")) %>%
    transmute(
      fecha = make_date(year, num, as.integer(str_extract(dia, "\\d+"))),
      high  = as.numeric(str_extract(high, "\\d+")),
      low   = as.numeric(str_extract(low, "\\d+"))
    ) %>%
    filter(!is.na(fecha))
}

imap_dfr(meses, extraer_mes) %>%
  arrange(fecha) %>%
  slice_head(n = 10) %>%
  knitr::kable()

#-------------------------------------
#            MÉTODO 2
#-------------------------------------

library(rvest)
library(dplyr)
library(httr)
library(stringr)

url <- "https://www.accuweather.com/es/co/bogota/107487/january-weather/107487?year=2025"

respuesta <- GET(url, add_headers(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"
))

html <- read_html(respuesta)

paneles <- html %>% html_nodes(".monthly-daypanel")

tibble(
  clase = paneles %>% html_attr("class"),
  dia   = paneles %>% html_node(".date") %>% html_text(trim = TRUE),
  high  = paneles %>% html_node(".high") %>% html_text(trim = TRUE),
  low   = paneles %>% html_node(".low") %>% html_text(trim = TRUE)
) %>%
  filter(!str_detect(clase, "outside")) %>%
  transmute(
    fecha = as.Date(paste0("2025-01-", str_pad(dia, 2, pad = "0"))),
    high  = as.numeric(str_extract(high, "\\d+")),
    low   = as.numeric(str_extract(low, "\\d+"))
  ) %>%
  arrange(fecha) %>%
  slice_head(n = 10) %>%
  knitr::kable()

