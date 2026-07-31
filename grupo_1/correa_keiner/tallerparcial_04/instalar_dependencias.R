

paquetes <- c(
  "shiny", "DBI", "RSQLite", "dplyr", "tidyr", "stringr", "lubridate",
  "highcharter", "DT", "httr", "rvest", "purrr", "tibble", "rsconnect",
  "tm", "SnowballC", "Matrix", "htmltools", "irlba", "slam", "knitr"
)

faltantes <- setdiff(paquetes, rownames(installed.packages()))

if (length(faltantes) > 0) {
  install.packages(faltantes)
} else {
  message("Todos los paquetes ya están instalados.")
}


