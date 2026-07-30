required_packages <- c(
  "shiny",
  "DBI",
  "RSQLite",
  "dplyr",
  "stringr",
  "highcharter",
  "DT",
  "httr2",
  "rvest",
  "jsonlite",
  "tidyr",
  "htmltools",
  "rmarkdown"
)

missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
} else {
  message("Todas las dependencias requeridas ya estan instaladas.")
}
