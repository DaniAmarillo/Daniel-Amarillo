# ============================================================
# Ejecutar Taller 10 Streamlit desde RStudio
# Selección manual
# ============================================================

# Cear directorio streamlit

dir.create(".streamlit", showWarnings = FALSE)

writeLines(
  'password = "analista123"',
  ".streamlit/secrets.toml"
)

library(reticulate)
library(rstudioapi)

python <- py_config()$python

app <- "Minería de Datos Taller Clase 10.py"

if (!file.exists(app)) {
  stop("No se encontró el archivo: ", app)
}

python <- normalizePath(python, winslash = "/", mustWork = TRUE)
app <- normalizePath(app, winslash = "/", mustWork = TRUE)

comando <- paste(
  shQuote(python),
  "-m streamlit run",
  shQuote(app),
  "--browser.gatherUsageStats=false",
  "--server.headless=false"
)

terminal_id <- rstudioapi::terminalExecute(
  command = comando,
  workingDir = dirname(app),
  show = TRUE
)

rstudioapi::terminalActivate(terminal_id)
