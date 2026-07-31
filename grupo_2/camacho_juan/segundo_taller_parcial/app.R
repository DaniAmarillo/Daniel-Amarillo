# ==================================================
# Taller 2 - Minería de Datos
# Dashboard de artículos científicos
# App
# ==================================================

setwd("C:/Users/juana/Downloads")
source("data_manager.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
