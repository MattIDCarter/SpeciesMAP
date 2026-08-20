# ============================================================
# Shiny application
# ============================================================

source("config/config.R")

source("R/mapdata.R")
source("R/maps.R")
source("R/ui.R")
source("R/server.R")

shinyApp(
  ui = app_ui(),
  server = app_server
)