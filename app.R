# =============================================================================
# WASKITA Professional Dashboard - Main Application
# Launch script for the professional statistical analysis platform
# =============================================================================

# Load the application components
source("global.R")
source("ui.R") 
source("server.R")

# Launch the Shiny application
shinyApp(ui = ui, server = server)
