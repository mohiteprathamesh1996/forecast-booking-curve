# Load global dependencies
source("global.R")

# Load UI and server components
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)
