

source("libraries.R")
source("FUN.R")

source("Dashboard/ui.R")
source('Dashboard/server.R')


shinyApp(
  ui = myUI,
  server = myserver
)