################################################################################
# CovidFever Shiny Dashboard App
# hpy1 - 2/28/2022
#
# app.R - main controller
# Import ui and server components; initializes the app.
################################################################################

library(shiny)

source('./ui.R')
source('./server.R')

shinyApp(ui, server)
