################################################################################
# ui.R
# 
# Initializes the ui. 
# Load in header, sidebar, and body components.
################################################################################

source('./Components/header.R')
source('./Components/sidebar.R')
source('./Components/body.R')

ui <- dashboardPage(
  header = header,
  sidebar =  sidebar,
  body = body)
