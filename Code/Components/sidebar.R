################################################################################
# sidebar.R
# 
# Create the sidebar menu options for the ui.
################################################################################

library(shinydashboard)

sidebar <- dashboardSidebar(
  width = 180,
  sidebarMenu(
    menuItem("Study sites", tabName = "site"),
    menuItem("Screening", tabName = "screen"),
    menuItem("Enrollment", tabName = "enrol")
  ),
  selectInput(
    inputId = "Province",
    label = "Province:",
    choices = c("All", unique(tblSection1$Province))
  ),
  selectInput(
    inputId = "Hospital",
    label = "Hospital:",
    choices = c("All", as.character(unique(tblSection1$S1HospitalID)))
  )
)
