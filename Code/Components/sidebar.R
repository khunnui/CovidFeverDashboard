################################################################################
# sidebar.R
# 
# Create the sidebar menu options for the ui.
################################################################################

library(shinydashboard)

sidebar <- dashboardSidebar(
  width = 160,
  tags$style(
    HTML(
      "
        .shiny-input-container > label {margin-bottom: -15px;}
        .sidebar-menu li a {padding-top: 5px; padding-bottom: 5px;}
      "
    )
  ),
  sidebarMenu(
    menuItem("Study sites", tabName = "site"),
    menuItem("Screening", tabName = "screen"),
    menuItem("Enrollment", tabName = "enrol"),
    menuItem("Diagnosis at OPD/ER", tabName = "diag"),
    menuItem("Underlying Condition", tabName = "underlying"),
    menuItem("Risk Factors", tabName = "risk"),
    menuItem("Signs and Symptoms", tabName = "sign"),
    menuItem("Vaccination", tabName = "vac"),
    menuItem("ATK Used", tabName = "atk"),
    menuItem("SARS-CoV-2 Detection", tabName = "detect"),
    menuItem("Serology Testing", tabName = "sero"),
    menuItem("KAP", tabName = "KAP")
  ),
  tags$div(id = "div_test", tags$style("#div_test div {margin-bottom: -2px}"),
           selectInput(
             inputId = "Province",
             label = "Province:",
             choices = c("All", unique(df_enr$Province))
           )),
  selectInput(
    inputId = "Hospital",
    label = "Hospital:",
    choices = c("All", as.character(unique(df_enr$S1HospitalID)))
  ),
  
  h6(textOutput("Date"), align = "center")
)
