################################################################################
# body.R
#
# Create the body for the ui.
################################################################################

library(shinydashboard)
library(plotly)
library(DT)

body <- dashboardBody(
  tags$head(tags$style(HTML("div.box-header {text-align: center;}"))),
  tabItems(
    tabItem(tabName = "site",
            fluidRow(
              box(title = "Study Sites: 3 hospitals in 2 provinces",
                  width = 12,
                  plotOutput("map", height = 455))
            )),
    tabItem(tabName = "screen",
            fluidRow(
              box(
                title = "Number Screened by Month",
                width = 12,
                plotlyOutput("ScreeningBar", height = 475)
              )
            ),
            fluidRow(
              box(
                title = "Gender",
                # width = 6,
                plotOutput("ScreeningGender", height = 204)
              ),
              box(
                title = "Age Summary statistics",
                # width = 6,
                dataTableOutput("ScreeningAge")
              )
            )),
    tabItem(tabName = "enrol",
            fluidRow(
              box(title = "Box title", "Box content")
            )),
    tabItem(tabName = "diag",
            h2("Widgets tab content")),
    tabItem(tabName = "underlying",
            h2("Widgets tab content")),
    tabItem(tabName = "risk",
            h2("Widgets tab content")),
    tabItem(tabName = "sign",
            h2("Widgets tab content")),
    tabItem(tabName = "vac",
            h2("Widgets tab content")),
    tabItem(tabName = "atk",
            h2("Widgets tab content")),
    tabItem(tabName = "detect",
            h2("Widgets tab content")),
    tabItem(tabName = "sero",
            h2("Widgets tab content"))
  )
)
