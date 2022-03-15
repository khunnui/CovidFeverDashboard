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
            plotOutput("map", height = 455)
        )
      )
    ),
    tabItem(tabName = "screen",
      fluidRow(
        box(
          title = "Number Screened by Month",
          width = 12,
          plotlyOutput("ScreeningBar", height = 475)
        )
      ),
      fluidRow(
        column(
          width = 6,
          box(
            title = "Age",
            width = NULL,
            dataTableOutput("ScreeningAge")
          ),
          box(
           title = "Gender",
           width = NULL,
           plotOutput("ScreeningGender", height = 242)
          )
        ),
        column(
          width = 6,
          box(
            title = "Enrollment", 
            width = NULL,
            plotOutput("ScreeningEnrol")
          )
        )
      )
    ),
    tabItem(tabName = "enrol",
      fluidRow(
        box(
          title = "Box title", "Box content"
        )
      )
    ),
    tabItem(tabName = "diag",
            fluidRow(
              box(
                title = "Box title", "Box content"
              )
            )
    ),
    tabItem(tabName = "underlying",
            fluidRow(
              box(
                title = "Box title", "Box content"
              )
            )
    ),
    tabItem(tabName = "risk",
            fluidRow(
              box(
                title = "Box title", "Box content"
              )
            )
    ),
    tabItem(tabName = "sign",
            fluidRow(
              box(
                title = "Box title", "Box content"
              )
            )
    ),
    tabItem(tabName = "vac",
            fluidRow(
              box(
                title = "COVID-19 Vaccination", 
                width =4,
                plotOutput("VaccinePie", height = 475)
              ),
              box(
                title = "COVID-19 Vaccination vs. PCR Results", 
                width =8,
                plotOutput("VaccinePie", height = 475)
              )
            )
    ),
    tabItem(tabName = "atk",
            fluidRow(
              box(
                title = "Box title", "Box content"
              )
            )
    ),
    tabItem(tabName = "detect",
            fluidRow(
              box(
                title = "Box title", "Box content"
              )
            )
    ),
    tabItem(tabName = "sero",
            fluidRow(
              box(
                title = "Box title", "Box content"
              )
            )
    )
  )
)
