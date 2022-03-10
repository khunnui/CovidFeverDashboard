################################################################################
# body.R
#
# Create the body for the ui.
################################################################################

library(shinydashboard)
library(plotly)
library(DT)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "site",
            h3("Study Sites: 3 hospitals in 2 provinces", align = "center"),
            br(),
            plotOutput("map")
    ),
    tabItem(tabName = "screen",
            h3("Number Screened by Month", align = "center"),
            br(),
            plotlyOutput("ScreeningBar"),
            plotOutput("ScreeningAgePie")
    ),
    tabItem(tabName = "enrol",
            h2("Widgets tab content")
    ),
    tabItem(tabName = "diag",
            h2("Widgets tab content")
    ),
    tabItem(tabName = "underlying",
            h2("Widgets tab content")
    ),
    tabItem(tabName = "risk",
            h2("Widgets tab content")
    ),
    tabItem(tabName = "sign",
            h2("Widgets tab content")
    ),
    tabItem(tabName = "vac",
            h2("Widgets tab content")
    ),
    tabItem(tabName = "atk",
            h2("Widgets tab content")
    ),
    tabItem(tabName = "detect",
            h2("Widgets tab content")
    ),
    tabItem(tabName = "sero",
            h2("Widgets tab content")
    )
  )
)
