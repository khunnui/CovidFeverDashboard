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
            h3("Study Sites: 3 hospitals in each province"),
            plotOutput("map")
    ),
    tabItem(tabName = "screen",
            h2("Widgets tab content")
    ),
    tabItem(tabName = "enrol",
            h2("Widgets tab content")
    )
  )
)
