################################################################################
# body.R
#
# Create the body for the ui.
################################################################################

library(shinydashboard)
library(plotly)
library(DT)

body <- dashboardBody(
  tags$head(tags$style(
    HTML("div.box-header {text-align: center;}
         .skin-black .main-sidebar {background-color: #49708B;}")
  )),
  tabItems(
    tabItem(tabName = "site",
            fluidRow(
              box(
                title = "Study Sites: 3 hospitals in 2 provinces",
                width = 12,
                plotOutput("map", height = 455)
              )
            )),
    tabItem(tabName = "screen",
            fluidRow(
              box(
                title = "Number Screened by Month",
                width = 12,
                align="center",
                plotlyOutput("ScreeningBar", height = 375),
                radioButtons("type", 
                             label = "",
                             inline = TRUE,
                             choices = list("Weekly" = 1, "Monthly" = 2), 
                             selected = 1)
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
                  plotlyOutput("ScreeningGender", height = 242)
                )
              ),
              column(width = 6,
                     box(
                       title = "Enrollment",
                       width = NULL,
                       plotlyOutput("ScreeningEnrol")
                     ))
            )),
    tabItem(
      tabName = "enrol",
      fluidRow(
        column(
          width = 10,
          box(
            title = "Eligible and Enrollment by Month",
            width = NULL,
            plotlyOutput("EnrollmentBar", height = 430)
          )
        ),
        column(
          width = 2,
          valueBoxOutput("eliBox", width = NULL),
          valueBoxOutput("enrolBox", width = NULL),
          valueBoxOutput("posBox", width = NULL),
          valueBoxOutput("pos3weekBox", width = NULL)
        )
      ),
      fluidRow(
        box(
          title = "Age Group",
          width = 4,
          plotlyOutput("EnrollmentAge", height = 270)
        ),
        box(
          title = "Gender",
          width = 4,
          plotlyOutput("EnrollmentGender", height = 270)
        ),
        box(
          title = "Occupation",
          width = 4,
          plotlyOutput("EnrollmentOcc", height = 270)
        ),
      ),
    ),
    tabItem(tabName = "diag",
            fluidRow(box(
              title = "Diagnosis at OPD/ER", 
              width = 12,
              plotlyOutput("Diag", height = 766)
            ))),
    tabItem(tabName = "underlying",
            fluidRow(box(
              title = "Underlying Condition Among Febrile Patients",
              width = 12,
              plotlyOutput("Underly", height = 475)
            ))),
    tabItem(tabName = "risk",
            fluidRow(box(
              title = "Risk Factors Within Past 14 Days among Febrile Patients  ",
              width =12,
              plotlyOutput("Risk", height = 475)
            ))),
    
    tabItem(tabName = "sign",
            fluidRow(
              column(
                width = 10,
                box(
                  title = "Clinical Signs and Symptoms among Febrile Patients",
                  width = NULL,
                  plotlyOutput("Sign", height = 612)
                )
              ),
              column(
                width = 2,
                valueBoxOutput("posBoxSign", width = NULL),
                valueBoxOutput("hospitalised", width = NULL),
                valueBoxOutput("intub", width = NULL),
                valueBoxOutput("deathBox", width = NULL)
              )
            )), 
    tabItem(tabName = "vac",
            fluidRow(
              box(
                title = "COVID-19 Vaccination",
                width = 4,
                plotlyOutput("VaccinePie1")
              ),
              box(
                title = "COVID-19 Vaccination vs. PCR Results",
                width = 8,
                plotlyOutput("VaccinePie2")
              )
            )),
    tabItem(tabName = "atk",
            fluidRow(box(
              title = "Antigen Test Kits (ATK) Used within Past 30 Days among Febrile Patients",
              width = 8,
              plotlyOutput("atkPie")
            ))),
    tabItem(tabName = "detect",
            fluidRow(box(
              title = "Box title", "Box content"
            ))),
    tabItem(tabName = "sero",
            fluidRow(box(
              title = "Box title", "Box content"
            ))),
    tabItem(tabName = "KAP",
            fluidRow(
              box(
                title = "Knowledge, Attitude and Practices",
                width = 12,
                div(
                  style = "text-align: center; font-size: 20px",
                  textOutput("titletext")
                ),
                plotlyOutput("kap1"),
                plotlyOutput("kap2", height = 267)
              )
            ))
  )
)
