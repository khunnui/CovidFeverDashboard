################################################################################
# body.R
#
# Create the body for the ui.
################################################################################

library(shinydashboard)
library(plotly)
library(DT)
library(gt)
library(shinyjs)

body <- dashboardBody(
  tags$head(
    tags$style(
      HTML("div.box-header {text-align: center;}
            .skin-black .main-sidebar {background-color: #0B2C4B;}
            .myClass {font-size: 14px; text-align: right; margin-top: 17px; margin-right: 20px;}
            .small-box.bg-blue   { background-color: #a1caf1 !important; color: #000000 !important}
            .small-box.bg-green  { background-color: #ace1af !important; color: #000000 !important}
            .small-box.bg-orange { background-color: #B78F62 !important; color: #000000 !important}
            .small-box.bg-yellow { background-color: #f8de7e !important; color: #000000 !important}"
      )
    )
  ),
  tags$script(
    HTML('$(document).ready(function() 
      {$("header").find("nav").append(\'<div id="dateHeader" class="myClass"></div>\');})'
    )
  ),
  tabItems(
    tabItem(
      tabName = "overview",
      fluidRow(
        # style = "margin-top: -5px; margin-bottom: -20px; margin-left: -20px; margin-right: -20px;",
        box(
          title = HTML("Surveillance of COVID-19 in Patients Presenting with Febrile Illness<br>in Nakhon Phanom and Tak Provinces, Thailand (COVID Fever)"),
          width = 12,
          div(
            style = "text-align: center; font-size: 20px",
            h4("Objectives")
          ),
          HTML("<ul>
                  <li>Detect and describe the proportion, clinical characteristics, risk factors of COVID-19 among febrile patients with/without respiratory symptoms.</li>
                  <li>Assess knowledge, attitudes, and practices (KAP) among febrile patients to better understand health seeking behavior.</li>
                  <li>Determine the utility of saliva and anterior nasal swabs as suitable specimens compared to NP/OP swab for the detection of SARS-CoV-2 RNA.</li>
                  <li>Determine if a pooled PCR testing strategy is cost-effective.  </li>
                  <li>Assess temporal patterns of antibody response, and outcomes among COVID-19 cases.</li>
                  <li>Determine the seroprevalence of antibodies against SARS-CoV-2 in febrile patients. </li>
                  <li>Understand the etiology of co-infections with other respiratory pathogens. </li>
                  <li>Describe the proportion of COVID-19 among those with/without COVID-19 vaccination. </li>
                  <li>Describe the proportion of patients with post-COVID condition.</li>
                </ul>"
          )
        ),
        box(
          title = "Study Sites",
          width = 12,
          # plotOutput("map", height = 455)
          div(
            style = "text-align:center; display:block;",
            img(
              src = "StudySites.png" ,
              alt = "Study Sites",
              height = "455",
              width = "1033"
            )
          )
        ),
        box(
          title = "Methods",
          width = 12,
          div(
            style = "text-align: left; font-size: 20px; font-weight: bold",
            h5("Study population:")
          ),
          HTML("<ul>
                  <li>Prospective enroll febrile patients aged >2 years old, presenting to OPDs or ERs</li>
                  <li>Expected 8,500 cases within 2-3 years</li>
                </ul>"
          ),
          div(
            style = "text-align: left; font-size: 20px; font-weight: bold",
            h5("Inclusion criteria:")
          ),
          HTML("<ul>
                  <li>Fever >37.5 &deg;C or history of fever  <14 days AND </li>
                  <li>No clinical signs or symptoms of respiratory infection  (no cough, difficulty breathing or sputum production) 
                    <ul>
                      <li>Remove this criteria since May 2022</li>
                    </ul>
                  </li>
                </ul>"
          ),
          div(
            style = "text-align: left; font-size: 20px; font-weight: bold",
            h5("Exclusion criteria:")
          ),
          HTML("<ul> <li>Known COVID-19 infection within the past 30 days or</li>
                  <li>Fever >14 days or</li>
                  <li>Present to OPDs or ERs with trauma or accident</li>
                  <li>Not willing to participate or cannot communicate</li>
               </ul>"
          ),
          div(
            style = "text-align: left; font-size: 20px; font-weight: bold",
            h5("Data collection:")
          ),
          HTML("<ul> <li>Interview: clinical, risk factors, knowledge/attitude/practice (KAP), vaccination </li>
                  <li>Review medical record: clinical, laboratory, treatment, outcome</li>
              </ul>"
          ),
          div(
            style = "text-align: left; font-size: 20px; font-weight: bold",
            h5("Specimen collection and laboratory testing:")
          ),
          HTML(
            "<style>
              table {
              font-family: Arial;
              background-color: #F5F5F5;
              border: 1px solid #1C6EA4; border-collapse: collapse;
              margin-left:auto; margin-right:auto;
              }
              table td {
              border: 1px solid #AAAAAA;
              padding: 5px 5px;
              }
              </style>
              <table>
              <tbody>
              <tr>
              <td rowspan='2'><span>&nbsp;</span></td>
              <td colspan='4'><div style='text-align:center;'>Specimen Collection</div></td>
              </tr>
              <tr>
              <td><div style='text-align:center; width:80pt;'>NP+OP swab</div></td>
              <td><div style='text-align:center; width:80pt;'>Nasal Swab</div></td>
              <td><div style='text-align:center; width:80pt;'>Saliva</div></td>
              <td><div style='text-align:center; width:80pt;'>Blood</div></td>
              </tr>
              <tr>
              <td>All enrollees at enrollment</td>
              <td><div style='text-align:center; color: green'><span>&#10004;</span></div></td>
              <td><div style='text-align:center; color: green'><span>&#10004;</span></div></td>
              <td><div style='text-align:center; color: green'><span>&#10004;</span></div></td>
              <td><div style='text-align:center; color: red'><span>&#10008;</span></div></td>
              </tr>
              <tr>
              <td>5<sup>th</sup> enrollees</td>
              <td><div style='text-align:center; color: green'><span>&#10004;</span></div></td>
              <td><div style='text-align:center; color: green'><span>&#10004;</span></div></td>
              <td><div style='text-align:center; color: green'><span>&#10004;</span></div></td>
              <td><div style='text-align:center; color: green'><span>&#10004;</span></div></td>
              </tr>
              </tbody>
              </table>
              "
          ),
          div(
            style = "text-align:center; display:block;",
            img(
              src = "CollectMethod.jpg" ,
              alt = "Collect Method",
              height = "100",
              width = "600"
            )
          )
        ),
        box(
          title = "CRF",
          width = 12,
          div(
            style = "text-align:center; display:block;"
          ),
          uiOutput("pdfview")
        )
      )
    ),
    tabItem(
      # style = "margin-top: -5px; margin-bottom: -20px; margin-left: -5px; margin-right: -5px;",
      tabName = "screen",
      fluidRow(
        box(
          title = "Number Screened by Month",
          width = 12,
          align="center",
          plotlyOutput("ScreeningBar", height = 355),
          "*Included febrile patients with respiratory symptoms since May 17, 2022 for Tak and May 23, 2022 for Nakhon Phanom",
          radioButtons(
            "screenx",
            label = "",
            inline = TRUE,
            choices = list("Weekly" = 1, "Monthly" = 2), 
            selected = 2
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          box(
            title = "Age",
            width = NULL,
            dataTableOutput("ScreeningAge"),
            plotlyOutput("ScreeningAgeGroup", height = 325)
          )
        ),
        column(
          width = 6,
          box(
            title = "Gender",
            width = NULL,
            plotlyOutput("ScreeningGender")
          )
        )
      )
    ),
    tabItem(
      tabName = "enrol",
      fluidRow(
        column(
          width = 10,
          box(
            title = "Eligible, Enrollment and SARS-Cov-2 RT-PCR Positive Cases by Month",
            width = NULL,
            align="center",
            plotlyOutput("EnrollmentBar", height = 355),
            "*Included febrile patients with respiratory symptoms since May 17, 2022 for Tak and May 23, 2022 for Nakhon Phanom",
            radioButtons(
              "enrollx",
              label = "",
              inline = TRUE,
              choices = list("Weekly" = 1, "Monthly" = 2), 
              selected = 2
            )
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
        )
      )
    ),
    tabItem(
      tabName = "diag",
      fluidRow(
        box(
          title = "Diagnosis at OPD/ER",
          width = 12,
          gt_output("Diag")
        )
      )
    ),
    tabItem(
      tabName = "sign",
      fluidRow(
        # tags$head(tags$style(HTML('
        #     .small-box {height: 70px}
        #   '))),
        valueBoxOutput("posBoxSign", width = 3),
        valueBoxOutput("hospitalised", width = 3),
        valueBoxOutput("intub", width = 3),
        valueBoxOutput("death", width = 3)
      ),
      fluidRow(
        tabBox(
          id = "tabset_ss",
          width = 12,
          tabPanel("All patients",
                   h4("Clinical Signs and Symptoms among Febrile Patients", align = "center"),
                   gt_output("Sign")
          ),
          tabPanel("PCR+ patients",
                   h4("Clinical Signs and Symptoms among PCR + Febrile Patients", align = "center"),
                   gt_output("SignBF")
          )
        )
      )
    ),
    
    
    tabItem(
      tabName = "underlying",
      fluidRow(
        box(
          title = "Underlying Condition Among Febrile Patients",
          width = 12,
          gt_output("Underly")
        )
      )
    ),
    tabItem(
      tabName = "risk",
      fluidRow(
        box(
          title = "Risk Factors Within Past 14 Days among Febrile Patients  ",
          width = 12,
          gt_output("Risk")
        )
      )
    ),
    tabItem(
      tabName = "vac",
      fluidRow(
        box(
          title = "COVID-19 Vaccination and PCR Result",
          width = 6,
          plotlyOutput("VaccineSunburst"),
          height = 450
          ),
        box(
          title = "Time from COVID-19 Vaccination to Fever Onset by PCR status",
          width = 6,
          gt_output("vac2")
          , height = 450
        )
      ),
      fluidRow(
        box(
          title = "Fully Vaccinated",
          width = 4,
          plotlyOutput("vacfull", height = 270),
          "* Patients were considered fully vaccinated if they had completed 2 doses of Sinovac, Sinopharm, Pfizer-BioNTech, or Astrazeneca or 1 dose of Johnson and Johnson as a primary vaccination series at least 1 month prior to presentation"
          
        ),
        box(
          title = "Partially Vaccinated",
          width = 4,
          plotlyOutput("vacpart", height = 270)
        ),
        box(
          title = "Unvaccinated",
          width = 4,
          plotlyOutput("unvac", height = 270)
        )
      )
      
    ),
    
    tabItem(
      tabName = "atk",
      fluidRow(
        box(
          title = "Antigen Test Kits (ATK) Used within Past 30 Days among Febrile Patients",
          width = 12,
          plotlyOutput("atkPie")
        )
      )
    ),
    tabItem(
      tabName = "detect",
      fluidRow(
        box(
          title = "SARS-CoV-2 Detection by Sample Type",
          width = 6,
          plotlyOutput("DetectBar")
        ),
        box(
          title = "% PCR Positive by Sample Type",
          width = 6,
          plotlyOutput("DetectPie")
        )
      )
    ),
    tabItem(
      tabName = "lab",
      fluidRow( 
        h4("Laboratory characteristics and co-infections in febrile patients with and without SARS-CoV-2", align = "center"),
        tabBox(
          id = "tabset_lab",
          width = 12,
          tabPanel("CBC & Blood Chemistry",
                   gt_output("labcbc")
          ),
          tabPanel("Culture",
                   gt_output("labcul")
          )
        )
      )
    ),
    tabItem(
      tabName = "sero",
      fluidRow(
        box(
          title = "Seroprevalence Among Febrile Patients",
          width = 12,
          align = "center",
          gt_output("sero1a")
        )
      ),
      fluidRow(
        box(
          title = "Characteristics of Febrile Patients with Serology Positive",
          width = 12,
          align = "center",
          radioButtons(
            "serox",
            label = "",
            inline = TRUE,
            choices = list("All" = 1, "PCR+" = 2, "PCR-" =3), 
            selected = 1
          ),
          gt_output("sero1b")
        )
      ),
      fluidRow(
        box(
          title = "Serology Response Among COVID-19 Patients",
          width = 12,
          align = "center",
          gt_output("sero2a"),
        )
      ),
      fluidRow(
        box(
          title = "IgG-S* Among COVID-19 Patients, Compared Between Baseline and 4-6 Weeks Followup",
          width = 12,
          align = "center",
          gt_output("sero2b"),
          "*IgG-S = Anti spike protein IgG (AU/ml)."
        )
      )
    ),
    tabItem(
      tabName = "KAP",
      fluidRow(
        tabBox(
          id = "tabset_kap",
          width = 12,
          tabPanel("Knowledge and Attitude",
                   h4("Knowledge and Attitude", align = "center"),
                   plotlyOutput("kap1", height = 360),
                   h4("Comparison between delta variant (Jun-Dec 2021) and omicron variant pandemic (Jan 2022-May 2023)", align = "center"),
                   gt_output("kap3"),
                   plotlyOutput("kap1a", height = 600)
          ),
          tabPanel("Practices",
                   h4("Practices", align = "center"),
                   plotlyOutput("kap2", height = 232),
                   h4("Comparison between delta variant (Jun-Dec 2021) and omicron variant pandemic (Jan 2022-May 2023)", align = "center"),
                   gt_output("kap4"),
                   plotlyOutput("kap2a", height =500)
          )
        )
      )
    )
  ),
  useShinyjs()
)
