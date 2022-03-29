################################################################################
# server.R
################################################################################

library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)
library(cowplot)

server <- function(input, output, session) {

  tt <- reactive({
    if (input$Hospital != "All") {
      tt <- paste0(input$Hospital, " Hospital")
    } else if (input$Province != "All") {
      tt <- paste0(input$Province, " Province")
    } else {
      tt <- ""
    }
  })
  
  shinyjs::html("dateHeader", paste0(" Data as of ", ddate))
      
  
  output$titletext <- renderText({
    if (input$Hospital != "All") {
      titletext <- paste0(input$Hospital, " Hospital")
    } else if (input$Province != "All") {
      titletext <- paste0(input$Province, " Province")
    } else {
      titletext <- ""
    }
  })

  observeEvent(input$Province, {
    if (input$Province == "All") {
      updateSelectInput(
        inputId = "Hospital",
        choices = c("All", as.character(unique(df_scrgender$S1HospitalID)))
      )
    } else {
      updateSelectInput(
        inputId = "Hospital",
        choices = c("All", as.character(unique(df_scrgender$S1HospitalID[df_scrgender$Province == input$Province])))
      )
    }
  })

  output$map <- renderPlot({
    ggdraw() +
      draw_plot(map_cf_tak,      x = 0.1,  y = 0.2, width = 0.25, height = 0.6) +
      draw_plot(map_cf_province, x = 0.25, y = 0,    width = 0.5,  height = 1) +
      draw_plot(map_cf_np,       x = 0.65, y = 0.2, width = 0.25, height = 0.6)
  })

  output$ScreeningBar <- renderPlotly({
    if (input$screenx == 1) {
      df_scr <- df_scrw
      l = list(
        title = '',
        dtick = 1209600000,
        tick0 = "2021-06-07",
        tickformat = "%b %d, %y",
        tickangle = -45,
        tickfont = list(size = 10)
      )
    } else {
      df_scr <- df_scrm
      l = list(title = '',
               dtick = "M1",
               tickformat = "%b %y")
    }
    if (input$Hospital != "All") {
      df <- df_scr %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_scr %>% filter(Province == input$Province)
    } else {
      df <- df_scr
    }
    p <- plot_ly(
      data = df %>%
        group_by(scrdate) %>%
        summarise(count = sum(n)),
      x = ~ scrdate,
      y = ~ count,
      name = "Screening",
      type = "bar",
      marker = list(color = 'rgb(158,202,225)'),
      hoverinfo = 'y'
    ) %>%
      layout(
        title = paste0(
          tt(),
          "<br><sup>Total screening = ",
          format(sum(df$n), big.mark = ","),
          "</sup>"
        ),
        xaxis = l,
        yaxis = list(title = 'Number Screened',
                     tickformat = ','),
        bargap = 0.5,
        margin = list(l = 80, r = 80)
      )
  })
  
  output$ScreeningAge <- renderDT({
    if (input$Hospital != "All") {
      df <- df_scrage2 %>% filter(S1HospitalID == input$Hospital) %>% 
        select(-S1HospitalID)
    } else if (input$Province != "All") {
      df <- df_scrage1 %>% filter(Province == input$Province) %>% 
        select(-Province)
    } else {
      df <- df_scrage0
    }
    datatable(
      df,
      caption = htmltools::tags$caption(style = "caption-side: top; text-align: center; color: black; font-size: 20px;",
                                        tt()),
      rownames = FALSE,
      options = list(
        initComplete = JS("function(){$(this).addClass('compact');}"),
        dom = 'rt'
      )
    ) %>%
      formatCurrency(1, '') %>%
      formatRound(1, 0)
  })
  
  output$ScreeningGender <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_scrgender %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_scrgender %>% filter(Province == input$Province)
    } else {
      df <- df_scrgender
    }
    pie(df, S1Gender, tt())
  })
  
  output$ScreeningEnrol <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_screnrol %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_screnrol %>% filter(Province == input$Province)
    } else {
      df <- df_screnrol
    }
    plot_ly(
      data = df %>%
        group_by(OLDCF, CF_Enrol) %>%
        summarise(count = sum(n)),
      x = ~ OLDCF,
      y = ~ count,
      color = ~ CF_Enrol,
      type = "bar",
      hoverinfo = 'y',
      hovertemplate = '%{y:,}<extra></extra>'
    ) %>%
      layout(
        title = tt(),
        xaxis = list(title = 'Previously Enrolled'),
        yaxis = list(title = 'Count',
                     tickformat = ','),
        bargap = 0.5,
        legend = list(title=list(text='Enrolled'),
                      x = 100, 
                      y = 0.5)
      )
  })

  output$EnrollmentBar <- renderPlotly({
    if (input$enrollx == 1) {
      df_eli <- df_eliw
      df_enr <- df_enrw
      l = list(
        title = '',
        dtick = 1209600000,
        tick0 = "2021-06-07",
        tickformat = "%b %d, %y",
        tickangle = -45,
        tickfont = list(size = 10)
      )
    } else {
      df_eli <- df_elim
      df_enr <- df_enrm
      l = list(title = '',
               dtick = "M1",
               tickformat = "%b %y"
      )
    }
    if (input$Hospital != "All") {
      df1 <- df_eli %>% filter(S1HospitalID == input$Hospital)
      df2 <- df_enr %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df1 <- df_eli %>% filter(Province == input$Province)
      df2 <- df_enr %>% filter(Province == input$Province)
    } else {
      df1 <- df_eli
      df2 <- df_enr
    }
    plot_ly() %>%
      add_trace(
        data = df1 %>%
          group_by(scrdate) %>%
          summarise(count = sum(n)),
        x = ~ scrdate,
        y = ~ count,
        name = "Eligible",
        type = 'bar',
        marker = list(color = '#39CCCC'),
        hoverinfo = 'y'
      ) %>%
      add_trace(
        data = df2 %>%
          group_by(enrdate) %>%
          summarise(count = sum(n)),
        x = ~ enrdate,
        y = ~ count,
        name = "Enroll",
        type = 'bar',
        marker = list(color = '#00C0EF'),
        hoverinfo = 'y'
      ) %>%
      add_trace(
        data = df2 %>%
          group_by(enrdate) %>%
          summarise(inc = round(sum(n[FinalResult == 'Positive'], na.rm = TRUE) / sum(n), 2)),
        x = ~ enrdate,
        y = ~ inc,
        yaxis = "y2",
        name = "COVID-19 Incidence",
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#F39C12'),
        hoverinfo = 'y'
      ) %>%
      layout(
        title = tt(),
        yaxis2 = list(
          overlaying = "y",
          side = "right",
          rangemode = 'tozero',
          title = 'Incidence',
          range = list(0, 1),
          tickformat = '.0%'
        ),
        xaxis = l,
        yaxis = list(title = 'Number Eligible/Enrolled'),
        bargap = 0.5,
        bargroupgap = 0.1,
        margin = list(l = 80, r = 80),
        legend = list(
          orientation = "h",
          # show entries horizontally
          xanchor = "center",
          # use center of legend as anchor
          x = 0.5,
          y = 0.95
        )
      )             
  })
  
  output$eliBox <- renderValueBox({
    if (input$Hospital != "All") {
      df <- df_elim %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_elim %>% filter(Province == input$Province)
    } else {
      df <- df_elim
    }
    valueBox(
      format(sum(df$n), big.mark= ","), 
      "Eligible",
      color = "teal"
    )
  })
  
  output$enrolBox <- renderValueBox({
    if (input$Hospital != "All") {
      df <- df_enrm %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_enrm %>% filter(Province == input$Province)
    } else {
      df <- df_enrm
    }
    valueBox(
      format(sum(df$n), big.mark= ","), 
      "Enrolled",
      color = "aqua"
    )
  })
  
  output$posBox <- renderValueBox({
    if (input$Hospital != "All") {
      df <- df_enrm %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_enrm %>% filter(Province == input$Province)
    } else {
      df <- df_enrm
    }
    df <- filter(df, FinalResult == "Positive")
    valueBox(
      format(sum(df$n), big.mark= ","),
      "SARS-CoV-2 Positive",
      color = "yellow"
    )
  })
  
  output$pos3weekBox <- renderValueBox({
    if (input$Hospital != "All") {
      df <- df_pos3wk %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_pos3wk %>% filter(Province == input$Province)
    } else {
      df <- df_pos3wk
    }
    df <- filter(df, FinalResult == "Positive")
    valueBox(
      format(sum(df$n), big.mark= ","), 
      "Cases detected in last 3 weeks",
      color = "purple"
    )
  })
  
  output$EnrollmentAge <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_enrage %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_enrage %>% filter(Province == input$Province)
    } else {
      df <- df_enrage
    }
    pie(df, agegroup, tt())
  })
  
  output$EnrollmentGender <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_enrgender %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_enrgender %>% filter(Province == input$Province)
    } else {
      df <- df_enrgender
    }
    pie(df, S1Gender, tt())
  })
  
  output$EnrollmentOcc <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_enrocc %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_enrocc %>% filter(Province == input$Province)
    } else {
      df <- df_enrocc
    }
    df %>%
      group_by(S34Occupation) %>% # Group by specified column
      summarise(count = sum(n)) %>% 
      mutate(rank = rank(-replace(count,S34Occupation=='Other',NA), ties.method = "first")) %>% 
      mutate(S34Occupation = ifelse(rank <= 5, levels(S34Occupation)[S34Occupation], "Other")) %>%
      rename(n = count) %>%      
      pie(S34Occupation, tt(), TRUE)
  })

  output$Diag <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_dx %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_dx %>% filter(Province == input$Province)
    } else {
      df <- df_dx
    }
    hbar(df, Diagnosis, tt())
  })  

  output$Underly <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_un %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_un %>% filter(Province == input$Province)
    } else {
      df <- df_un
    }
    hbar(df, Underlying, tt())
  })  
  
  output$Risk <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_rf %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_rf %>% filter(Province == input$Province)
    } else {
      df <- df_rf
    }
    hbar(df, Risk, tt())
  })
  
  output$Sign <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_sign %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_sign %>% filter(Province == input$Province)
    } else {
      df <- df_sign
    }
    hbar(df, Signs, tt())
  })

  output$posBoxSign <- renderValueBox({
    if (input$Hospital != "All") {
      df <- df_enrm %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_enrm %>% filter(Province == input$Province)
    } else {
      df <- df_enrm
    }
    df <- filter(df, FinalResult == "Positive")
    valueBox(
      tags$p(format(sum(df$n), big.mark= ","), style = "font-size: 75%;"),
      subtitle = " SARS-CoV-2 Positive",
      color = "yellow"
    )
  })

  output$hospitalised <- renderValueBox({
    if (input$Hospital != "All") {
      df <- df_signBox %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_signBox %>% filter(Province == input$Province)
    } else {
      df <- df_signBox
    }
    valueBox(
      tags$p(format(sum(df$n), big.mark= ","), style = "font-size: 75%;"),
      "Hospitalized",
      color = "teal"
    )
  })
  
  output$intub <- renderValueBox({
    if (input$Hospital != "All") {
      df <- df_signBox %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_signBox %>% filter(Province == input$Province)
    } else {
      df <- df_signBox
    }
    df <- filter(df, S5Intub == 2)
    valueBox(
      tags$p(format(sum(df$n), big.mark= ","), style = "font-size: 75%;"),
      "Intubation",
      color = "purple"
    )
  })
  
  output$death <- renderValueBox({
    if (input$Hospital != "All") {
      df <- df_signBox %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_signBox %>% filter(Province == input$Province)
    } else {
      df <- df_signBox
    }
    df <- filter(df, S5DishargeType == 4)
    valueBox(
      tags$p(format(sum(df$n), big.mark= ","), style = "font-size: 75%;"),
      "Death",
      color = "maroon"
    )
  })
  
  output$VaccinePie1 <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_vac %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_vac %>% filter(Province == input$Province)
    } else {
      df <- df_vac
    }
    pie(df, S33CovidVaccine, tt())
  }) 
  
  output$VaccinePie2 <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_vac %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_vac %>% filter(Province == input$Province)
    } else {
      df <- df_vac
    }
    df <- df %>%
      group_by(S33CovidVaccine, FinalResult) %>%
      summarise(count = sum(n))
    df1 <- filter(df, S33CovidVaccine == "Vaccinated")
    df2 <- filter(df, S33CovidVaccine == "Unvaccinated")
    plot_ly(labels = ~ FinalResult,
            values = ~ count,
            sort = FALSE,
            marker = list(
              colors = c("#F39C12", "#FBDEB0", "#ECF0F5"),
              line = list(color = '#FFFFFF', width = 1)
            ),
            texttemplate = "%{percent:.1%}",
            hovertemplate = '%{value:,}<extra></extra>'
    ) %>%
      add_pie(
        data = df1,
        name = "Vaccinated",
        scalegroup = 'one',
        direction ='clockwise', 
        domain = list(row = 0, column = 0)
      ) %>%
      add_pie(
        data = df2,
        name = "Unvaccinated",
        scalegroup = 'one',
        direction ='clockwise', 
        domain = list(row = 0, column = 1)
      ) %>%
      layout(
        title = tt(),
        grid = list(rows = 1, columns = 2),
        margin = list(l = 160, r = 160),
        legend = list(
          orientation = "h",
          # show entries horizontally
          xanchor = "center",
          # use center of legend as anchor
          x = 0.5
        ),
        annotations = list(
          list(
            x = 0.12,
            y = 1,
            text = "Vaccinated",
            showarrow = F,
            xref = 'paper',
            yref = 'paper'
          ),
          list(
            x = 0.88,
            y = 1,
            text = "Unvaccinated",
            showarrow = F,
            xref = 'paper',
            yref = 'paper'
          )
        )
      )
  })
  
  output$atkPie <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_atk %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_atk %>% filter(Province == input$Province)
    } else {
      df <- df_atk
    }
    pie(df, FinalResult, tt())
  }) 
  
  output$DetectBar <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_lab %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_lab %>% filter(Province == input$Province)
    } else {
      df <- df_lab
    }
    plot_ly(
      df %>%
        group_by(SpecType) %>%
        summarise(total = sum(n),
                  positive = sum(n[FinalResult == 'Positive'])),
      x = ~ SpecType,
      y = ~ total,
      type = "bar",
      name = 'Tested',
      hoverinfo = 'y',
      hovertemplate = '%{y:,}<extra></extra>'
    ) %>% 
      add_trace(y = ~ positive, 
                name = 'PCR Positive',
                hoverinfo = 'y',
                hovertemplate = '%{y:,}<extra></extra>') %>% 
      layout(
        title = tt(),
        xaxis = list(title = ''),
        yaxis = list(title = 'Count',
                     tickformat = ','),
        bargap = 0.5,
        legend = list(
          orientation = "h",
          # show entries horizontally
          xanchor = "center",
          # use center of legend as anchor
          x = 0.5
        )
      )
  })
  
  output$DetectPie <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_labpos %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_labpos %>% filter(Province == input$Province)
    } else {
      df <- df_labpos
    }
    pie(df, specimens, tt(), TRUE)
  })  
    
  output$kap1 <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_kap1 %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_kap1 %>% filter(Province == input$Province)
    } else {
      df <- df_kap1
    }
    df %>%
      mutate(
        kap = recode(
          kap,
          'S3604SickSpread' = 'Only people who are sick and who shows symptoms can\nspread the disease',
          'S3615CareLate'   = 'I sought care today later than I usual\nbecause of COVID-19',
          'S3616'           = 'I was afraid of being placed under quarantine after\nclose contact with COVID-19 patient',
          'S3617'           = 'I was afraid to seek care today or previously out of\nfear of being tested for COVID-19/isolated in hospital',
          'S3618'           = 'Always wearing mask in public is a good thing to do',
          'S3619'           = 'Always practicing social distancing from other people\nis a good thing to do',
          'S3620'           = 'Patients should disclose their exposure to COVID-19\nand their symptoms'
        )
      ) %>%
      scalebar(kap, c('#93C2A4', '#C8EABA', '#FFFEDF', '#FFCB81', '#EE9134'))
  })
  
  output$kap2 <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_kap2 %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_kap2 %>% filter(Province == input$Province)
    } else {
      df <- df_kap2
    }
    df %>%
      mutate(
        kap = recode(
          kap,
          'S3610MaskIn'     = 'During the past 2 weeks, did you wear a mask at home?',
          'S3613MaskOut'    = 'Did you wear a mask when you went outside of your\nresidence in crowded areas?',
          'S3621'           = 'Do you practice social distancing from other persons\nin your household?',
          'S3622'           = 'Do you practice social distancing from other persons\noutside of your residence?'
        )
      ) %>%
      scalebar(kap, c('#66A38F', '#86C499', '#ADDFAA', '#D7F3C1', '#FFFEDF'))
  })
  
}
