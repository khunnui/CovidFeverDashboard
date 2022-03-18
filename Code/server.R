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
        choices = c("All", as.character(unique(df_scr$S1HospitalID)))
      )
    } else {
      updateSelectInput(
        inputId = "Hospital",
        choices = c("All", as.character(unique(df_scr$S1HospitalID[df_scr$Province == input$Province])))
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
    if (input$Hospital != "All") {
      df <- df_scr %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_scr %>% filter(Province == input$Province)
    } else {
      df <- df_scr
    }
    plot_ly(
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
        title = paste0(tt(), "<br><sup>Total screening = ", format(sum(df$n), big.mark = ","), "</sup>"),
        xaxis = list(title = '',
                     tickformat = "%b %y"),
        yaxis = list(title = 'Number Screened',
                     range = list(0, 1000)),
        bargap = 0.5
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
        columnDefs = list(list(visible=FALSE, targets=1)),
        initComplete = JS("function(){$(this).addClass('compact');}"),
        dom = 'rt'
      )
    )
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
      hoverinfo = 'y'
    ) %>%
      layout(
        title = tt(),
        xaxis = list(title = 'Previously Enrolled'),
        yaxis = list(title = 'Count'),
        bargap = 0.5,
        legend = list(title=list(text='Enrolled'),
                      x = 100, 
                      y = 0.5)
      )
  })

  output$EnrollmentBar <- renderPlotly({
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
      add_trace(data = df1 %>%
                  group_by(scrdate) %>%
                  summarise(count = sum(n)),
                x = ~ scrdate,
                y = ~ count,
                name = "Eligible",
                type = 'scatter', 
                mode = 'lines',
                hoverinfo = 'y') %>% 
    add_trace(data = df2 %>%
                group_by(enrdate, FinalResult) %>%
                summarise(count = sum(n)),
              x = ~ enrdate,
              y = ~ count,
              color = ~FinalResult,
              type = 'bar',
              hoverinfo = 'y') %>% 
      layout(title = tt(),
             xaxis = list(title = '',
                          tickformat = "%b %y"),
             yaxis = list(title = 'Number Eligible/Enrolled',
                          range = list(0, 400)),
             margin = list(l = 5, r = 5),
             legend = list(orientation = "h",   # show entries horizontally
                           xanchor = "center",  # use center of legend as anchor
                           x = 0.5))             # put legend in center of x-axis)  # use center of legend as anchor)
  })
  
  output$eliBox <- renderValueBox({
    if (input$Hospital != "All") {
      df <- df_eli %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_eli %>% filter(Province == input$Province)
    } else {
      df <- df_eli
    }
    valueBox(
      format(sum(df$n), big.mark= ","), 
      "Eligible",
      color = "aqua"
    )
  })
  
  output$enrolBox <- renderValueBox({
    if (input$Hospital != "All") {
      df <- df_enr %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_enr %>% filter(Province == input$Province)
    } else {
      df <- df_enr
    }
    valueBox(
      format(sum(df$n), big.mark= ","), 
      "Enrolled",
      color = "teal"
    )
  })
  
  output$posBox <- renderValueBox({
    if (input$Hospital != "All") {
      df <- df_enr %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_enr %>% filter(Province == input$Province)
    } else {
      df <- df_enr
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
      mutate(rank = rank(-count, ties.method = "first")) %>% 
      mutate(S34Occupation = ifelse(rank <= 5, levels(S34Occupation)[S34Occupation], "Other")) %>%
      rename(n = count) %>%      
      pie(S34Occupation, tt())
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
            values = ~ count) %>%
      add_pie(
        data = df1,
        name = "Vaccinated",
        scalegroup = 'one',
        domain = list(row = 0, column = 0)
      ) %>%
      add_pie(
        data = df2,
        name = "Unvaccinated",
        scalegroup = 'one',
        domain = list(row = 0, column = 1)
      ) %>%
      layout(
        title = tt(),
        grid = list(rows = 1, columns = 2),
        legend = list(
          orientation = "h",
          # show entries horizontally
          xanchor = "center",
          # use center of legend as anchor
          x = 0.5
        ),
        annotations = list(
          list(
            x = 0.18 ,
            y = 1,
            text = "Vaccinated",
            showarrow = F,
            xref = 'paper',
            yref = 'paper'
          ),
          list(
            x = 0.82 ,
            y = 1,
            text = "Unvaccinated",
            showarrow = F,
            xref = 'paper',
            yref = 'paper'
          )
        )
      )
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
      # filter(
      #   kap %in% c(
      #     'S3604SickSpread',
      #     'S3615CareLate',
      #     'S3616',
      #     'S3617',
      #     'S3618',
      #     'S3619',
      #     'S3620'
      #   )
      # ) %>%
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
      # filter(kap %in% c('S3610MaskIn', 'S3613MaskOut', 'S3621', 'S3622')) %>%
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

  output$Diag <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_dx %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_dx %>% filter(Province == input$Province)
    } else {
      df <- df_dx
    }
    hbar(df, Diagnosis, tt())
    # plot_ly(
    #   data = df %>% 
    #     group_by(FinalResult, Diagnosis) %>% 
    #     summarise(count = sum(n)),
    #   y = ~ Diagnosis,
    #   x = ~ count,
    #   type = "bar",
    #   orientation = 'h',
    #   color = ~ FinalResult,
    #   hoverinfo = 'x'
    # ) %>% 
    #   layout(barmode = 'stack',
    #          xaxis = list(title = 'Count',
    #                       bargap = 0.5),
    #          yaxis = list(title = '',
    #                       categoryorder = "total ascending"))
    #   
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
    # plot_ly(
    #   data = df %>% 
    #     group_by(FinalResult, Underlying) %>% 
    #     summarise(count = sum(n)),
    #   y = ~ Underlying,
    #   x = ~ count,
    #   type = "bar",
    #   orientation = 'h',
    #   color = ~ FinalResult,
    #   hoverinfo = 'x'
    # ) %>% 
    #   layout(barmode = 'stack',
    #          xaxis = list(title = 'Count',
    #                       bargap = 0.5),
    #          yaxis = list(title = '',
    #                       categoryorder = "total ascending"))
    
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
  
}
