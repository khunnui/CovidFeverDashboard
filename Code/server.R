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
    if (input$hospital != "All") {
      tt <- paste0(input$hospital, " Hospital")
    } else if (input$province != "All") {
      tt <- paste0(input$province, " province")
    } else {
      tt <- ""
    }
  })
  
  shinyjs::html("dateHeader", paste0(" Data as of ", ddate))

  output$titletext <- renderText({
    if (input$hospital != "All") {
      titletext <- paste0(input$hospital, " Hospital")
    } else if (input$province != "All") {
      titletext <- paste0(input$province, " province")
    } else {
      titletext <- ""
    }
  })

  observeEvent(input$province, {
    if (input$province == "All") {
      updateSelectInput(
        inputId = "hospital",
        choices = c("All", as.character(unique(df_scrgender$hospital)))
      )
    } else {
      updateSelectInput(
        inputId = "hospital",
        choices = c("All", as.character(unique(df_scrgender$hospital[df_scrgender$province == input$province])))
      )
    }
  })

  output$map <- renderPlot({
    ggdraw() +
      draw_plot(map_cf_tak,      x = 0.06, width = 0.24, y = 0.2, height = 0.6) +
      draw_plot(map_cf_province, x = 0.3,  width = 0.4,  y = 0,   height = 1) +
      draw_plot(map_cf_np,       x = 0.7,  width = 0.25, y = 0.2, height = 0.6)
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
    if (input$hospital != "All") {
      df <- df_scr %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_scr %>% filter(province == input$province)
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
      marker = list(color = color_scr),
      hoverinfo = 'y'
    ) %>%
      layout(
        title = list(text = paste0(
          tt(),
          "<br><sup>Total screening = ",
          format(sum(df$n), big.mark = ","),
          "</sup>"
        ), font = list(family = "Verdana", size = 14)),
        xaxis = l,
        yaxis = list(title = 'Number Screened',
                     tickformat = ','),
        bargap = 0.5,
        margin = list(l = 80, r = 80)
      )
  })
  
  output$ScreeningAge <- renderDT({
    if (input$hospital != "All") {
      df <- df_scrage2 %>% filter(hospital == input$hospital) %>% 
        select(-hospital)
    } else if (input$province != "All") {
      df <- df_scrage1 %>% filter(province == input$province) %>% 
        select(-province)
    } else {
      df <- df_scrage0
    }
    datatable(
      df,
      caption = htmltools::tags$caption(style = "caption-side: top; text-align: center; font-family: Verdana; font-size: 14px;",
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
  
  output$ScreeningAgeGroup <- renderPlotly({
    if (input$hospital != "All") {
      df <- df_scrage %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_scrage %>% filter(province == input$province)
    } else {
      df <- df_scrage
    }
    bar_age(df, "")
  })
  
  output$ScreeningGender <- renderPlotly({
    if (input$hospital != "All") {
      df <- df_scrgender %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_scrgender %>% filter(province == input$province)
    } else {
      df <- df_scrgender
    }
    pie_gender(df, tt())
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
    if (input$hospital != "All") {
      df1 <- df_eli %>% filter(hospital == input$hospital)
      df2 <- df_enr %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df1 <- df_eli %>% filter(province == input$province)
      df2 <- df_enr %>% filter(province == input$province)
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
        marker = list(color = color_eli),
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
        marker = list(color = color_enr),
        hoverinfo = 'y'
      ) %>%
      add_trace(
        data = df2 %>%
          group_by(enrdate) %>%
          summarise(inc = round(sum(n[finalresult == 'Positive'], na.rm = TRUE) / sum(n), 2)),
        x = ~ enrdate,
        y = ~ inc,
        yaxis = "y2",
        name = "COVID-19 Incidence",
        type = 'scatter',
        mode = 'lines',
        line = list(color = color_pos),
        hoverinfo = 'y'
      ) %>%
      layout(
        title = list(text = tt(), font = list(family = "Verdana", size = 14)),
        yaxis2 = list(
          overlaying = "y",
          side = "right",
          rangemode = 'tozero',
          title = 'Incidence',
          range = list(0, 1),
          tickformat = '.0%',
          showgrid = FALSE
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
          y = 1
        )
      )             
  })
  
  output$eliBox <- renderValueBox({
    if (input$hospital != "All") {
      df <- df_elim %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_elim %>% filter(province == input$province)
    } else {
      df <- df_elim
    }
    valueBox(
      format(sum(df$n), big.mark= ","), 
      "Eligible",
      color = "blue"
    )
  })
  
  output$enrolBox <- renderValueBox({
    if (input$hospital != "All") {
      df <- df_enrm %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_enrm %>% filter(province == input$province)
    } else {
      df <- df_enrm
    }
    valueBox(
      format(sum(df$n), big.mark= ","), 
      "Enrolled",
      color = "green"
    )
  })
  
  output$posBox <- renderValueBox({
    if (input$hospital != "All") {
      df <- df_enrm %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_enrm %>% filter(province == input$province)
    } else {
      df <- df_enrm
    }
    df <- filter(df, finalresult == "Positive")
    valueBox(
      format(sum(df$n), big.mark= ","),
      "SARS-CoV-2 Positive",
      color = "orange"
    )
  })
  
  output$pos3weekBox <- renderValueBox({
    if (input$hospital != "All") {
      df <- df_pos3wk %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_pos3wk %>% filter(province == input$province)
    } else {
      df <- df_pos3wk
    }
    df <- filter(df, finalresult == "Positive")
    valueBox(
      format(sum(df$n), big.mark= ","), 
      "Cases detected in last 3 weeks",
      color = "yellow"
    )
  })
  
  output$EnrollmentAge <- renderPlotly({
    if (input$hospital != "All") {
      df <- df_enrage %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_enrage %>% filter(province == input$province)
    } else {
      df <- df_enrage
    }
    #pie(df, agegroup, tt(), FALSE, color_qual)
    bar_age(df, tt())
    
  })
  
  output$EnrollmentGender <- renderPlotly({
    if (input$hospital != "All") {
      df <- df_enrgender %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_enrgender %>% filter(province == input$province)
    } else {
      df <- df_enrgender
    }
    pie_gender(df, tt())
  })
  
  output$EnrollmentOcc <- renderPlotly({
    if (input$hospital != "All") {
      df <- df_enrocc %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_enrocc %>% filter(province == input$province)
    } else {
      df <- df_enrocc
    }
    df %>%
      group_by(s34occupation) %>% # Group by specified column
      summarise(count = sum(n)) %>% 
      mutate(rank = rank(-replace(count,s34occupation=='Other',NA), ties.method = "first")) %>% 
      mutate(s34occupation = ifelse(rank <= 5, levels(s34occupation)[s34occupation], "Other")) %>%
      rename(n = count) %>%      
      pie(s34occupation, tt(), FALSE, color_qual)
  })

  output$Diag <- render_gt({
    if (input$province == "All" & input$hospital == "All") {
      gt_dx
    } else if (input$province == "Nakorn Phanom" & input$hospital == "All") {
      gt_dx_n
    } else if (input$province == "Tak" & input$hospital == "All") {
      gt_dx_t
    } else if (input$hospital == "Nakorn Phanom") {
      gt_dx_n1
    } else if (input$hospital == "Sri Songkhram") {
      gt_dx_n2
    } else if (input$hospital == "That Phanom") {
      gt_dx_n3
    } else if (input$hospital == "Mae Sot") {
      gt_dx_t1
    } else if (input$hospital == "Umphang") {
      gt_dx_t2
    } else if (input$hospital == "Tha Song Yang") {
      gt_dx_t3
    }
  })

  output$Sign <- render_gt({
    if (input$province == "All" & input$hospital == "All") {
      gt_sign
    } else if (input$province == "Nakorn Phanom" & input$hospital == "All") {
      gt_sign_n
    } else if (input$province == "Tak" & input$hospital == "All") {
      gt_sign_t
    } else if (input$hospital == "Nakorn Phanom") {
      gt_sign_n1
    } else if (input$hospital == "Sri Songkhram") {
      gt_sign_n2
    } else if (input$hospital == "That Phanom") {
      gt_sign_n3
    } else if (input$hospital == "Mae Sot") {
      gt_sign_t1
    } else if (input$hospital == "Umphang") {
      gt_sign_t2
    } else if (input$hospital == "Tha Song Yang") {
      gt_sign_t3
    }
  })
  
  output$Underly <- render_gt({
    if (input$province == "All" & input$hospital == "All") {
      gt_un
    } else if (input$province == "Nakorn Phanom" & input$hospital == "All") {
      gt_un_n
    } else if (input$province == "Tak" & input$hospital == "All") {
      gt_un_t
    } else if (input$hospital == "Nakorn Phanom") {
      gt_un_n1
    } else if (input$hospital == "Sri Songkhram") {
      gt_un_n2
    } else if (input$hospital == "That Phanom") {
      gt_un_n3
    } else if (input$hospital == "Mae Sot") {
      gt_un_t1
    } else if (input$hospital == "Umphang") {
      gt_un_t2
    } else if (input$hospital == "Tha Song Yang") {
      gt_un_t3
    }
  })  
  
  output$Risk <- render_gt({
    if (input$province == "All" & input$hospital == "All") {
      gt_rf
    } else if (input$province == "Nakorn Phanom" & input$hospital == "All") {
      gt_rf_n
    } else if (input$province == "Tak" & input$hospital == "All") {
      gt_rf_t
    } else if (input$hospital == "Nakorn Phanom") {
      gt_rf_n1
    } else if (input$hospital == "Sri Songkhram") {
      gt_rf_n2
    } else if (input$hospital == "That Phanom") {
      gt_rf_n3
    } else if (input$hospital == "Mae Sot") {
      gt_rf_t1
    } else if (input$hospital == "Umphang") {
      gt_rf_t2
    } else if (input$hospital == "Tha Song Yang") {
      gt_rf_t3
    }
  })

  output$posBoxSign <- renderValueBox({
    if (input$hospital != "All") {
      df <- df_enrm %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_enrm %>% filter(province == input$province)
    } else {
      df <- df_enrm
    }
    df <- filter(df, finalresult == "Positive")
    valueBox(
      tags$p(format(sum(df$n), big.mark= ","), style = "font-size: 75%;"),
      subtitle = " SARS-CoV-2 Positive",
      color = "orange"
    )
  })

  output$hospitalised <- renderValueBox({
    if (input$hospital != "All") {
      df <- df_signBox %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_signBox %>% filter(province == input$province)
    } else {
      df <- df_signBox
    }
    valueBox(
      tags$p(format(sum(df$n), big.mark= ","), style = "font-size: 75%;"),
      "Hospitalized",
      color = "yellow"
    )
  })
  
  output$intub <- renderValueBox({
    if (input$hospital != "All") {
      df <- df_signBox %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_signBox %>% filter(province == input$province)
    } else {
      df <- df_signBox
    }
    df <- filter(df, s5intub == 2)
    valueBox(
      tags$p(format(sum(df$n), big.mark= ","), style = "font-size: 75%;"),
      "Intubation",
      color = "red"
    )
  })
  
  output$death <- renderValueBox({
    if (input$hospital != "All") {
      df <- df_signBox %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_signBox %>% filter(province == input$province)
    } else {
      df <- df_signBox
    }
    df <- filter(df, s5dischargetype == 4)
    valueBox(
      tags$p(format(sum(df$n), big.mark= ","), style = "font-size: 75%;"),
      "Death",
      color = "black"
    )
  })
  
  output$VaccineSunburst <- renderPlotly({
    if (input$hospital != "All") {
      df <- df_vac %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_vac %>% filter(province == input$province)
    } else {
      df <- df_vac
    }
    df %>% 
      group_by(s33covidvaccine, finalresult) %>% # Group by specified column
      summarise(count = sum(n)) %>% 
      sunburst_df(value_column = "count", add_root = FALSE) %>% 
      mutate(colors = case_when(
        ids == 'Vaccinated'    ~ '#ace1af',
        ids == 'Unvaccinated'  ~ '#a1caf1',
        ids == 'Unknown'       ~ '#ebe6e5',
        grepl('Positive', ids) ~ '#b78f62'
      )) %>% 
      plot_ly(
        ids     = ~ ids,
        labels  = ~ labels,
        parents = ~ parents,
        values  = ~ values,
        type = 'sunburst',
        branchvalues = 'total',
        textinfo = 'label+percent entry',
        marker = list(colors = ~ colors)
      ) %>%
      layout(
        title = list(text = tt(), font = list(family = "Verdana", size = 14)),
        margin = list(l = 30, r = 30, t= 30))
  }) 
  
  output$atkPie <- renderPlotly({
    if (input$hospital != "All") {
      df <- df_atk %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_atk %>% filter(province == input$province)
    } else {
      df <- df_atk
    }
    pie(df, atkresult, tt(), FALSE, color_posneg)
  }) 
  
  output$DetectBar <- renderPlotly({
    if (input$hospital != "All") {
      df <- df_lab %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_lab %>% filter(province == input$province)
    } else {
      df <- df_lab
    }
    plot_ly(
      df %>%
        group_by(spectype) %>%
        summarise(total = sum(n),
                  positive = sum(n[finalresult == 'Positive'], na.rm = TRUE)),
      x = ~ spectype,
      y = ~ total,
      type = "bar",
      marker = list(color = color_scr),
      name = 'Tested',
      hoverinfo = 'y',
      hovertemplate = '%{y:,}<extra></extra>'
    ) %>% 
      add_trace(y = ~ positive, 
                marker = list(color = color_pos),
                name = 'PCR Positive',
                hoverinfo = 'y',
                hovertemplate = '%{y:,}<extra></extra>') %>% 
      layout(
        title = list(text = tt(), font = list(family = "Verdana", size = 14)),
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
    if (input$hospital != "All") {
      df <- df_labpos %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_labpos %>% filter(province == input$province)
    } else {
      df <- df_labpos
    }
    pie(df, specimens, tt(), FALSE, color_qual)
  })  
    
  output$kap1 <- renderPlotly({
    if (input$hospital != "All") {
      df <- df_kap1 %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_kap1 %>% filter(province == input$province)
    } else {
      df <- df_kap1
    }
    df %>%
      mutate(
        kap = factor(recode(
          kap,
          's3604sickspread' = 'Only people who are sick and who shows symptoms can\nspread the disease',
          's3615carelate'   = 'I sought care today later than I usual\nbecause of COVID-19',
          's3616'           = 'I was afraid of being placed under quarantine after\nclose contact with COVID-19 patient',
          's3617'           = 'I was afraid to seek care today or previously out of\nfear of being tested for COVID-19/isolated in hospital',
          's3618'           = 'Always wearing mask in public is a good thing to do',
          's3619'           = 'Always practicing social distancing from other people\nis a good thing to do',
          's3620'           = 'Patients should disclose their exposure to COVID-19\nand their symptoms'
        ))
      ) %>%
      bar_scale(kap, color_scale1)
  })
  
  output$kap2 <- renderPlotly({
    if (input$hospital != "All") {
      df <- df_kap2 %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_kap2 %>% filter(province == input$province)
    } else {
      df <- df_kap2
    }
    df %>%
      mutate(
        kap = factor(recode(
          kap,
          's3610maskin'     = 'During the past 2 weeks, did you wear a mask at home?',
          's3613maskout'    = 'Did you wear a mask when you went outside of your\nresidence in crowded areas?',
          's3621'           = 'Do you practice social distancing from other persons\nin your household?',
          's3622'           = 'Do you practice social distancing from other persons\noutside of your residence?'
        ))
      ) %>%
      bar_scale(kap, color_scale2)
  })
  
}
