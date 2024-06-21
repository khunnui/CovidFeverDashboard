################################################################################
# server.R
################################################################################

library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)
library(svglite)
library(gtsummary)
library(dplyr)

# library(cowplot)
theme_gtsummary_compact()

server <- function(input, output, session) {

  tt <- reactive({
    if (input$hospital != "All") {
      tt <- paste0(input$hospital, " Hospital")
    } else if (input$province != "All") {
      tt <- paste0(input$province, " Province")
    } else {
      tt <- ""
    }
    if (input$rps == "Yes") {
      tt <- paste0(tt, " (RPS Only)")
    } else if (input$rps == "No") {
      tt <- paste0(tt, " (Non-RPS Only)")
    } else {
      tt <- paste0(tt, " ")
    }
  })
  
 # shinyjs::html("dateHeader", paste0(" Data as of ", ddate))
  shinyjs::html("dateHeader", "go to <a href='https://dghp.shinyapps.io/Long-COVID/'> Long COVID dashboard </a>")

  output$titletext <- renderText({
    if (input$hospital != "All") {
      titletext <- paste0(input$hospital, " Hospital")
    } else if (input$province != "All") {
      titletext <- paste0(input$province, " province")
    } else {
      titletext <- ""
    }
    if (input$rps == "Yes") {
      titletext <- paste0(titletext, " (RPS Only)")
    } else if (input$rps == "No") {
      titletext <- paste0(titletext, " (Non-RPS Only)")
    } else {
      titletext <- paste0(titletext, " ")
    }
  })

  # output$dl <- downloadHandler(
  #   filename = "CRF.pdf",
  #   content = function(file) {
  #     file.copy("www/crf.pdf", file)
  #   }
  # )
  
  output$pdfview <- renderUI({
    tags$iframe(style = "height:920px; width:100%", src = "CRF.pdf")
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

  # output$map <- renderPlot({
  #   ggdraw() +
  #     draw_plot(map_cf_tak,      x = 0.06, width = 0.24, y = 0.2, height = 0.6) +
  #     draw_plot(map_cf_province, x = 0.3,  width = 0.4,  y = 0,   height = 1) +
  #     draw_plot(map_cf_np,       x = 0.7,  width = 0.25, y = 0.2, height = 0.6)
  # })

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
     if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
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
      df <- df_scragestat %>% filter(hospital == input$hospital) %>% 
        select(-hospital)
    } else if (input$province != "All") {
      df <- df_scragestat %>% filter(province == input$province) %>% 
        select(-province)
    } else {
      df <- df_scragestat
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    df <- df %>% 
      summarize(
        n = n(),
        min = min(s1age_year, na.rm = TRUE),
        q1 = quantile(s1age_year, 0.25, na.rm = TRUE),
        median = median(s1age_year, na.rm = TRUE),
        mean = round(mean(s1age_year, na.rm = TRUE), 1),
        q3 = quantile(s1age_year, 0.75, na.rm = TRUE),
        max = max(s1age_year, na.rm = TRUE)
      ) %>% 
      
    datatable(df,
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
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
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
     if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    pie1(df, s1gender, tt(), color_gender)
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
    if (input$rps == "Yes") {
      df1 <- df1 %>% filter(rps == TRUE)
      df2 <- df2 %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df1 <- df1 %>% filter(rps == FALSE)
      df2 <- df2 %>% filter(rps == FALSE)
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
        name = "COVID-19 Positive rate",
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
          title = '% SARS-CoV-2 RT-PCR positive',
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
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
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
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
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
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
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
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
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
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
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
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    pie1(df, s1gender, tt(), color_gender)
  })
  
  output$EnrollmentOcc <- renderPlotly({
    if (input$hospital != "All") {
      df <- df_enrocc %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_enrocc %>% filter(province == input$province)
    } else {
      df <- df_enrocc
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    df %>%
      group_by(s34occupation) %>% # Group by specified column
      summarise(count = sum(n)) %>% 
      mutate(rank = rank(-replace(count,s34occupation=='Other',NA), ties.method = "first")) %>% 
      mutate(s34occupation = ifelse(rank <= 5, levels(s34occupation)[s34occupation], "Other")) %>%
      rename(n = count) %>%      
      pie2(s34occupation, tt())
  })

  output$Diag <- render_gt({
    if (input$hospital != "All") {
      df <- df_dx %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_dx %>% filter(province == input$province)
    } else {
      df <- df_dx
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    
    # Calculate counts for each variable and sort them
    overall_counts <- df %>%
      select(-c('province', 'hospital', 'rps')) %>%
      summarise(across(everything(), ~ sum(!is.na(.)), .names = "count_{col}")) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "count") %>%
      arrange(desc(count)) %>%
      mutate(variable = gsub("count_", "", variable)) %>%
      pull(variable)
    
    # Create the summary table using the sorted variables
     tbl_summary(
      data = df %>% select(all_of(overall_counts), finalresult),
      by = finalresult,
      digits = list(all_categorical() ~ c(0, 1)),
      missing = 'no'
    ) %>%
      add_overall() %>%
       modify_table_body(
         fun = ~ dplyr::arrange(.x, desc(readr::parse_number(stat_0)))
       ) %>%
      modify_header(update = list(label =" ",
                                  all_stat_cols() ~ "**{level}**<br>N = {n}")) %>%
      modify_spanning_header(stat_1:stat_2 ~ "**PCR Result**") %>%
      
      as_gt() %>%
      tab_header(
        title = "",
        subtitle = tt()
      ) %>%
      tab_options(table.border.bottom.style = 'none')
  })
  
  #    if (input$rps == "All") {
  #     if (input$province == "All" & input$hospital == "All") {
  #       gt_dx
  #     } else if (input$province == "Nakorn Phanom" & input$hospital == "All") {
  #       gt_dx_n
  #     } else if (input$hospital == "Nakorn Phanom") {
  #       gt_dx_n1
  #     } else if (input$hospital == "Sri Songkhram") {
  #       gt_dx_n2
  #     } else if (input$hospital == "That Phanom") {
  #       gt_dx_n3
  #     } else if (input$province == "Tak" & input$hospital == "All") {
  #       gt_dx_t
  #     } else if (input$hospital == "Mae Sot") {
  #       gt_dx_t1
  #     } else if (input$hospital == "Umphang") {
  #       gt_dx_t2
  #     } else if (input$hospital == "Tha Song Yang") {
  #       gt_dx_t3
  #     }
  #    } else if (input$rps == "Yes") {
  #      if (input$province == "All" & input$hospital == "All") {
  #        gt_dx_rps
  #      } else if (input$province == "Nakorn Phanom" & input$hospital == "All") {
  #        gt_dx_rps_n
  #      } else if (input$hospital == "Nakorn Phanom") {
  #        gt_dx_rps_n1
  #      } else if (input$hospital == "Sri Songkhram") {
  #        gt_dx_rps_n2
  #      } else if (input$hospital == "That Phanom") {
  #        gt_dx_rps_n3
  #      } else if (input$province == "Tak" & input$hospital == "All") {
  #        gt_dx_rps_t
  #      } else if (input$hospital == "Mae Sot") {
  #        gt_dx_rps_t1
  #      } else if (input$hospital == "Umphang") {
  #        gt_dx_rps_t2
  #      } else if (input$hospital == "Tha Song Yang") {
  #        gt_dx_rps_t3
  #      }
  #    } else if (input$rps == "No") {
  #      if (input$province == "All" & input$hospital == "All") {
  #       gt_dx_norps
  #     } else if (input$province == "Nakorn Phanom" & input$hospital == "All") {
  #        gt_dx_norps_n
  #      } else if (input$hospital == "Nakorn Phanom") {
  #        gt_dx_norps_n1
  #      } else if (input$hospital == "Sri Songkhram") {
  #        gt_dx_norps_n2
  #      } else if (input$hospital == "That Phanom") {
  #        gt_dx_norps_n3
  #      } else if (input$province == "Tak" & input$hospital == "All") {
  #        gt_dx_norps_t
  #      } else if (input$hospital == "Mae Sot") {
  #        gt_dx_norps_t1
  #      } else if (input$hospital == "Umphang") {
  #        gt_dx_norps_t2
  #      } else if (input$hospital == "Tha Song Yang") {
  #        gt_dx_norps_t3
  #      }
  #    }
  # })
  # 
  output$posBoxSign <- renderValueBox({
    if (input$hospital != "All") {
      df <- df_enrm %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_enrm %>% filter(province == input$province)
    } else {
      df <- df_enrm
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
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
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
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
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
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
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    df <- filter(df, s5dischargetype == 4)
    valueBox(
      tags$p(format(sum(df$n), big.mark= ","), style = "font-size: 75%;"),
      "Death",
      color = "black"
    )
  })
  
  output$Sign <- render_gt({
    
    if (input$hospital != "All") {
      df <- df_ss %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_ss %>% filter(province == input$province)
    } else {
      df <- df_ss
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    
    tbl_summary(
      data = df %>% select(-c('province', 'hospital', 'rps')),
      by = finalresult,
      digits = list(all_categorical() ~ c(0, 1)),
      missing = 'no'
    ) %>%
      add_overall() %>%
      modify_table_body(
        fun = ~ dplyr::arrange(.x, desc(readr::parse_number(stat_0)))
      ) %>%
      modify_header(update = list(label = " ",
                                  all_stat_cols() ~ "**{level}**<br>N = {n}")) %>%
      modify_spanning_header(stat_1:stat_2 ~ "**PCR Result**") %>%
      as_gt() %>%
      tab_header(
        title = "",
        subtitle = tt()
      ) %>%
      tab_options(table.border.bottom.style = 'none')
  })
 
  output$SignBF <- render_gt({
    if (input$hospital != "All") {
      df <- df_ss_bf %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_ss_bf %>% filter(province == input$province)
    } else {
      df <- df_ss_bf
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    tbl_summary(data = df %>% select(-c('rps')),
                by = visit,
                digits = list(all_categorical() ~ c(0, 1))) %>%
      add_overall() %>%
      modify_table_body(
        fun = ~ dplyr::arrange(.x, desc(readr::parse_number(stat_0)))
      ) %>%
      modify_header(update = list(label = " ",
                                  all_stat_cols() ~ "**{level}**<br>N = {n}")) %>%
      as_gt() %>% 
      tab_header(
        title = "",
        subtitle = tt()
      ) %>%   
      tab_options(table.border.bottom.style = 'none')
  })
  
  output$Underly <- render_gt({
    if (input$hospital != "All") {
      df <- df_un %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_un %>% filter(province == input$province)
    } else {
      df <- df_un
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    
    tbl_summary(
      data = df %>% select(-c('province', 'hospital', 'rps')),
      by = finalresult,
      digits = list(all_categorical() ~ c(0, 1)),
      missing = 'no'
    ) %>%
      add_overall() %>%
      modify_table_body(
        fun = ~ dplyr::arrange(.x, desc(readr::parse_number(stat_0)))
      ) %>%
      modify_header(update = list(label = " ",
                                  all_stat_cols() ~ "**{level}**<br>N = {n}")) %>%
      modify_spanning_header(stat_1:stat_2 ~ "**PCR Result**") %>%
      as_gt() %>%
      tab_header(
        title = "",
        subtitle = tt()
      ) %>%   
      tab_options(table.border.bottom.style = 'none')
  })
    # if (input$rps == "All") {
      # if (input$province == "All" & input$hospital == "All") {
      #   gt_un
      # } else if (input$province == "Nakorn Phanom" & input$hospital == "All") {
      #   gt_un_n
      # } else if (input$hospital == "Nakorn Phanom") {
      #   gt_un_n1
      # } else if (input$hospital == "Sri Songkhram") {
      #   gt_un_n2
      # } else if (input$hospital == "That Phanom") {
      #   gt_un_n3
      # } else if (input$province == "Tak" & input$hospital == "All") {
      #   gt_un_t
      # } else if (input$hospital == "Mae Sot") {
      #   gt_un_t1
      # } else if (input$hospital == "Umphang") {
      #   gt_un_t2
      # } else if (input$hospital == "Tha Song Yang") {
      #   gt_un_t3
      # }
    # } else if (input$rps == "Yes") {
    #   if (input$province == "All" & input$hospital == "All") {
    #     gt_un_rps
    #   } else if (input$province == "Nakorn Phanom" & input$hospital == "All") {
    #     gt_un_rps_n
    #   } else if (input$hospital == "Nakorn Phanom") {
    #     gt_un_rps_n1
    #   } else if (input$hospital == "Sri Songkhram") {
    #     gt_un_rps_n2
    #   } else if (input$hospital == "That Phanom") {
    #     gt_un_rps_n3
    #   } else if (input$province == "Tak" & input$hospital == "All") {
    #     gt_un_rps_t
    #   } else if (input$hospital == "Mae Sot") {
    #     gt_un_rps_t1
    #   } else if (input$hospital == "Umphang") {
    #     gt_un_rps_t2
    #   } else if (input$hospital == "Tha Song Yang") {
    #     gt_un_rps_t3
    #   }
    # } else if (input$rps == "No") {
    #   if (input$province == "All" & input$hospital == "All") {
    #     gt_un_norps
    #   } else if (input$province == "Nakorn Phanom" & input$hospital == "All") {
    #     gt_un_norps_n
    #   } else if (input$hospital == "Nakorn Phanom") {
    #     gt_un_norps_n1
    #   } else if (input$hospital == "Sri Songkhram") {
    #     gt_un_norps_n2
    #   } else if (input$hospital == "That Phanom") {
    #     gt_un_norps_n3
    #   } else if (input$province == "Tak" & input$hospital == "All") {
    #     gt_un_norps_t
    #   } else if (input$hospital == "Mae Sot") {
    #     gt_un_norps_t1
    #   } else if (input$hospital == "Umphang") {
    #     gt_un_norps_t2
    #   } else if (input$hospital == "Tha Song Yang") {
    #     gt_un_norps_t3
    #   }
    # }
 # })
  
  output$Risk <- render_gt({
      if (input$hospital != "All") {
        df <- df_rf %>% filter(hospital == input$hospital)
      } else if (input$province != "All") {
        df <- df_rf %>% filter(province == input$province)
      } else {
        df <- df_rf
      }
      if (input$rps == "Yes") {
        df <- df %>% filter(rps == TRUE)
      } else if (input$rps == "No") {
        df <- df %>% filter(rps == FALSE)
      }
      
      tbl_summary(
        data = df %>% select(-c('province', 'hospital', 'rps')),
        by = finalresult,
        digits = list(all_categorical() ~ c(0, 1)),
        missing = 'no'
      ) %>%
        add_overall() %>%
        modify_header(update = list(label =  " ",
                                    all_stat_cols() ~ "**{level}**<br>N = {n}")) %>%
        modify_spanning_header(stat_1:stat_2 ~ "**PCR Result**") %>%
        as_gt() %>%
        tab_header(
          title = "",
          subtitle = tt()
        ) %>% 
        tab_options(table.border.bottom.style = 'none')
    })
    # # if (input$rps == "All") {
    #   if (input$province == "All" & input$hospital == "All") {
    #     gt_rf
    #   } else if (input$province == "Nakorn Phanom" & input$hospital == "All") {
    #     gt_rf_n
    #   } else if (input$hospital == "Nakorn Phanom") {
    #     gt_rf_n1
    #   } else if (input$hospital == "Sri Songkhram") {
    #     gt_rf_n2
    #   } else if (input$hospital == "That Phanom") {
    #     gt_rf_n3
    #   } else if (input$province == "Tak" & input$hospital == "All") {
    #     gt_rf_t
    #   } else if (input$hospital == "Mae Sot") {
    #     gt_rf_t1
    #   } else if (input$hospital == "Umphang") {
    #     gt_rf_t2
    #   } else if (input$hospital == "Tha Song Yang") {
    #     gt_rf_t3
    #   }
    # } else if (input$rps == "Yes") {
    #   if (input$province == "All" & input$hospital == "All") {
    #     gt_rf_rps
    #   } else if (input$province == "Nakorn Phanom" & input$hospital == "All") {
    #     gt_rf_rps_n
    #   } else if (input$hospital == "Nakorn Phanom") {
    #     gt_rf_rps_n1
    #   } else if (input$hospital == "Sri Songkhram") {
    #     gt_rf_rps_n2
    #   } else if (input$hospital == "That Phanom") {
    #     gt_rf_rps_n3
    #   } else if (input$province == "Tak" & input$hospital == "All") {
    #     gt_rf_rps_t
    #   } else if (input$hospital == "Mae Sot") {
    #     gt_rf_rps_t1
    #   } else if (input$hospital == "Umphang") {
    #     gt_rf_rps_t2
    #   } else if (input$hospital == "Tha Song Yang") {
    #     gt_rf_rps_t3
    #   }
    # } else if (input$rps == "No") {
    #   if (input$province == "All" & input$hospital == "All") {
    #     gt_rf_norps
    #   } else if (input$province == "Nakorn Phanom" & input$hospital == "All") {
    #     gt_rf_norps_n
    #   } else if (input$hospital == "Nakorn Phanom") {
    #     gt_rf_norps_n1
    #   } else if (input$hospital == "Sri Songkhram") {
    #     gt_rf_norps_n2
    #   } else if (input$hospital == "That Phanom") {
    #     gt_rf_norps_n3
    #   } else if (input$province == "Tak" & input$hospital == "All") {
    #     gt_rf_norps_t
    #   } else if (input$hospital == "Mae Sot") {
    #     gt_rf_norps_t1
    #   } else if (input$hospital == "Umphang") {
    #     gt_rf_norps_t2
    #   } else if (input$hospital == "Tha Song Yang") {
    #     gt_rf_norps_t3
    #   }
    # }
 # })

  output$VaccineSunburst <- renderPlotly({
    if (input$hospital != "All") {
      df <- df_vac %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_vac %>% filter(province == input$province)
    } else {
      df <- df_vac
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    df %>% 
      group_by(vac, finalresult) %>% # Group by specified column
      summarise(count = sum(n)) %>% 
      sunburst_df(value_column = "count", add_root = FALSE) %>% 
      mutate(colors = case_when(
        ids == 'Fully vaccinated'     ~ '#ace1af',
        ids == 'Partially vaccinated' ~ '#f8de7e',
        ids == 'Unvaccinated'         ~ '#ffcba4',
        ids == 'Unknown'              ~ '#ebe6e5',
        grepl('Positive', ids)        ~ '#b78f62'
      )) %>% 
      plot_ly(
        ids     = ~ ids,
        labels  = ~ labels,
        parents = ~ parents,
        values  = ~ values,
        type = 'sunburst',
        branchvalues = 'total',
        sort = FALSE,
        rotation = 90,
        textinfo = 'label+percent entry',
        marker = list(colors = ~ colors)
      ) %>%
      layout(
        title = list(text = tt(), font = list(family = "Verdana", size = 14)),
        margin = list(l = 30, r = 30, t= 30))
  }) 
  
  output$vac2 <- render_gt({
    if (input$hospital != "All") {
      df <- df_vac2 %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_vac2 %>% filter(province == input$province)
    } else {
      df <- df_vac2
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    
    tbl_summary(
      data = df %>% select(-c('province', 'hospital', 'rps')),
      by = finalresult,
      digits = list(all_categorical() ~ c(0, 1)),
      missing = 'no'
    ) %>%
      add_overall() %>%
      modify_header(update = list(label =  " ",
                                  all_stat_cols() ~ "**{level}**<br>N = {n}")) %>%
      modify_spanning_header(stat_1:stat_2 ~ "**PCR Result**") %>%
      as_gt() %>%
      tab_header(
        title = "",
        subtitle = tt()
      ) %>% 
      tab_options(table.border.bottom.style = 'none')
  })
  
  output$atkPie <- renderPlotly({
    if (input$hospital != "All") {
      df <- df_atk %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_atk %>% filter(province == input$province)
    } else {
      df <- df_atk
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    pie1(df, atkresult, tt(), color_atk)
  }) 
  
  output$DetectBar <- renderPlotly({
    if (input$hospital != "All" ) {
      df <- df_lab %>% filter(hospital == input$hospital)
    } else if (input$province != "All" ) {
      df <- df_lab %>% filter(province == input$province)
    } else {
      df <- df_lab
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    plot_ly(
      df %>%
        group_by(spectype) %>%
        summarise(total = sum(n),
                  positive = sum(n[finalresult == 'Positive'], na.rm = TRUE),
                  pct = round(positive / total * 100, 1)),
      x = ~ spectype
    ) %>%
      add_trace(
        y = ~ total,
        type = "bar",
        marker = list(color = color_scr),
        name = 'Tested',
        text = ~ total,
        textposition = "none",
        hoverinfo = 'text'
      ) %>%
      add_trace(
        y = ~ positive,
        type = "bar",
        marker = list(color = color_pos),
        name = 'PCR Positive',
        text = ~ paste(positive, "/", total, " (", pct, "%)"),
        textposition = "none",
        hoverinfo = 'text'
      ) %>%
      layout(
        title = list(
          text = tt(),
          font = list(family = "Verdana", size = 14)
        ),
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
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    pie2(df, specimens, tt(), 90)
  })  
  
  #lab
  output$labcbc <- render_gt({
    if (input$hospital != "All") {
      df <- df_cbc %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_cbc %>% filter(province == input$province)
    } else {
      df <- df_cbc
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    tcbc <- tbl_summary(
      data = df %>%
        select (
          finalresult,
          s4hematocrit,
          s4plateletx10,
          s4wbccountx10,
          s4neutrophil,
          s4lymphocyte,
          s4monocyte,
          s4eosinophil,
          s4basophil
        ),
      by = finalresult,
      digits = list(all_categorical() ~ c(0, 1)),
      missing = "no"
    ) %>%
      add_overall() %>%
      modify_header(update = list(label = tt(),
                                  all_stat_cols() ~ "**{level}**<br>N = {n}")) %>%
      modify_spanning_header(stat_0:stat_2 ~ "SARS-CoV-2 RT-PCR")
    tbio <- tbl_summary(
      data = df %>%
        select (
          finalresult,
          s4bun,
          s4creatinine,
          s4ast,
          s4alt,
          s4albumin,
          s4lactate,
          s4procal,
          s4creprotein
        ),
      by = finalresult,
      digits = list(all_categorical() ~ c(0, 1)),
      missing = "no"
    ) %>%
      add_overall() %>%
      modify_header(update = list(label = tt(),
                                  all_stat_cols() ~ "**{level}**<br>N = {n}"))
    tbl_stack(list(tcbc, tbio),
              group_header = c("Complete Blood Count", "Blood Chemistry")) %>%
      as_gt() %>%
      tab_header(
        title = "",
        subtitle = tt()
      ) %>% 
      tab_options(table.border.bottom.style = 'none') %>%
      tab_style(style = cell_text(weight = "bold"),
                locations = cells_column_spanners()) %>%
      tab_style(style = cell_text(weight = "bold"),
                locations = cells_row_groups())
  })
  
  output$labcul <- render_gt({
    if (input$hospital != "All") {
      df <- df_cul %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_cul %>% filter(province == input$province)
    } else {
      df <- df_cul
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    tcul <- tbl_summary(
      data = df  %>% select(-c('province', 'hospital', 'rps')),
      by = finalresult,
      digits = list(all_categorical() ~ c(0, 1)),
      missing = "no"
    ) %>%
      add_overall() %>%
      italicize_labels() %>%
      modify_header(update = list(label = "**Organisms**",
                                  all_stat_cols() ~ "**{level}**<br>N = {n}")) %>%
      modify_spanning_header(stat_0:stat_2 ~ "SARS-CoV-2 RT-PCR") %>%
      as_gt() %>%
      tab_header(
        title = "",
        subtitle = tt()
      ) %>% 
      tab_options(table.border.bottom.style = 'none') %>%
      tab_style(style = cell_text(weight = "bold"),
                locations = cells_column_spanners())
  })
  
# sero
  output$sero1a <- render_gt({
    if (input$hospital != "All") {
      df <- df_sero1a %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_sero1a %>% filter(province == input$province)
    } else {
      df <- df_sero1a
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    tbl_summary(data = df %>% select(-c('province', 'hospital','rps')),
                by = finalresult,
                digits = list(all_categorical() ~ c(0, 1))) %>%
      add_overall() %>%
      modify_header(update = list(label = "**Serology Testing**",
                                  all_stat_cols() ~ "**{level}**<br>N = {n}")) %>%
      modify_spanning_header(stat_1:stat_2 ~ "**PCR Result**") %>% 
      as_gt() %>% 
      tab_header(
        title = "",
        subtitle = tt()
      ) %>% 
      tab_options(table.border.bottom.style = 'none')
  })
    
  output$sero1b <- render_gt({
    if (input$hospital != "All") {
      df <- df_sero1b %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_sero1b %>% filter(province == input$province)
    } else {
      df <- df_sero1b
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    if (input$serox == "2") {
      df <- df %>% filter(finalresult == 'Positive')
    } else if (input$serox == "3") {
      df <- df %>% filter(finalresult == 'Negative')
    }
    df <- df %>% select(-c('province', 'hospital', 'finalresult','rps'))
    # Call functions to create columns
    t1bo    <- df %>% select(-c(igminterpret, igginterpret, iggquantiinterpret)) %>% create_t1bo()
    t1bigm  <- df %>% select(-c(igginterpret, iggquantiinterpret)) %>% create_t1b(igminterpret)
    t1biggn <- df %>% select(-c(igminterpret, iggquantiinterpret)) %>% create_t1b(igginterpret)
    t1biggs <- df %>% select(-c(igminterpret, igginterpret)) %>% create_t1b(iggquantiinterpret)
    # Merge all columns side by side
    tbl_merge(list(t1bo, t1bigm, t1biggn, t1biggs),
              tab_spanner = c('**Overall**', '**IgM**', '**IgG-N**', '**IgG-S**')) %>%
    modify_header(update = list(label = " ",
                                  all_stat_cols() ~ "**{level}**<br>N = {n}")) %>%
      as_gt() %>% 
      tab_header(
        title = "",
        subtitle = tt()
      ) %>% 
      tab_options(table.border.bottom.style = 'none')
  })
  
  output$sero2a <- render_gt({
    if (input$hospital != "All") {
      df <- df_sero2a %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_sero2a %>% filter(province == input$province)
    } else {
      df <- df_sero2a
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    df <- df %>% select(-c('province', 'hospital','rps'))
    list(
      df,
      df %>% filter(igm_o == 'Positive'),
      df %>% filter(igm_11 == 'Positive'),
      df %>% filter(igm_91 == 'Positive'),
      df %>% filter(iggn_o == 'Positive'),
      df %>% filter(iggn_11 == 'Positive'),
      df %>% filter(iggn_91 == 'Positive'),
      df %>% filter(iggs_o == 'Positive'),
      df %>% filter(iggs_11 == 'Positive'),
      df %>% filter(iggs_91 == 'Positive')
    ) %>%
      map( ~ select(.x,-c(igm_o:iggs_91))) %>%
      map( ~ tbl_summary(.x, type = all_continuous() ~ "continuous2")) %>%
      map( ~ bold_labels(.x)) %>%
      tbl_merge(tab_spanner = FALSE) %>%
      modify_header(update = list(label = " ",
                                  all_stat_cols() ~ "**{level}**<br>N = {n}")) %>%
      modify_spanning_header(list(
        c(stat_0_2, stat_0_5, stat_0_8) ~ "Overall<br>+ve",
        c(stat_0_3, stat_0_6, stat_0_9) ~ "1st sample<br>+ve",
        c(stat_0_4, stat_0_7, stat_0_10) ~ "Last sample<br>+ve"
      )) %>%
      as_gt() %>% 
      tab_header(
        title = "",
        subtitle = tt()
      ) %>% 
      tab_spanner(
        columns = c(stat_0_1),
        label = "Total",
        level = 2,
        gather = FALSE
      ) %>%
      tab_spanner(
        columns = c(stat_0_2, stat_0_3, stat_0_4),
        label = "IgM",
        gather = FALSE
      ) %>%
      tab_spanner(
        columns = c(stat_0_5, stat_0_6, stat_0_7),
        label = "IgG-N",
        gather = FALSE
      ) %>%
      tab_spanner(
        columns = c(stat_0_8, stat_0_9, stat_0_10),
        label = "IgG-S",
        gather = FALSE
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_spanners(c(1:5))
      ) %>%
      tab_options(table.border.bottom.style = 'none')
  })
  
  output$sero2b <- render_gt({
    if (input$hospital != "All") {
      df <- df_sero2b %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_sero2b %>% filter(province == input$province)
    } else {
      df <- df_sero2b
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    df <- df %>% select(-c('province', 'hospital','rps'))
    tn <- df %>% 
      select(-c(iggsq_11:iggsq_91,tigsfold)) %>% 
      tbl_summary() %>% 
      bold_labels() %>% 
      modify_footnote(update = everything() ~ NA) 
    tigs11 <- df %>% 
      filter(!is.na(iggsq_11)) %>%
      select(-c(iggsq_91,tigsfold)) %>% 
      tbl_continuous(variable = iggsq_11) %>% 
      modify_footnote(update = everything() ~ NA)
    tigs91 <- df %>% 
      filter(!is.na(iggsq_91) ) %>%
      select(-c(iggsq_11,tigsfold)) %>% 
      tbl_continuous(variable = iggsq_91) %>%
      modify_footnote(update = everything() ~ NA) 
    tfold <- df %>% 
      filter(!is.na(tigsfold) ) %>%
      select(-c(iggsq_11,iggsq_91)) %>% 
      tbl_continuous(variable = tigsfold) %>%
      modify_footnote(update = everything() ~ NA) 
    tbl_merge(
      list(tn, tigs11, tigs91,  tfold),
      tab_spanner = c(
        '**Total**',
        '**Baseline**',
        '**F/U 4-6 weeks**',
        '**Fold change**'
      )
    ) %>%
      modify_header(update = list(label = " ",
                                  all_stat_cols() ~ "**{level}**<br>N = {n}")) %>%
      as_gt() %>% 
      tab_header(
        title = "",
        subtitle = tt()
      ) %>% 
      tab_options(table.border.bottom.style = 'none')
  })
  
  output$kap1 <- renderPlotly({
    if (input$hospital != "All") {
      df <- df_kap1 %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_kap1 %>% filter(province == input$province)
    } else {
      df <- df_kap1
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    df %>%
      mutate(
        kap = factor(
          kap,
          levels = c(
            's3604',
            's3615',
            's3616',
            's3617',
            's3618',
            's3619',
            's3620'
          ),
          labels = c(
            'Only sick people with symptoms can spread the disease',
            'Sought care later than usual because of COVID-19',
            'Afraid of being quarantined after close contact with COVID-19 patient',
            'Afraid to seek care out of fear of being tested/isolated',
            'Always wearing mask in public is a good thing to do',
            'Always practicing social distancing is a good thing to do',
            'Patients should disclose their exposure and symptoms'
          )
        )
      ) %>%
      
      bar_scale(kap, color_scale1,tt())
  })

  output$kap1a <- renderPlotly({
    if (input$hospital != "All") {
      df <- df_kap1 %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_kap1 %>% filter(province == input$province)
    } else {
      df <- df_kap1
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    df %>%
      mutate(
        kap = factor(
          paste0(kap, variant),
          levels = c(
            's3604Jun-Dec 2021',
            's3604Jan 2022 - May 2023',
            's3615Jun-Dec 2021',
            's3615Jan 2022 - May 2023',
            's3616Jun-Dec 2021',
            's3616Jan 2022 - May 2023',
            's3617Jun-Dec 2021',
            's3617Jan 2022 - May 2023',
            's3618Jun-Dec 2021',
            's3618Jan 2022 - May 2023',
            's3619Jun-Dec 2021',
            's3619Jan 2022 - May 2023',
            's3620Jun-Dec 2021',
            's3620Jan 2022 - May 2023'
          ),
          labels = c(
            'Only sick people with symptoms can spread the disease<br>Jun-Dec 2021',
            'Only sick people with symptoms can spread the disease<br>Jan 2022 - May 2023',
            'Sought care later than usual because of COVID-19<br>Jun-Dec 2021',
            'Sought care later than usual because of COVID-19<br>Jan 2022 - May 2023',
            'Afraid of being quarantined after close contact with COVID-19 patient<br>Jun-Dec 2021',
            'Afraid of being quarantined after close contact with COVID-19 patient<br>Jan 2022 - May 2023',
            'Afraid to seek care out of fear of being tested/isolated<br>Jun-Dec 2021',
            'Afraid to seek care out of fear of being tested/isolated<br>Jan 2022 - May 2023',
            'Always wearing mask in public is a good thing to do<br>Jun-Dec 2021',
            'Always wearing mask in public is a good thing to do<br>Jan 2022 - May 2023',
            'Always practicing social distancing is a good thing to do<br>Jun-Dec 2021',
            'Always practicing social distancing is a good thing to do<br>Jan 2022 - May 2023',
            'Patients should disclose their exposure and symptoms<br>Jun-Dec 2021',
            'Patients should disclose their exposure and symptoms<br>Jan 2022 - May 2023'
          )
        )
      ) %>%
      bar_scale(kap, color_scale1,tt())
  })
  
  output$kap2 <- renderPlotly({
    if (input$hospital != "All") {
      df <- df_kap2 %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_kap2 %>% filter(province == input$province)
    } else {
      df <- df_kap2
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    df %>%
      mutate(
        kap = factor(
          kap,
          levels = c(
            's3610',
            's3613',
            's3621',
            's3622'
          ),
          labels = c(
            'Did you wear a mask at home?',
            'Did you wear a mask when you went in crowded areas?',
            'Do you practice social distancing in your household?',
            'Do you practice social distancing outside of your residence?'
          )
        )
      ) %>%
      
      bar_scale(kap, color_scale2,tt())
  })
  
  output$kap2a <- renderPlotly({
    if (input$hospital != "All") {
      df <- df_kap2 %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_kap2 %>% filter(province == input$province)
    } else {
      df <- df_kap2
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    df %>%
      mutate(
        kap = factor(
          paste0(kap, variant),
          levels = c(
            's3610Jun-Dec 2021',
            's3610Jan 2022 - May 2023',
            's3613Jun-Dec 2021',
            's3613Jan 2022 - May 2023',
            's3621Jun-Dec 2021',
            's3621Jan 2022 - May 2023',
            's3622Jun-Dec 2021',
            's3622Jan 2022 - May 2023'
          ),
          labels = c(
            'Did you wear a mask at home?<br>Jun-Dec 2021',
            'Did you wear a mask at home?<br>Jan 2022 - May 2023',
            'Did you wear a mask when you went in crowded areas?<br>Jun-Dec 2021',
            'Did you wear a mask when you went in crowded areas?<br>Jan 2022 - May 2023',
            'Do you practice social distancing in your household?<br>Jun-Dec 2021',
            'Do you practice social distancing in your household?<br>Jan 2022 - May 2023',
            'Do you practice social distancing outside of your residence?<br>Jun-Dec 2021',
            'Do you practice social distancing outside of your residence?<br>Jan 2022 - May 2023'
          )
        )
      ) %>% 
      bar_scale(kap, color_scale2,tt())
  })
  
  output$kap3 <- render_gt({
    if (input$hospital != "All") {
      df <- df_kap3 %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_kap3 %>% filter(province == input$province)
    } else {
      df <- df_kap3
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    df %>% select(s3604:s3620, variant)
    tbl_summary(
      data = df,
      include = c(s3604:s3620),
      by = variant,
      digits = list(all_categorical() ~ c(0, 1)),
      missing = 'no'
    ) %>%
      bold_labels() %>%
      add_p() %>%
      bold_p() %>%
      modify_header(update = list(label = "",
                                  all_stat_cols() ~ "**{level}**<br>N = {n}")) %>%
      as_gt() %>%
      tab_header(
        title = "",
        subtitle = tt()
      ) %>% 
      tab_options(table.border.bottom.style = 'none')
  })
  
  output$kap4 <- render_gt({
    if (input$hospital != "All") {
      df <- df_kap4 %>% filter(hospital == input$hospital)
    } else if (input$province != "All") {
      df <- df_kap4 %>% filter(province == input$province)
    } else {
      df <- df_kap4
    }
    if (input$rps == "Yes") {
      df <- df %>% filter(rps == TRUE)
    } else if (input$rps == "No") {
      df <- df %>% filter(rps == FALSE)
    }
    df %>% select(s3610:s3622, variant)
    tbl_summary(
      data = df,
      include = c(s3610:s3622),
      by = variant,
      digits = list(all_categorical() ~ c(0, 1)),
      missing = 'no'
    ) %>%
      bold_labels() %>%
      add_p() %>%
      bold_p() %>%
      modify_header(update = list(label = "",
                                  all_stat_cols() ~ "**{level}**<br>N = {n}")) %>%
      as_gt() %>%
      tab_header(
        title = "",
        subtitle = tt()
      ) %>% 
      tab_options(table.border.bottom.style = 'none')
  })
  
}
