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
    df %>%
      group_by(S1Gender) %>% # Group by specified column
      summarise(count = sum(n)) %>% # Number of observations in each group
      plot_ly(
        labels = ~ S1Gender,
        values = ~ count,
        type = 'pie',
        texttemplate = "%{value:,d}<br>(%{percent:.1%})",
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)),
        hovertemplate = "%{value:,d}<br>(%{percent:.1%})"
      ) %>%
      layout(title = tt(),
             legend = list(x = 100, y = 0.5))
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
    df %>%
      group_by(agegroup) %>% # Group by specified column
      summarise(count = sum(n)) %>% # Number of observations in each group
      plot_ly(
        labels = ~ agegroup,
        values = ~ count,
        type = 'pie',
        texttemplate = "%{percent:.1%}",
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)),
        hovertemplate = "%{percent:.1%}"
      ) %>% 
      layout(title = tt(),
             margin = list(l = 5, r = 5),
             legend = list(orientation = "h",   # show entries horizontally
                           xanchor = "center",  # use center of legend as anchor
                           x = 0.5))             # put legend in center of x-axis)  # use center of legend as anchor)
  })
  
  output$EnrollmentGender <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_enrgender %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_enrgender %>% filter(Province == input$Province)
    } else {
      df <- df_enrgender
    }
    df %>%
      group_by(S1Gender) %>% # Group by specified column
      summarise(count = sum(n)) %>% # Number of observations in each group
      plot_ly(
        labels = ~ S1Gender,
        values = ~ count,
        type = 'pie',
        texttemplate = "%{percent:.1%}",
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)),
        hovertemplate = "%{percent:.1%}"
      ) %>% 
      layout(title = tt(),
             margin = list(l = 5, r = 5),
             legend = list(orientation = "h",   # show entries horizontally
                           xanchor = "center",  # use center of legend as anchor
                           x = 0.5))             # put legend in center of x-axis)  # use center of legend as anchor)
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
      group_by(S34Occupation) %>% 
      summarise(countgroup = sum(count)) %>% 
      plot_ly(
        labels = ~ S34Occupation,
        values = ~ countgroup,
        type = 'pie',
        texttemplate = "%{percent:.1%}",
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)),
        hovertemplate = "%{percent:.1%}"
      ) %>% 
      layout(title = tt(),
    margin = list(l = 5, r = 5),
    legend = list(orientation = "h",   # show entries horizontally
                  xanchor = "center",  # use center of legend as anchor
                  x = 0.5))             # put legend in center of x-axis)  # use center of legend as anchor)
  })
 
  output$VaccinePie1 <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_vac %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_vac %>% filter(Province == input$Province)
    } else {
      df <- df_vac
    }
    df %>%
      group_by(S33CovidVaccine) %>% # Group by specified column
      summarise(count = sum(n)) %>% # Number of observations in each group
      plot_ly(
        labels = ~ S33CovidVaccine,
        values = ~ count,
        type = 'pie',
        texttemplate = "%{value:,d}<br>(%{percent:.1%})",
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)),
        hovertemplate = "%{value:,d}<br>(%{percent:.1%})"
      ) %>%
      layout(title = tt(),
             legend = list(x = 100, y = 0.5))
  }) 
  
  output$VaccinePie2 <- renderPlotly({
    if (input$Hospital != "All") {
      df <- df_vac %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- df_vac %>% filter(Province == input$Province)
    } else {
      df <- df_vac
    }
    
      fig1 <- plot_ly(
        df %>%
          filter(S33CovidVaccine == "Vaccinated") %>% 
          group_by(FinalResult) %>% # Group by specified column
          summarise(count = sum(n)),  # Number of observations in each group
        labels = ~ FinalResult,
        values = ~ count,
        type = 'pie'
        #texttemplate = "%{value:,d}<br>(%{percent:.1%})",
      #  marker = list(colors = colors,
       #               line = list(color = '#FFFFFF', width = 1)),
        #hovertemplate = "%{value:,d}<br>(%{percent:.1%})"
      )
      
      fig2 <- plot_ly(
        df %>%
          filter(S33CovidVaccine == "Unvaccinated") %>% 
          group_by(FinalResult) %>% # Group by specified column
          summarise(count = sum(n)), # Number of observations in each group
          labels = ~ FinalResult,
        values = ~ count,
        type = 'pie'
        # texttemplate = "%{value:,d}<br>(%{percent:.1%})",
        # marker = list(colors = colors,
        #               line = list(color = '#FFFFFF', width = 1)),
        # hovertemplate = "%{value:,d}<br>(%{percent:.1%})"
      )
      
      subplot(fig2,fig1) %>% 
      layout(title = tt())
  })
}
