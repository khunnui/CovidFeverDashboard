################################################################################
# server.R
################################################################################

library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)
library(cowplot)

server <- function(input, output, session) {

  df <- reactive({
    if (input$Hospital != "All") {
      df <- tblSection1 %>% filter(S1HospitalID == input$Hospital)
    } else if (input$Province != "All") {
      df <- tblSection1 %>% filter(Province == input$Province)
    } else {
      df <- tblSection1
    }
  })
  
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
        choices = c("All", as.character(unique(tblSection1$S1HospitalID)))
      )
    } else {
      updateSelectInput(
        inputId = "Hospital",
        choices = c("All", as.character(unique(tblSection1$S1HospitalID[tblSection1$Province == input$Province])))
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
    plot <- ggplot(df(), aes(x = floor_date(S1ScreenDate, "month"),
                             text = after_stat(paste("Count: ", count)))) +
      geom_bar(
        col = "white",
        fill = "steelblue",
        width = 16,
        alpha = 0.5
      ) +
      labs(
        title = tt(),
        x = "Screening Date",
        y = "Number Screened"
      ) +
      annotate(geom = "text",
               label = paste0("Total screening = ", format(nrow(df()), big.mark = ",")),
               x = min(df()$S1ScreenDate, na.rm = TRUE),
               y = 900,
               hjust = 0) +
      scale_x_date(
        breaks = "1 month", # Place X axis tick mark at every month
        date_labels = "%b %y"
      ) +
      scale_y_continuous(
        limits = c(0, 1000),
        expand = c(0, 0)
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 0.75)
      )
    # ggplotly to convert ggplot object to plotly object
    ggplotly(plot, tooltip = "text")
  })
  
  output$ScreeningGender <- renderPlot({
    pie(df(), S1Gender) +
    labs(title = tt()) +
    theme(plot.title = element_text(hjust = 0.5))
  })

  output$ScreeningAge <- renderDT({
    df() %>%
      # group_by(S1HospitalID) %>%
      summarize(
        n = n(),
        min = min(S1Age_Year, na.rm = TRUE),
        q1 = quantile(S1Age_Year, 0.25, na.rm = TRUE),
        median = median(S1Age_Year, na.rm = TRUE),
        mean = round(mean(S1Age_Year, na.rm = TRUE), 1),
        q3 = quantile(S1Age_Year, 0.75, na.rm = TRUE),
        max = max(S1Age_Year, na.rm = TRUE)
      ) %>%
      datatable(
        caption = htmltools::tags$caption(style = "caption-side: top; text-align: center; color: black;", 
                                          tt()),
        rownames = FALSE,
        # colnames = c('Hospital' = 'S1HospitalID'),
        options = list(
        initComplete = JS("function(){$(this).addClass('compact');}"),
        dom = 'rt'
      ))
  })
  
  # ggplot(tblSection1 %>%  filter(!is.na(S1Gender)), aes(S1Age_Year, S1Gender)) +
  #   geom_boxplot() +
  #   coord_flip() +
  #   theme_classic()
  
  output$ScreeningEnrol <- renderPlot({
    df() %>%
      group_by(OLDCF, CF_Enrol) %>%
      tally() %>%
      ggplot(aes(x = OLDCF, y = n, fill = CF_Enrol)) +
      geom_bar(position = 'dodge', 
               stat = 'identity',
               width = 0.5,
               alpha = 0.5) +
      geom_text(aes(label = n),
                position = position_dodge(width = 0.5),
                vjust = -0.5) +
      scale_y_continuous(
        limits = c(0, 7000),
        expand = c(0, 0)
      ) +
      scale_fill_lancet() +
      labs(
        title = tt(),
        x = 'Previously enrolled',
        y = 'Count',
        fill = 'Enrolled') +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "bottom")
  })

  }
