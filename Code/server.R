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
    if (input$Hospital != "All") {
      tt <- paste0(input$Province, " - ", input$Hospital)
    } else if (input$Province != "All") {
      tt <- input$Province
    } else {
      tt <- ""
    }
    plot <- ggplot(df(), aes(x = floor_date(S1ScreenDate, "month"),
                             text = after_stat(paste("Count: ", count)))) +
      geom_bar(
        col = "white",
        fill = "steelblue",
        width = 16,
        alpha = 0.5
      ) +
      labs(
        title = tt,
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
        axis.text.x = element_text(vjust = 0.75)
      )
    # ggplotly to convert ggplot object to plotly object
    ggplotly(plot, tooltip = "text")
  })
  
  output$ScreeningAgePie <- renderPlotly({
  })

}
