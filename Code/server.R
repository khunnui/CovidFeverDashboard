################################################################################
# server.R
################################################################################

library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)

library(ggpubr)

server <- function(input, output, session) {

  output$map <- renderPlot({
    ggarrange(map_cf_tak, map_cf_province, map_cf_np, ncol = 3)
  })

}
