################################################################################
# server.R
################################################################################

library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)

library(ggpubr)

server <- function(input, output, session) {

  map_cf_province <- ggplot() +
    geom_sf(data = world,
            fill =  "grey95",
            size = 0.5) +
    geom_sf(data = th_province,
            fill = "#A6F7D9",
            color = "#90a3ad") +
    geom_sf(data = cf_province,
            fill = "red") +
    coord_sf(xlim = c(96.5, 106),
             ylim = c(6, 20),
             expand = TRUE) +
    theme_void()
  
  map_cf_tak <- ggplot() +
    geom_sf(data = tak_district, 
            fill = "white") +
    geom_sf(data = cf_tak, aes(fill = AmphoeE)) +
    scale_fill_manual(values = c("yellow", "pink", "green")) +
    theme_void() +
    theme(legend.position = "none")
  
  map_cf_np <- ggplot() +
    geom_sf(data = np_district, 
            fill = "white") +
    geom_sf(data = cf_np, aes(fill = AmphoeE)) +
    scale_fill_manual(values = c("orange", "blue", "purple")) +
    theme_void() +
    theme(legend.position = "none")
  
  output$map <- renderPlot({
    ggarrange(map_cf_tak, map_cf_province, map_cf_np, ncol = 3)
  })

}
