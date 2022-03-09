################################################################################
# global.R
################################################################################

##### Load packages #####
library(tidyverse)
library(lubridate)
library(sf)

shapefile_folder <- "../Maps/"

#############
# Data Load #
#############
githubURL <- "https://github.com/NartladaC/CovidFeverData/raw/main/Data/CFMast.Rdata"
load(url(githubURL))

###############
# Study sites #
###############
world        <- st_read(paste0(shapefile_folder, "World_Countries.shp"),
                        stringsAsFactors = FALSE)
th_province  <- st_read(paste0(shapefile_folder, "ThailandProvince.shp"),
                        stringsAsFactors = FALSE)
th_district  <- st_read(paste0(shapefile_folder, "Amphoe77.shp"), 
                        stringsAsFactors = FALSE)
tak_district <- th_district %>% filter(ProvID == 63)
np_district  <- th_district %>% filter(ProvID == 48)

cf_province  <- th_province %>% filter(ProvNum %in% c(48, 63))
cf_tak       <- tak_district %>% filter(AmphoeID %in% c("05", "06", "08"))
cf_np        <- np_district %>% filter(AmphoeID %in% c("01", "05", "08"))

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

###############
# Screening   #
###############
