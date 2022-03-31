################################################################################
# global.R
################################################################################

##### Load packages #####
library(tidyverse)
library(lubridate)
library(sf)
library(ggsci)

shapefile_folder <- "./Maps/"
source('./function.R')

#############
# Data Load #
#############
githubURL <- "https://github.com/NartladaC/CovidFeverData/raw/main/Data/CFDashboard.RData"
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
sea          <- world %>%  filter(ISO %in% c('KH','MM','LA','MY','VN'))
tak_district <- th_district %>% filter(ProvID == 63)
np_district  <- th_district %>% filter(ProvID == 48)


cf_province  <- th_province %>% filter(ProvNum %in% c(48, 63))
cf_tak       <- tak_district %>% filter(AmphoeID %in% c("05", "06", "08"))
cf_np        <- np_district %>% filter(AmphoeID %in% c("01", "05", "08"))

map_cf_province <- ggplot() +
  geom_sf(data = sea,
          fill = "gray95",
          size = 0.5) +
  geom_sf(data = th_province,
          fill = "grey85",
          size = 0.5) +
  geom_sf(data = cf_province,
          fill = "red",
          alpha = 0.5) +
  geom_sf_text(data = sea, 
               aes(label = COUNTRY),
               size = 4) +
  geom_sf_text(data = cf_province, 
               aes(label = ProvName),
               size = 4) +
  coord_sf(xlim = c(95.2, 106),
           ylim = c(5, 20),
           expand = TRUE) + 
  theme_void()

map_cf_tak <- ggplot() +
  geom_sf(data = tak_district, 
          fill = "grey95") +
  geom_sf(data = cf_tak, aes(fill = AmphoeE)) +
  geom_sf_text(data = cf_tak,
            aes(label = AmphoeE),
            size = 4) +
  coord_sf(xlim = c(374000, 541000),
           ylim = c(1688000, 1965000)) +
  scale_fill_manual(values = c("yellow", "pink", "deepskyblue")) +
  theme_void() +
  theme(legend.position = "none")

map_cf_np <- ggplot() +
  geom_sf(data = np_district, 
          fill = "grey95") +
  geom_sf(data = cf_np, aes(fill = AmphoeE)) +
  geom_sf_text(data = cf_np,
            aes(label = AmphoeE),
            size = 4) +
  coord_sf(xlim = c(1026500, 1123500),
           ylim = c(1870000, 1995000)) +
  scale_fill_manual(values = c("orange", "mediumpurple1", "darkseagreen3")) +
  theme_void() +
  theme(legend.position = "none")
