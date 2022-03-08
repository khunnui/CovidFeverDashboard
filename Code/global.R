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
