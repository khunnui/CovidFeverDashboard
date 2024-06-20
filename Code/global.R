################################################################################
# global.R
################################################################################

##### Load packages #####
library(tidyverse)
library(lubridate)
library(ggsci)

source('./function.R')

#############
# Data Load #
#############
githubURL <- "https://github.com/khunnui/CovidFeverData/raw/main/Data/CFDashboard.RData"
#load(url(githubURL))
load('C:/COVIDDashboard/CovidFeverData/Data/CFDashboard.RData')

##########
# Colors #
##########
color_scr <- '#a1caf1'
color_age <- '#a1caf1'
color_gender <- c('#a1caf1', '#d8bfd8', '#ebe6e5')
color_eli <- '#a1caf1'
color_enr <- '#ace1af'
color_pos <- '#b78f62'
color_qual <- c('#ffcba4',
                '#b78f62',
                '#f8de7e',
                '#d8bfd8',
                '#a1caf1',
                '#ace1af',
                '#ebe6e5')
color_posneg <- c('#b78f62','#a1caf1','#ebe6e5')
color_atk    <- c('#b78f62','#ace1af','#ebe6e5')
color_scale1 <- c('#81613c','#b78f62','#ebe6e5','#a1caf1','#1a67af')
color_scale2 <- c('#e0edfa','#c0dcf6','#a1caf1','#5e99d0','#1a67af')
