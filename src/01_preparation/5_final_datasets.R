library(pacman)
p_load(tidyverse, foreign, haven, data.table, dummies)

setwd("Y:\\Respondi\\RESPONDI_w3\\")  

survey_dat <- load(".\\survey_daten\\survey_data_all.RData")
rm(survey_dat)
apps_domains <- readRDS(file = ".\\data\\apps_and_sites_final.rds")

general_tracking <- readRDS(file = ".\\data\\general_usage_info_final.rds")

news_media <- readRDS(file = ".\\data\\news_media_final.rds")

oeffrecht <- readRDS(file = ".\\data\\oeffrecht_final.rds")

fakemedia <- readRDS(file = ".\\data\\fake_final.rds")