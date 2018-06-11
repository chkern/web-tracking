library(pacman)
p_load(tidyverse, foreign, haven, data.table, dummies)

setwd("Z:\\Respondi\\")

#wv_prep_sm <- readRDS(file = ".\\data\\web_visits_prepared_small.rds")
wp_prep_sm <- readRDS(file = ".\\data\\wep_pageviews_prepared_small.RDS")
mv_prep_sm <- readRDS(file = ".\\data\\mobile_views_prepared_small.RDS")

#wv_names_sm <- names(wv_prep_sm)
wp_names_sm <- names(wp_prep_sm)
mv_names_sm <- names(mv_prep_sm)

#wv_names_sm <- paste0('wv_', wv_names_sm)
wp_names_sm <- paste0('wp_', wp_names_sm)
mv_names_sm <- paste0('mv_', mv_names_sm)

#wv_prep_sm <- set_names(wv_prep_sm, nm = wv_names_sm)
wp_prep_sm <- set_names(wp_prep_sm, nm = wp_names_sm)
mv_prep_sm <- set_names(mv_prep_sm, nm = mv_names_sm)

#wv_prep_sm <- rename(wv_prep_sm, panelist_id = wv_panelist_id)
wp_prep_sm <- rename(wp_prep_sm, panelist_id = wp_panelist_id)
mv_prep_sm <- rename(mv_prep_sm, panelist_id = mv_panelist_id)

rm(mv_names_sm, wp_names_sm, wv_names_sm)

data_prep_sm <- merge(mv_prep_sm, wp_prep_sm, by="panelist_id", all = TRUE)
#data_prep_sm <- merge(data_prep_sm, wv_prep_sm, by="panelist_id", all = TRUE)
rm(mv_prep_sm, wp_prep_sm, wv_prep_sm)

data_prep_sm[is.na(data_prep_sm)] <- 0

saveRDS(data_prep_sm, file = ".\\data\\data_prep_final_small.rds")


survey_data <- load(".\\survey_daten\\survey_data_all.RData")


#wv_prep <- readRDS(file = ".\\data\\web_visits_prepared.rds")
wp_prep <- readRDS(file = ".\\data\\wep_pageviews_prepared.RDS")
mv_prep <- readRDS(file = ".\\data\\mobile_views_prepared.RDS")

#wv_names <- names(wv_prep)
wp_names <- names(wp_prep)
mv_names <- names(mv_prep)

#wv_names <- paste0('wv_', wv_names)
wp_names <- paste0('wp_', wp_names)
mv_names <- paste0('mv_', mv_names)

#wv_prep <- set_names(wv_prep, nm = wv_names)
wp_prep <- set_names(wp_prep, nm = wp_names)
mv_prep <- set_names(mv_prep, nm = mv_names)

#wv_prep <- rename(wv_prep, panelist_id = wv_panelist_id)
wp_prep <- rename(wp_prep, panelist_id = wp_panelist_id)
mv_prep <- rename(mv_prep, panelist_id = mv_panelist_id)

rm(mv_names, wp_names, wv_names)

data_prep <- merge(mv_prep, wp_prep, by="panelist_id", all = TRUE)
#data_prep <- merge(data_prep, wv_prep, by="panelist_id", all = TRUE)
rm(mv_prep, wp_prep, wv_prep)

data_prep[is.na(data_prep)] <- 0


saveRDS(data_prep, file = ".\\data\\data_prep_final.rds")

rm(data_prep)

#load apps classified and extend to all people
all_Rs <- readRDS(file = ".\\data\\data_prep_final_small.rds")
all_Rs <- select(all_Rs, panelist_id)
all_Rs$IDTHING <- 1

apps_classified <- readRDS(file = ".\\data\\apps_classified.rds")

apps_classified <- merge(apps_classified, all_Rs, by = "panelist_id", all = TRUE)
rm(all_Rs)
apps_classified$IDTHING <- NULL

saveRDS(apps_classified, file = ".\\data\\apps_classified_final.rds")
rm(apps_classified)
###load all news media stuff, merge and save as one file


