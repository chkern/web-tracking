library(pacman)
p_load(tidyverse, foreign, haven, data.table, dummies)

setwd("Y:\\Respondi\\RESPONDI_w3\\")  

wp_prep_sm <- readRDS(file = ".\\data\\wep_pageviews_prepared_small.RDS")
mv_prep_sm <- readRDS(file = ".\\data\\mobile_views_prepared_small.RDS")

wp_names_sm <- names(wp_prep_sm)
mv_names_sm <- names(mv_prep_sm)

wp_names_sm <- paste0('wp_', wp_names_sm)
mv_names_sm <- paste0('mv_', mv_names_sm)

wp_prep_sm <- set_names(wp_prep_sm, nm = wp_names_sm)
mv_prep_sm <- set_names(mv_prep_sm, nm = mv_names_sm)

wp_prep_sm <- rename(wp_prep_sm, panelist_id = wp_panelist_id)
mv_prep_sm <- rename(mv_prep_sm, panelist_id = mv_panelist_id)

rm(mv_names_sm, wp_names_sm)

data_prep_sm <- merge(mv_prep_sm, wp_prep_sm, by="panelist_id", all = TRUE)
rm(mv_prep_sm, wp_prep_sm)

data_prep_sm[is.na(data_prep_sm)] <- 0

wp_prep <- readRDS(file = ".\\data\\wep_pageviews_prepared.RDS")
mv_prep <- readRDS(file = ".\\data\\mobile_views_prepared.RDS")

wp_names <- names(wp_prep)
mv_names <- names(mv_prep)

wp_names <- paste0('wp_', wp_names)
mv_names <- paste0('mv_', mv_names)

wp_prep <- set_names(wp_prep, nm = wp_names)
mv_prep <- set_names(mv_prep, nm = mv_names)

wp_prep <- rename(wp_prep, panelist_id = wp_panelist_id)
mv_prep <- rename(mv_prep, panelist_id = mv_panelist_id)

rm(mv_names, wp_names)

data_prep <- merge(mv_prep, wp_prep, by="panelist_id", all = TRUE)
rm(mv_prep, wp_prep)

data_prep[is.na(data_prep)] <- 0


# drop one nonsense observation
data_prep_sm <- filter(data_prep_sm, panelist_id!=0)
# drop some duplicate variables
data_prep_sm <- select(data_prep_sm, -c(mv_kind_tab_rel_dur, mv_kind_tab_rel_n, mv_kind_totdur_p_user, mv_kind_totn_p,
                                        mv_utype_wo_rel_dur, mv_utype_wo_rel_n, mv_utype_totdur_p_user, mv_utype_totn_p,
                                        mv_con_totdur_p_user, mv_con_totn_p, mv_scheme_tot_n, mv_scheme_tot_dur))


saveRDS(data_prep, file = ".\\data\\apps_and_sites_final.rds")
saveRDS(data_prep_sm, file = ".\\data\\general_usage_info_final.rds")

rm(data_prep,data_prep_sm)

#load apps classified and extend to all people
all_Rs <- readRDS(file = ".\\data\\general_usage_info_final.rds")
all_Rs <- select(all_Rs, panelist_id)
all_Rs$IDTHING <- 1

apps_classified <- readRDS(file = ".\\data\\apps_classified.rds")

apps_classified <- merge(apps_classified, all_Rs, by = "panelist_id", all = TRUE)
apps_classified$IDTHING <- NULL

saveRDS(apps_classified, file = ".\\data\\apps_classified_final.rds")

##load news media data
mv_app_news <- readRDS(file = ".\\data\\mv_app_news_media_final.RDS")
mv_dom_news <- readRDS(file = ".\\data\\mv_dom_news_media_final.RDS")
wp_dom_news <- readRDS(file = ".\\data\\wp_dom_news_media_final.RDS")

mv_app_news <- merge(mv_app_news, all_Rs, by = "panelist_id", all = TRUE)
mv_app_news$IDTHING <- NULL
mv_dom_news <- merge(mv_dom_news, all_Rs, by = "panelist_id", all = TRUE)
mv_dom_news$IDTHING <- NULL
wp_dom_news <- merge(wp_dom_news, all_Rs, by = "panelist_id", all = TRUE)
wp_dom_news$IDTHING <- NULL

wp_dom_news_nam <- names(wp_dom_news)
mv_dom_news_nam <- names(mv_dom_news)
mv_app_news_nam <- names(mv_app_news)

#wv_names <- paste0('wv_', wv_names)
wp_dom_news_nam <- paste0('wp_', wp_dom_news_nam)
mv_dom_news_nam <- paste0('mv_', mv_dom_news_nam)
mv_app_news_nam <- paste0('mv_', mv_app_news_nam)

#wv_prep <- set_names(wv_prep, nm = wv_names)
wp_dom_news <- set_names(wp_dom_news, nm = wp_dom_news_nam)
mv_dom_news <- set_names(mv_dom_news, nm = mv_dom_news_nam)
mv_app_news <- set_names(mv_app_news, nm = mv_app_news_nam)


#wv_prep <- rename(wv_prep, panelist_id = wv_panelist_id)
wp_dom_news <- rename(wp_dom_news, panelist_id = wp_panelist_id)
mv_dom_news <- rename(mv_dom_news, panelist_id = mv_panelist_id)
mv_app_news <- rename(mv_app_news, panelist_id = mv_panelist_id)

rm(wp_dom_news_nam, mv_dom_news_nam, mv_app_news_nam)

news_media <- merge(wp_dom_news, mv_dom_news, all=TRUE)
news_media <- merge(news_media, mv_app_news, all=TRUE)

rm(wp_dom_news,mv_dom_news,mv_app_news)

saveRDS(news_media, file = ".\\data\\news_media_final.rds")

##load oeffentlich rechtlich data
OER_data <- readRDS(file = "./data/OER_final.RDS")
OER_data <- merge(OER_data, all_Rs, by = "panelist_id", all = TRUE)
OER_data[is.na(OER_data)] <- 0
OER_data$IDTHING <- NULL

OER_data <- OER_data %>% 
  select(-c(mv_app_tot_d, mv_app_tot_n,mv_dom_tot_n,mv_dom_tot_d,wp_tot_d,wp_tot_n,
            mv_app_oer_app,wp_oeff_recht,mv_dom_oeff_recht))

saveRDS(OER_data, file = ".\\data\\oeffrecht_final.rds")


FAKE <- readRDS(file = ".\\data\\fake_final.rds")

