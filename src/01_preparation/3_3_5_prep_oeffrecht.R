library(pacman)
p_load(tidyverse, foreign, haven, data.table, dummies, caret, magrittr, randomForest,doParallel, snow)

##################################################################################
##Some information and descriptive stuff regarding Sinus Milieus
##################################################################################

setwd("Y:\\Respondi\\RESPONDI_w3\\")

oeffrecht_sites <- read.csv(file=".\\media_landscape\\Oeffrecht_sites.csv", header=TRUE, sep = ";")
oeffrecht_apps <- read.csv(file=".\\media_landscape\\Oeffrecht_apps.csv", header=TRUE, sep = ",")

#drop unnecessary characters from domains
oeffrecht_sites$pages_oeffrecht <- gsub("/","",oeffrecht_sites$pages_oeffrecht)
#gen identifier that will tell us after merge if news media domain
oeffrecht_sites$oeff_recht <- 1

mobile_views <- readRDS(file = "./original daten/mobile_views.rds")

MV_apps <- mobile_views %>%
  filter(!(is.na(app_n)))

rm(mobile_views)


v1 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[1], MV_apps$app_n)]))
v1[,1] <- as.character(v1[,1])
names(v1)[1]<-"app_name"

v2 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[2], MV_apps$app_n)]))
v2[,1] <- as.character(v2[,1])
names(v2)[1]<-"app_name"

v3 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[3], MV_apps$app_n)]))
rm(v3)

v4 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[4], MV_apps$app_n)]))
v4[,1] <- as.character(v4[,1])
names(v4)[1]<-"app_name"

v5 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[5], MV_apps$app_n)]))
v5[,1] <- as.character(v5[,1])
v5 <- as.data.frame(v5[-c(2, 3,5, 7), ])
names(v5)[1]<-"app_name"

v6 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[6], MV_apps$app_n)]))
v6[,1] <- as.character(v6[,1])
v6 <- as.data.frame(v6[-c(1, 3), ])
names(v6)[1]<-"app_name"

v7 <- as.data.frame(unique(MV_apps$app_n[grep(tolower(oeffrecht_apps$apps[7]), tolower(MV_apps$app_n))]))
rm(v7)

v8 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[8], MV_apps$app_n)]))
v8[,1] <- as.character(v8[,1])
names(v8)[1]<-"app_name"

v9 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[9], MV_apps$app_n)]))
rm(v9)

v10 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[10], MV_apps$app_n)]))
v10[,1] <- as.character(v10[,1])
names(v10)[1]<-"app_name"

v11 <- as.data.frame(unique(MV_apps$app_n[grep(tolower(oeffrecht_apps$apps[11]), tolower(MV_apps$app_n))]))
rm(v11)

v12 <- as.data.frame(unique(MV_apps$app_n[grep(tolower(oeffrecht_apps$apps[12]), tolower(MV_apps$app_n))]))
rm(v12)

v13 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[13], MV_apps$app_n)]))
v13[,1] <- as.character(v13[,1])
names(v13)[1]<-"app_name"

v14 <- as.data.frame(unique(MV_apps$app_n[grep(tolower(oeffrecht_apps$apps[14]), tolower(MV_apps$app_n))]))
rm(v14)

v15 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[15], MV_apps$app_n)]))
rm(v15)

v16 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[16], MV_apps$app_n)]))
v16[,1] <- as.character(v16[,1])
names(v16)[1]<-"app_name"
rm(v16)

v17 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[17], MV_apps$app_n)]))
rm(v17)

v18 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[18], MV_apps$app_n)]))
rm(v18)

v19 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[19], MV_apps$app_n)]))
rm(v19)

v20 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[20], MV_apps$app_n)]))
v20[,1] <- as.character(v20[,1])
names(v20)[1]<-"app_name"

v21 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[21], MV_apps$app_n)]))
rm(v21)

v22 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[22], MV_apps$app_n)]))
rm(v22)

v23 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[23], MV_apps$app_n)]))
v23[,1] <- as.character(v23[,1])
names(v23)[1]<-"app_name"

v24 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[24], MV_apps$app_n)]))
rm(v24)

v25 <- as.data.frame(unique(MV_apps$app_n[grep(tolower(oeffrecht_apps$apps[25]), tolower(MV_apps$app_n))]))
rm(v25)

v26 <- as.data.frame(unique(MV_apps$app_n[grep(tolower(oeffrecht_apps$apps[26]), tolower(MV_apps$app_n))]))
rm(v26)

v27 <- as.data.frame(unique(MV_apps$app_n[grep(tolower(oeffrecht_apps$apps[27]), tolower(MV_apps$app_n))]))
rm(v27)

v28 <- as.data.frame(unique(MV_apps$app_n[grep(tolower(oeffrecht_apps$apps[28]), tolower(MV_apps$app_n))]))
v28[,1] <- as.character(v28[,1])
v28 <- as.data.frame(v28[-c(3,4,12), ])
names(v28)[1]<-"app_name"

v29 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[29], MV_apps$app_n)]))
rm(v29)

v30 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[30], MV_apps$app_n)]))
rm(v30)

v31 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[31], MV_apps$app_n)]))
v31[,1] <- as.character(v31[,1])
names(v31)[1]<-"app_name"

v32 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[32], MV_apps$app_n)]))
v32[,1] <- as.character(v32[,1])
names(v32)[1]<-"app_name"

v33 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[33], MV_apps$app_n)]))
v33[,1] <- as.character(v33[,1])
names(v33)[1]<-"app_name"

v34 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[34], MV_apps$app_n)]))
v34[,1] <- as.character(v34[,1])
names(v34)[1]<-"app_name"

v35 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[35], MV_apps$app_n)]))
rm(v35)

v36 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[36], MV_apps$app_n)]))
rm(v36)

v37 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[37], MV_apps$app_n)]))
v37[,1] <- as.character(v37[,1])
names(v37)[1]<-"app_name"

v38 <- as.data.frame(unique(MV_apps$app_n[grep(tolower(oeffrecht_apps$apps[38]), tolower(MV_apps$app_n))]))
rm(v38)

v39 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[39], MV_apps$app_n)]))
rm(v39)

v40 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[40], MV_apps$app_n)]))
rm(v40)

v41 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[41], MV_apps$app_n)]))
rm(v41)

v42 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[42], MV_apps$app_n)]))
rm(v42)

v43 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[43], MV_apps$app_n)]))
rm(v43)

v44 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[44], MV_apps$app_n)]))
rm(v44)

v45 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[45], MV_apps$app_n)]))
v45[,1] <- as.character(v45[,1])
v45 <- as.data.frame(v45[-c(2,7), ])
names(v45)[1]<-"app_name"

v46 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[46], MV_apps$app_n)]))
v46[,1] <- as.character(v46[,1])
names(v46)[1]<-"app_name"

v47 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[47], MV_apps$app_n)]))
rm(v47)

v48 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[48], MV_apps$app_n)]))
v48[,1] <- as.character(v48[,1])
names(v48)[1]<-"app_name"

v49 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[49], MV_apps$app_n)]))
v49[,1] <- as.character(v49[,1])
names(v49)[1]<-"app_name"

v50 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[50], MV_apps$app_n)]))
v50[,1] <- as.character(v50[,1])
names(v50)[1]<-"app_name"

v51 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[51], MV_apps$app_n)]))
v51[,1] <- as.character(v51[,1])
names(v51)[1]<-"app_name"

v52 <- as.data.frame(unique(MV_apps$app_n[grep(oeffrecht_apps$apps[52], MV_apps$app_n)]))
v52[,1] <- as.character(v52[,1])
names(v52)[1]<-"app_name"

OER_apps <- rbind(v1,v10,v13,v2,v20,v23,v28,v31,v32,v33,v34,v37,v4,v45,v46,v48,v49,
                  v5,v50,v51,v52,v6,v8)
names(OER_apps)[1]<-"app_n"

OER_apps$oer_app <- 1



rm(oeffrecht_apps)
rm(MV_apps, v1,v10,v13,v2,v20,v23,v28,v31,v32,v33,v34,v37,v4,v45,v46,v48,v49,
   v5,v50,v51,v52,v6,v8)


mobile_views <- readRDS(file = "./original daten/mobile_views.rds")

###MERGE TO MOBILE VIEWS
mobile_views <- merge(mobile_views, OER_apps, by="app_n", all=TRUE)
mobile_views$oer_app[is.na(mobile_views$oer_app)] <- 0
rm(OER_apps)
##############################
oeffrecht_sites <- rename(oeffrecht_sites,  domain = pages_oeffrecht )
mobile_views <- merge(mobile_views, oeffrecht_sites, by="domain", all=TRUE)
mobile_views$oeff_recht[is.na(mobile_views$oeff_recht)] <- 0

DOMAINS <- mobile_views %>%
  filter(is.na(app_n)) %>%
  filter(is.na(app_os)) %>% 
  select(domain, panelist_id, duration, oeff_recht)


d_totals <- DOMAINS %>%
  group_by(panelist_id) %>% 
  summarise(
    tot_n = n(),
    tot_d = sum(duration, na.rm = TRUE)
  )

oer_share2 <- DOMAINS %>% 
  group_by(panelist_id, oeff_recht) %>%
  summarise(
    oer_tot_n = n(),
    oer_tot_d = sum(duration, na.rm = TRUE))  

oer_share2 <- merge(oer_share2, d_totals, by="panelist_id")

oer_share2 <- oer_share2 %>%
  mutate(
    oer_rel_n = oer_tot_n/tot_n,
    oer_rel_d = oer_tot_d/tot_d
  ) %>% 
  filter(oeff_recht==1)

APPS <- mobile_views %>%
  filter(!(is.na(app_n)))

a_totals <- APPS %>%
  group_by(panelist_id) %>% 
  summarise(
    tot_n = n(),
    tot_d = sum(duration, na.rm = TRUE)
  )

A_oer_share2 <- APPS %>% 
  group_by(panelist_id, oer_app) %>%
  summarise(
    oer_tot_n = n(),
    oer_tot_d = sum(duration, na.rm = TRUE))  

A_oer_share2 <- merge(A_oer_share2, a_totals, by="panelist_id")

A_oer_share2 <- A_oer_share2 %>%
  mutate(
    oer_rel_n = oer_tot_n/tot_n,
    oer_rel_d = oer_tot_d/tot_d
  ) %>% 
  filter(oer_app==1)

rm(APPS, DOMAINS)

APP_names <- names(A_oer_share2)
DOM_names <- names(oer_share2)

APP_names <- paste0('app_', APP_names)
DOM_names <- paste0('dom_', DOM_names)

A_oer_share2 <- set_names(A_oer_share2, nm = APP_names)
oer_share2 <- set_names(oer_share2, nm = DOM_names)

A_oer_share2 <- rename(A_oer_share2, panelist_id = app_panelist_id)
oer_share2 <- rename(oer_share2, panelist_id = dom_panelist_id)

rm(APP_names, DOM_names)

mv_OER_data <- merge(A_oer_share2, oer_share2, by="panelist_id", all = TRUE)
rm(A_oer_share2, oer_share2, mobile_views)

#merge to tracking data ----WEB Pageviews
web_pageviews <- readRDS(file = ".\\data\\web_pageviews_prep.rds")
length(unique(web_pageviews$panelist_id))

web_pageviews <- merge(web_pageviews, oeffrecht_sites, by="domain", all=TRUE)
web_pageviews$oeff_recht[is.na(web_pageviews$oeff_recht)] <- 0

wp_totals <- web_pageviews %>%
  group_by(panelist_id) %>% 
  summarise(
    tot_n = n(),
    tot_d = sum(active_seconds, na.rm = TRUE)
  )

wp_oer_share2 <- web_pageviews %>% 
  group_by(panelist_id, oeff_recht) %>%
  summarise(
    oer_tot_n = n(),
    oer_tot_d = sum(active_seconds, na.rm = TRUE))  

wp_oer_share2 <- merge(wp_oer_share2, wp_totals, by="panelist_id")

wp_oer_share2 <- wp_oer_share2 %>%
  mutate(
    oer_rel_n = oer_tot_n/tot_n,
    oer_rel_d = oer_tot_d/tot_d
  ) %>% 
  filter(oeff_recht==1)

wp_OER_data <- merge(wp_totals, wp_oer_share2, by=c("panelist_id", "tot_n", "tot_d"), all = TRUE)
# mv_OER_data <- merge(totals, mv_oer_share2, by=c("panelist_id"), all.x = TRUE)
rm(oeffrecht_sites, web_pageviews, wp_oer_share2, a_totals, d_totals, wp_totals)

wp_names_sm <- names(wp_OER_data)
mv_names_sm <- names(mv_OER_data)

wp_names_sm <- paste0('wp_', wp_names_sm)
mv_names_sm <- paste0('mv_', mv_names_sm)

wp_OER_data <- set_names(wp_OER_data, nm = wp_names_sm)
mv_OER_data <- set_names(mv_OER_data, nm = mv_names_sm)

wp_OER_data <- rename(wp_OER_data, panelist_id = wp_panelist_id)
mv_OER_data <- rename(mv_OER_data, panelist_id = mv_panelist_id)

rm(mv_names_sm, wp_names_sm)

oeff_recht <- merge(mv_OER_data, wp_OER_data, by="panelist_id", all = TRUE)
rm(mv_OER_data, wp_OER_data)

oeff_recht[is.na(oeff_recht)] <- 0

saveRDS(oeff_recht, file = "./data/OER_final.RDS")
