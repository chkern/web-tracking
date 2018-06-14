library(pacman)
p_load(tidyverse, foreign, haven, data.table, dummies, caret, magrittr, randomForest,doParallel, snow)

##################################################################################
##Some information and descriptive stuff regarding Sinus Milieus
##################################################################################

setwd("\\\\nas.uni-mannheim.de\\uni-shares\\swnsswml\\Respondi\\")

media_appnames <- read.csv(file=".\\media_landscape\\50mostusednewssites_apps_clean.csv", header=TRUE, sep = ";")
media_appnames$X <- NULL
media_appnames$App <- NULL
media_appnames$App2 <- NULL
media_appnames$App3 <- NULL


mobile_views <- readRDS(file = "./original daten/mobile_views.rds")

MV_apps <- mobile_views %>%
  filter(!(is.na(app_n)))
rm(mobile_views)


v1 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[1], MV_apps$app_n)]))
v1[,1] <- as.character(v1[,1])
names(v1)[1]<-"app_name"

v2 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[2], MV_apps$app_n)]))
v2[,1] <- as.character(v2[,1])
names(v2)[1]<-"app_name"

v3 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[3], MV_apps$app_n)]))
v3[,1] <- as.character(v3[,1])
names(v3)[1]<-"app_name"

v4 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[4], MV_apps$app_n)]))
v4[,1] <- as.character(v4[,1])
names(v4)[1]<-"app_name"

v5 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[5], MV_apps$app_n)]))
rm(v5)

v6 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[6], MV_apps$app_n)]))
v6[,1] <- as.character(v6[,1])
names(v6)[1]<-"app_name"

v7 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[7], MV_apps$app_n)]))
v7[,1] <- as.character(v7[,1])
names(v7)[1]<-"app_name"

v8 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[8], MV_apps$app_n)]))
v8[,1] <- as.character(v8[,1])
names(v8)[1]<-"app_name"

v9 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[9], MV_apps$app_n)]))
v9[,1] <- as.character(v9[,1])
names(v9)[1]<-"app_name"

v10 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[10], MV_apps$app_n)]))
v10[,1] <- as.character(v10[,1])
names(v10)[1]<-"app_name"

v11 <- as.data.frame(unique(MV_apps$app_n[grep(tolower(media_appnames$App4[11]), tolower(MV_apps$app_n))]))
rm(v11)

v12 <- as.data.frame(unique(MV_apps$app_n[grep(tolower(media_appnames$App4[12]), tolower(MV_apps$app_n))]))
rm(v12)

v13 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[13], MV_apps$app_n)]))
v13[,1] <- as.character(v13[,1])
names(v13)[1]<-"app_name"

v14 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[14], MV_apps$app_n)]))
rm(v14)

v15 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[15], MV_apps$app_n)]))
rm(v15)

v16 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[16], MV_apps$app_n)]))
rm(v16)

v17 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[17], MV_apps$app_n)]))
rm(v17)

v18 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[18], MV_apps$app_n)]))
rm(v18)

v19 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[19], MV_apps$app_n)]))
rm(v19)

v20 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[20], MV_apps$app_n)]))
v20[,1] <- as.character(v20[,1])
names(v20)[1]<-"app_name"

v21 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[21], MV_apps$app_n)]))
rm(v21)

v22 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[22], MV_apps$app_n)]))
v22[,1] <- as.character(v22[,1])
names(v22)[1]<-"app_name"

v23 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[23], MV_apps$app_n)]))
v23[,1] <- as.character(v23[,1])
names(v23)[1]<-"app_name"

v24 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[24], MV_apps$app_n)]))
v24[,1] <- as.character(v24[,1])
names(v24)[1]<-"app_name"

v25 <- as.data.frame(unique(MV_apps$app_n[grep(tolower(media_appnames$App4[25]), tolower(MV_apps$app_n))]))
rm(v25)

v26 <- as.data.frame(unique(MV_apps$app_n[grep(tolower(media_appnames$App4[26]), tolower(MV_apps$app_n))]))
rm(v26)

v27 <- as.data.frame(unique(MV_apps$app_n[grep(tolower(media_appnames$App4[27]), tolower(MV_apps$app_n))]))
rm(v27)

v28 <- as.data.frame(unique(MV_apps$app_n[grep(tolower(media_appnames$App4[28]), tolower(MV_apps$app_n))]))
rm(v28)

v29 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[29], MV_apps$app_n)]))
rm(v29)

v30 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[30], MV_apps$app_n)]))
rm(v30)

v31 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[31], MV_apps$app_n)]))
rm(v31)

v32 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[32], MV_apps$app_n)]))
rm(v32)

v33 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[33], MV_apps$app_n)]))
rm(v33)

v34 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[34], MV_apps$app_n)]))
rm(v34)

v35 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[35], MV_apps$app_n)]))
rm(v35)

v36 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[36], MV_apps$app_n)]))
rm(v36)

v37 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[37], MV_apps$app_n)]))
v37[,1] <- as.character(v37[,1])
names(v37)[1]<-"app_name"

v38 <- as.data.frame(unique(MV_apps$app_n[grep(tolower(media_appnames$App4[38]), tolower(MV_apps$app_n))]))
rm(v38)

v39 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[39], MV_apps$app_n)]))
rm(v39)

v40 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[40], MV_apps$app_n)]))
v40[,1] <- as.character(v40[,1])
names(v40)[1]<-"app_name"

v41 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[41], MV_apps$app_n)]))
rm(v41)

v42 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[42], MV_apps$app_n)]))
rm(v42)

v43 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[43], MV_apps$app_n)]))
rm(v43)

v44 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[44], MV_apps$app_n)]))
rm(v44)

v45 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[45], MV_apps$app_n)]))
rm(v45)

v46 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[46], MV_apps$app_n)]))
rm(v46)

v47 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[47], MV_apps$app_n)]))
rm(v47)

v48 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[48], MV_apps$app_n)]))
rm(v48)

v49 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[49], MV_apps$app_n)]))
rm(v49)

v50 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[50], MV_apps$app_n)]))
rm(v50)

v51 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[51], MV_apps$app_n)]))
rm(v51)

v52 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[52], MV_apps$app_n)]))
rm(v52)

v53 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[53], MV_apps$app_n)]))
rm(v53)

v54 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[54], MV_apps$app_n)]))
rm(v54)

v55 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App4[55], MV_apps$app_n)]))
rm(v55)

v56 <- as.data.frame(unique(MV_apps$app_n[grep(media_appnames$App[56], MV_apps$app_n)]))
rm(v56)


media_apps <- rbind(v1,v10,v13,v2,v20,v22,v23,v24,v3,v37,v4,v40,v6,v7,v8,v9)
names(media_apps)[1]<-"app_n"

media_apps$news_app <- 1

rm(media_appnames)
rm(v1, v10, v13, v2, v20, v22, v23, v24, v3, v37, v4, v40, v6,v7,v8,v9)


###MERGE TO MOBILE VIEWS
MV_apps <- merge(MV_apps, media_apps, by="app_n", all=TRUE)
MV_apps$news_app[is.na(MV_apps$news_app)] <- 0
rm(media_apps)

MV_apps$facebook_app <- ifelse(MV_apps$app_n=="Facebook" |
                                 MV_apps$app_n=="Facebook Lite" |
                                 MV_apps$app_n=="Facebook Groups" |
                                 MV_apps$app_n=="Facebook Pages Manager",
                                    yes = 1, no = 0)

#################APP info
a_totals <- MV_apps %>%
  group_by(panelist_id) %>% 
  summarise(
    tot_n = n(),
    tot_d = sum(duration, na.rm = TRUE)
  )

a_fb_share2 <- MV_apps %>% 
  group_by(panelist_id, facebook_app) %>%
  summarise(
    fb_tot_n = n(),
    fb_tot_d = sum(duration, na.rm = TRUE))  

a_fb_share2 <- merge(a_fb_share2, a_totals, by="panelist_id")

a_fb_share2 <- a_fb_share2 %>%
  mutate(
    fb_rel_n = fb_tot_n/tot_n,
    fb_rel_d = fb_tot_d/tot_d
  ) %>% 
  filter(facebook_app==1)

a_media_share2 <- MV_apps %>% 
  group_by(panelist_id, news_app) %>%
  summarise(
    news_tot_n = n(),
    news_tot_d = sum(duration, na.rm = TRUE))  

a_media_share2 <- merge(a_media_share2, a_totals, by="panelist_id")

a_media_share2 <- a_media_share2 %>%
  mutate(
    news_rel_n = news_tot_n/tot_n,
    news_rel_d = news_tot_d/tot_d
  ) %>% 
  filter(news_app==1)

media_usage <- merge(a_totals, a_fb_share2, by=c("panelist_id", "tot_n", "tot_d"), all = TRUE)
media_usage <- merge(media_usage, a_media_share2, by=c("panelist_id", "tot_n", "tot_d"), all = TRUE)

media_usage[is.na(media_usage)] <- 0

media_usage <- select(media_usage, -c(facebook_app, news_app))

media_usage <- mutate(media_usage,
                      fb_news_tot_n = fb_tot_n + news_tot_n,
                      fb_news_tot_d = fb_tot_d + news_tot_d,
                      fb_news_rel_n = fb_tot_n / fb_news_tot_n,
                      fb_news_rel_d = fb_tot_d / fb_news_tot_d
)  
media_usage$fb_news_rel_n[is.na(media_usage$fb_news_rel_n)] <- 0.5
media_usage$fb_news_rel_d[is.na(media_usage$fb_news_rel_d)] <- 0.5

media_usage$tot_d <- NULL
media_usage$tot_n <- NULL

rm(a_fb_share2, a_media_share2, a_totals)

app_names <- names(media_usage)
app_names <- paste0('app_', app_names)
media_usage <- set_names(media_usage, nm = app_names)

media_usage <- rename(media_usage, panelist_id = app_panelist_id)
rm(app_names, MV_apps)


saveRDS(media_usage, file = "./data/mv_app_news_media_final.RDS")

rm(media_usage)


########################################################################
##repeat for SITES of mobile visits
########################################################################


media_sites <- read.csv(file=".\\media_landscape\\50 most used news sites_clean.csv", header=TRUE, sep = ";")
media_sites$X <- NULL
#drop unnecessary characters from domains
media_sites$domain <- gsub("http","",media_sites$domain)
media_sites$domain <- gsub("s:","",media_sites$domain)
media_sites$domain <- gsub(":","",media_sites$domain)
media_sites$domain <- gsub("//","",media_sites$domain)
media_sites$domain <- gsub("/","",media_sites$domain)
media_sites$domain <- gsub("www.","",media_sites$domain)

#gen identifier that will tell us after merge if news media domain
media_sites$news_media <- 1


mobile_views <- readRDS(file = "./original daten/mobile_views.rds")

MV_sites <- mobile_views %>%
  filter(is.na(app_n)) %>%
  filter(is.na(app_os))

rm(mobile_views)

MV_sites <- merge(MV_sites, media_sites, by="domain", all=TRUE)
MV_sites$news_media[is.na(MV_sites$news_media)] <- 0

MV_sites$short_dom <- substr(MV_sites$domain, 1, 9)
MV_sites$facebook_site <- ifelse(MV_sites$short_dom=="facebook.", yes = 1, no = 0)
MV_sites$short_dom <- NULL



#################DOMAIN info
d_totals <- MV_sites %>%
  group_by(panelist_id) %>% 
  summarise(
    tot_n = n(),
    tot_d = sum(duration, na.rm = TRUE)
  )

d_fb_share2 <- MV_sites %>% 
  group_by(panelist_id, facebook_site) %>%
  summarise(
    fb_tot_n = n(),
    fb_tot_d = sum(duration, na.rm = TRUE))  

d_fb_share2 <- merge(d_fb_share2, d_totals, by="panelist_id")

d_fb_share2 <- d_fb_share2 %>%
  mutate(
    fb_rel_n = fb_tot_n/tot_n,
    fb_rel_d = fb_tot_d/tot_d
  ) %>% 
  filter(facebook_site==1)

d_media_share2 <- MV_sites %>% 
  group_by(panelist_id, news_media) %>%
  summarise(
    news_tot_n = n(),
    news_tot_d = sum(duration, na.rm = TRUE))  

d_media_share2 <- merge(d_media_share2, d_totals, by="panelist_id")

d_media_share2 <- d_media_share2 %>%
  mutate(
    news_rel_n = news_tot_n/tot_n,
    news_rel_d = news_tot_d/tot_d
  ) %>% 
  filter(news_media==1)

media_usage <- merge(d_totals, d_fb_share2, by=c("panelist_id", "tot_n", "tot_d"), all = TRUE)
media_usage <- merge(media_usage, d_media_share2, by=c("panelist_id", "tot_n", "tot_d"), all = TRUE)

media_usage[is.na(media_usage)] <- 0

media_usage <- select(media_usage, -c(facebook_site, news_media))

media_usage <- mutate(media_usage,
                      fb_news_tot_n = fb_tot_n + news_tot_n,
                      fb_news_tot_d = fb_tot_d + news_tot_d,
                      fb_news_rel_n = fb_tot_n / fb_news_tot_n,
                      fb_news_rel_d = fb_tot_d / fb_news_tot_d
)  
media_usage$fb_news_rel_n[is.na(media_usage$fb_news_rel_n)] <- 0.5
media_usage$fb_news_rel_d[is.na(media_usage$fb_news_rel_d)] <- 0.5

media_usage$tot_d <- NULL
media_usage$tot_n <- NULL

rm(d_fb_share2, d_media_share2, d_totals)

dom_names <- names(media_usage)
dom_names <- paste0('dom_', dom_names)
media_usage <- set_names(media_usage, nm = dom_names)

media_usage <- rename(media_usage, panelist_id = dom_panelist_id)
rm(dom_names, MV_sites)


saveRDS(media_usage, file = "./data/mv_dom_news_media_final.RDS")

rm(media_usage)




#################DOMAIN info
web_pageviews <- readRDS(file = ".\\data\\web_pageviews_prep.rds")
web_pageviews <- merge(web_pageviews, media_sites, by="domain", all=TRUE)
web_pageviews$news_media[is.na(web_pageviews$news_media)] <- 0

web_pageviews$short_dom <- substr(web_pageviews$domain, 1, 9)
web_pageviews$facebook <- ifelse(web_pageviews$short_dom=="facebook.", yes = 1, no = 0)
web_pageviews$short_dom <- NULL

totals <- web_pageviews %>%
  group_by(panelist_id) %>% 
  summarise(
    tot_n = n(),
    tot_d = sum(active_seconds, na.rm = TRUE)
  )

d_fb_share2 <- web_pageviews %>% 
  group_by(panelist_id, facebook) %>%
  summarise(
    fb_tot_n = n(),
    fb_tot_d = sum(active_seconds, na.rm = TRUE))  

d_fb_share2 <- merge(d_fb_share2, totals, by="panelist_id")

d_fb_share2 <- d_fb_share2 %>%
  mutate(
    fb_rel_n = fb_tot_n/tot_n,
    fb_rel_d = fb_tot_d/tot_d
  ) %>% 
  filter(facebook==1)

d_media_share2 <- web_pageviews %>% 
  group_by(panelist_id, news_media) %>%
  summarise(
    news_tot_n = n(),
    news_tot_d = sum(active_seconds, na.rm = TRUE))  

d_media_share2 <- merge(d_media_share2, totals, by="panelist_id")

d_media_share2 <- d_media_share2 %>%
  mutate(
    news_rel_n = news_tot_n/tot_n,
    news_rel_d = news_tot_d/tot_d
  ) %>% 
  filter(news_media==1)

media_usage <- merge(totals, d_fb_share2, by=c("panelist_id", "tot_n", "tot_d"), all = TRUE)
media_usage <- merge(media_usage, d_media_share2, by=c("panelist_id", "tot_n", "tot_d"), all = TRUE)

media_usage[is.na(media_usage)] <- 0

media_usage <- select(media_usage, -c(facebook, news_media))

media_usage <- mutate(media_usage,
                      fb_news_tot_n = fb_tot_n + news_tot_n,
                      fb_news_tot_d = fb_tot_d + news_tot_d,
                      fb_news_rel_n = fb_tot_n / fb_news_tot_n,
                      fb_news_rel_d = fb_tot_d / fb_news_tot_d
)  
media_usage$fb_news_rel_n[is.na(media_usage$fb_news_rel_n)] <- 0.5
media_usage$fb_news_rel_d[is.na(media_usage$fb_news_rel_d)] <- 0.5

media_usage$tot_d <- NULL
media_usage$tot_n <- NULL

rm(d_fb_share2, d_media_share2, totals)

dom_names <- names(media_usage)
dom_names <- paste0('dom_', dom_names)
media_usage <- set_names(media_usage, nm = dom_names)

media_usage <- rename(media_usage, panelist_id = dom_panelist_id)
rm(dom_names, web_pageviews, media_sites)


saveRDS(media_usage, file = "./data/wp_dom_news_media_final.RDS")

rm(media_usage)


