#################DONT RUN#######

##################DONT RUN###############


library(pacman)
p_load(tidyverse, foreign, haven, data.table, dummies)

setwd("Z:\\Respondi\\RESPONDI_w3\\")

mobile_views <- readRDS(file = ".\\original daten\\mobile_views.rds")

# Drop all Spells after wave 2 survey: 4. September 2017
mobile_views$DATE <- mobile_views$used_at
mobile_views$DATE <- substr(mobile_views$DATE, 1, 10)

mobile_views$MONTH <- substr(mobile_views$DATE, 6, 7)
mobile_views$MONTH <- as.numeric(mobile_views$MONTH)

head(mobile_views$MONTH)
summary(mobile_views$MONTH)
mobile_views <- filter(mobile_views, MONTH<=9)

mobile_views$DAY <- substr(mobile_views$DATE, 9, 10)
mobile_views$DAY <- as.numeric(mobile_views$DAY)

head(mobile_views$DAY)
summary(mobile_views$DAY)

mobile_views1 <-filter(mobile_views, MONTH<9)


mobile_views2 <- filter(mobile_views, MONTH==9 & DAY<25)

mobile_views <- rbind(mobile_views1, mobile_views2)
rm(mobile_views1, mobile_views2)
mobile_views <- select(mobile_views, -c(DATE, MONTH, DAY))

saveRDS(mobile_views, file = ".\\original daten\\mobile_views.rds")
rm(mobile_views)

web_pageviews <- readRDS(file = ".\\original daten\\web_pageviews.rds")

# Drop all Spells after wave 2 survey: 4. September 2017
web_pageviews$DATE <- substr(web_pageviews$used_at, 1, 10)
head(web_pageviews$DATE)

web_pageviews$MONTH <- substr(web_pageviews$DATE, 6, 7)
web_pageviews$MONTH <- as.numeric(web_pageviews$MONTH)

head(web_pageviews$MONTH)
summary(web_pageviews$MONTH)
web_pageviews <- filter(web_pageviews, MONTH<=9)

web_pageviews$DAY <- substr(web_pageviews$DATE, 9, 10)
web_pageviews$DAY <- as.numeric(web_pageviews$DAY)

head(web_pageviews$DAY)
summary(web_pageviews$DAY)

web_pageviews1 <-filter(web_pageviews, MONTH<9)


web_pageviews2 <- filter(web_pageviews, MONTH==9 & DAY<25)
rm(web_pageviews)
web_pageviews <- rbind(web_pageviews1, web_pageviews2)
rm(web_pageviews1, web_pageviews2)
web_pageviews <- select(web_pageviews, -c(DATE, MONTH, DAY))

saveRDS(web_pageviews, file = ".\\original daten\\web_pageviews.rds")
