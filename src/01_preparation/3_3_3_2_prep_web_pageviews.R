library(pacman)
p_load(tidyverse, foreign, haven, data.table, dummies, urltools, parallel, stringr)

setwd("Y:\\Respondi\\RESPONDI_w3\\")

#source(".\\code\\3_3_3_1_prep_web_pageviews.R")

###############################################################################s
###############################################################################
web_pageviews <- readRDS(file = ".\\data\\web_pageviews_prep.rds")

#limit to most used domains (at least 60sec/1minute spent overall in data and 80 visits)
SITES20 <- web_pageviews %>%
  group_by(domain) %>%
  summarise(count_visits = n(),
            time_spent = sum(active_seconds, na.rm=TRUE) ) %>%
  arrange(time_spent,count_visits) %>%
  filter(time_spent >= 60 & count_visits >= 80) %>% 
  select(domain)

SITES20$MOSTUSED <- 1 
SITES20 <- merge(SITES20, web_pageviews, by="domain", all=TRUE)

SITES202 <- SITES20 %>% 
  filter(MOSTUSED==1) %>% 
  ungroup() %>% 
  group_by(panelist_id, domain) %>% 
  summarise(count_visits = n(),
            time_spent = sum(active_seconds, na.rm=TRUE) ) 

rm(web_pageviews, SITES20)
SITES_1 <- SITES202[1:49709,]
SITES_2 <- SITES202[49710:99938,]
SITES_3 <- SITES202[99939:149898,]
SITES_4 <- SITES202[149899:199959,]
SITES_5 <- SITES202[199960:length(SITES202$panelist_id),]

SITES_1 <- SITES_1 %>%
  select(panelist_id, domain, time_spent)
SITES_2 <- SITES_2 %>%
  select(panelist_id, domain, time_spent)
SITES_3 <- SITES_3 %>%
  select(panelist_id, domain, time_spent)
SITES_4 <- SITES_4 %>%
  select(panelist_id, domain, time_spent)
SITES_5 <- SITES_5 %>%
  select(panelist_id, domain, time_spent)

SITES20_1 <- spread(SITES_1, domain, time_spent)
SITES20_2 <- spread(SITES_2, domain, time_spent)
SITES20_3 <- spread(SITES_3, domain, time_spent)
SITES20_4 <- spread(SITES_4, domain, time_spent)
SITES20_5 <- spread(SITES_5, domain, time_spent)

SITES_20 <- bind_rows(SITES20_1,SITES20_2,SITES20_3,SITES20_4,SITES20_5 )
rm(SITES_1,SITES_2 ,SITES_3,SITES_4,SITES_5,SITES20_1,SITES20_2,SITES20_3,SITES20_4,SITES20_5)

SITES_20[is.na(SITES_20)] <- 0
rm(SITES202)

saveRDS(SITES_20, file = ".\\data\\wep_pageviews_prepared.RDS")
rm(SITES_20)

####################################################################################
web_pageviews <- readRDS(file = ".\\data\\web_pageviews_prep.rds")

web_pageviews$hour <- as.integer(substr(web_pageviews$used_at, 12, 13))

v_twelvetosix <- web_pageviews %>%
  group_by(panelist_id, domain) %>%
  summarise(
    v_betw12and6 = sum(ifelse(hour==00| hour==01| hour==02| hour==03|hour==04|hour==05, 1, 0))
  )
web_pageviews <- merge(web_pageviews, v_twelvetosix, by=c("panelist_id", "domain"), all.x=TRUE)
rm(v_twelvetosix)

# Total visits between 12 and 6
tot_v_twelvetosix <- web_pageviews %>%
  group_by(panelist_id, domain) %>%
  summarise(
    tot_v_12to6 = as.numeric(sum(v_betw12and6)/(length(domain)))
  ) %>%
  group_by(panelist_id) %>%
  summarise(
    tot_12to6 = sum(tot_v_12to6)
  ) 
#n_domains_user <- merge(n_domains_user, tot_v_twelvetosix, by=c("panelist_id"), all.x=TRUE)
#rm(tot_v_twelvetosix)

# Total time spent online between 12 and 6
tot_t_12to6 <- web_pageviews %>%
  group_by(panelist_id) %>%
  summarise(
    tot_t_time12to6 = sum(as.numeric(ifelse(hour==00| hour==01| hour==02| hour==03|hour==04|hour==05, active_seconds, 0)))
  ) 
n_domains_user <- merge(tot_v_twelvetosix, tot_t_12to6, by=c("panelist_id"), all.x=TRUE)
rm(tot_t_12to6)
web_pageviews <- select(web_pageviews, -hour)

# Time spent on weekends
web_pageviews$date <- (substr(web_pageviews$used_at, 1, 10))
web_pageviews$day <- as.integer(strftime(web_pageviews$date, '%u'))

time_weekend <- web_pageviews %>%
  group_by(panelist_id, day) %>%
  summarise(
    time = sum(as.numeric(active_seconds))
  )

time_weekend <- time_weekend %>%
  filter(day==6 | day==7) %>%
  group_by(panelist_id) %>%
  summarise(
    time_weekend = sum(time)
  )

n_domains_user <- merge(n_domains_user, time_weekend, by=c("panelist_id"), all.x=TRUE)
rm(time_weekend)

# Time spent on weekdays
time_weekday <- web_pageviews %>%
  group_by(panelist_id, day) %>%
  summarise(
    time = sum(as.numeric(active_seconds))
  )

time_weekday <- time_weekday %>%
  filter(day==1 | day==2 | day==3 | day==4 | day==5) %>%
  group_by(panelist_id) %>%
  summarise(
    time_weekday = sum(time)
  )
n_domains_user <- merge(n_domains_user, time_weekday, by=c("panelist_id"), all.x=TRUE)
rm(time_weekday)
rm(tot_v_twelvetosix)

rm(web_pageviews)

saveRDS(n_domains_user, file = ".\\data\\wep_pageviews_prepared_small.rds")


