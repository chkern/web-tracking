#if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, haven, data.table, dummies)

setwd("C:\\Users\\Lukas\\Desktop\\New folder\\")

web_visits <- readRDS(file = ".\\original daten\\web_visits.rds")
#web_visits <- readRDS(file = "\\\\nas2.ad.uni-mannheim.de\\vol_swnsswml\\Home\\swnsswml\\Respondi\\original daten\\web_visits.rds")
web_visits$domain <- as.character(web_visits$domain)
web_visits$used_at <- as.character(web_visits$used_at)


#here, 20 most used & longest visited sites --- create vars that are constant within user,
#i.e., 1user, 1obs
#results in many covariates

#Total number of domains, total duration and total number of pageviews per user --- constant within user
n_domains_user <- web_visits %>%
  group_by(panelist_id) %>%
  summarise(
    dom_tot_p_n = n(),
    dom_tot_p_dur = sum(duration, na.rm = TRUE),
    pviews_p_user = sum(pageviews, na.rm = TRUE)
  )
web_visits <- merge(web_visits, n_domains_user, by="panelist_id")

#visits per domain per user. time spent per domain per user. pageviews per domain per user
SITES20 <- web_visits %>%
  group_by(panelist_id, domain) %>%
  summarise(count_visits = n(),
            time_spent = sum(duration, na.rm=TRUE),
            pviews_p_user_domain = sum(pageviews, na.rm=TRUE))
web_visits <- merge(web_visits, SITES20, by=c("panelist_id", "domain"))

#share of visits, duration, pageviews per user per domain
X2 <- transmute(web_visits,
                dom_rel_p_n = count_visits/dom_tot_p_n,
                dom_rel_p_dur = time_spent/dom_tot_p_dur,
                pviews_rel_p_user_dom = pviews_p_user_domain/pviews_p_user
)
web_visits <- cbind(web_visits,X2)
X2 <- web_visits %>% 
  group_by(panelist_id, domain) %>% 
  filter(row_number() == 1) %>%
  select(panelist_id, domain, starts_with("dom_rel"), pviews_rel_p_user_dom)
SITES20 <- merge(SITES20, X2, by = c("panelist_id", "domain"))
rm(X2)

#select 5 most used, sorted by duration and then pageviews
SITES20 <- SITES20 %>%
  arrange(-(panelist_id), -(time_spent),-(count_visits) )%>%
  ungroup() %>%
  group_by(panelist_id) %>%
  arrange(panelist_id, -(time_spent),-(count_visits) ) %>%
  filter(row_number() >= 1, row_number() <= 5)
#how many different domains remain?
length(unique(SITES20$domain)) #4k

#create dummies for every domain that remains (because we need one obs per person)
SITES20_count <- SITES20
SITES20_count <- SITES20_count %>%
  select(panelist_id, domain, count_visits)
SITES20_count <- spread(SITES20_count, domain, count_visits)

SITES20_time <- SITES20
SITES20_time <- SITES20_time %>%
  select(panelist_id, domain, time_spent)
SITES20_time <- spread(SITES20_time, domain, time_spent)

SITES20_views <- SITES20
SITES20_views <- SITES20_views %>%
  select(panelist_id, domain, pviews_p_user_domain)
SITES20_views <- spread(SITES20_views, domain, pviews_p_user_domain)

SITES20_d_rel_n <- SITES20
SITES20_d_rel_n <- SITES20_d_rel_n %>%
  select(panelist_id, domain, dom_rel_p_n)
SITES20_d_rel_n <- spread(SITES20_d_rel_n, domain, dom_rel_p_n)

SITES20_d_rel_dur <- SITES20
SITES20_d_rel_dur <- SITES20_d_rel_dur %>%
  select(panelist_id, domain, dom_rel_p_dur)
SITES20_d_rel_dur <- spread(SITES20_d_rel_dur, domain, dom_rel_p_dur)

SITES20_v_rel <- SITES20
SITES20_v_rel <- SITES20_v_rel %>%
  select(panelist_id, domain, pviews_rel_p_user_dom)
SITES20_v_rel <- spread(SITES20_v_rel, domain, pviews_rel_p_user_dom)

CNT_names <- names(SITES20_count)
TM_names <- names(SITES20_time)
V_names <- names(SITES20_views)
DN_names <- names(SITES20_d_rel_n)
DD_names <- names(SITES20_d_rel_dur)
VR_names <- names(SITES20_v_rel)

CNT_names <- paste0('CNT_', CNT_names)
TM_names <- paste0('TM_', TM_names)
V_names <- paste0('V_', V_names)
DN_names <- paste0('DN_', DN_names)
DD_names <- paste0('DD_', DD_names)
VR_names <- paste0('VR_', VR_names)

SITES20_count <- set_names(SITES20_count, nm = CNT_names)
SITES20_time <- set_names(SITES20_time, nm = TM_names)
SITES20_views <- set_names(SITES20_views, nm = V_names)
SITES20_d_rel_n <- set_names(SITES20_d_rel_n, nm = DN_names)
SITES20_d_rel_dur <- set_names(SITES20_d_rel_dur, nm = DD_names)
SITES20_v_rel <- set_names(SITES20_v_rel, nm = VR_names)


SITES20_count <- rename(SITES20_count, panelist_id = CNT_panelist_id)
SITES20_time <- rename(SITES20_time, panelist_id = TM_panelist_id)
SITES20_views <- rename(SITES20_views, panelist_id = V_panelist_id)
SITES20_d_rel_n <- rename(SITES20_d_rel_n, panelist_id = DN_panelist_id)
SITES20_d_rel_dur <- rename(SITES20_d_rel_dur, panelist_id = DD_panelist_id)
SITES20_v_rel <- rename(SITES20_v_rel, panelist_id = VR_panelist_id)

rm(CNT_names, TM_names, V_names, DN_names, DD_names, VR_names)
SITES20 <- merge(SITES20_count, SITES20_time, by="panelist_id")
SITES20 <- merge(SITES20, SITES20_views, by="panelist_id")
SITES20 <- merge(SITES20, SITES20_d_rel_n, by="panelist_id")
SITES20 <- merge(SITES20, SITES20_d_rel_dur, by="panelist_id")
SITES20 <- merge(SITES20, SITES20_v_rel, by="panelist_id")

rm(SITES20_count, SITES20_time,SITES20_views,SITES20_d_rel_n,SITES20_d_rel_dur,SITES20_v_rel)
SITES20[is.na(SITES20)] <- 0

mean(SITES20$DN_readly.com, na.rm = TRUE)



#Split up all data in four chunks (all should have same variables:
#before wave 1, after wave1&before wave2, 
#after wave2&before wave3, after wave 3



#usage between 12pm and 6am? Usage on weekdays? Weekends? 
# Number of visits per domain between 12pm and 6am
web_visits$hour <- as.integer(substr(web_visits$used_at, 12, 13))

v_twelvetosix <- web_visits %>%
  group_by(panelist_id, domain) %>%
  summarise(
    v_betw12and6 = sum(ifelse(hour==00| hour==01| hour==02| hour==03|hour==04|hour==05, 1, 0))
  )
web_visits <- merge(web_visits, v_twelvetosix, by=c("panelist_id", "domain"), all.x=TRUE)
rm(v_twelvetosix)

# Total visits between 12 and 6
tot_v_twelvetosix <- web_visits %>%
  group_by(panelist_id, domain) %>%
  summarise(
    tot_v_12to6 = as.numeric(sum(v_betw12and6)/(length(domain)))
  ) %>%
  group_by(panelist_id) %>%
  summarise(
    tot_12to6 = sum(tot_v_12to6)
  ) 
n_domains_user <- merge(n_domains_user, tot_v_twelvetosix, by=c("panelist_id"), all.x=TRUE)
rm(tot_v_twelvetosix)

# Total time spent online between 12 and 6
tot_t_12to6 <- web_visits %>%
  group_by(panelist_id) %>%
  summarise(
    tot_t_time12to6 = sum(as.numeric(ifelse(hour==00| hour==01| hour==02| hour==03|hour==04|hour==05, duration, 0)))
  ) 
n_domains_user <- merge(n_domains_user, tot_t_12to6, by=c("panelist_id"), all.x=TRUE)
rm(tot_t_12to6)
web_visits <- select(web_visits, -hour)

# Time spent on weekends
web_visits$date <- (substr(web_visits$used_at, 1, 10))
web_visits$day <- as.integer(strftime(web_visits$date, '%u'))

time_weekend <- web_visits %>%
  group_by(panelist_id, day) %>%
  summarise(
    time = sum(as.numeric(duration))
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
time_weekday <- web_visits %>%
  group_by(panelist_id, day) %>%
  summarise(
    time = sum(as.numeric(duration))
  )

time_weekday <- time_weekday %>%
  filter(day==1 | day==2 | day==3 | day==4 | day==5) %>%
  group_by(panelist_id) %>%
  summarise(
    time_weekday = sum(time)
  )
n_domains_user <- merge(n_domains_user, time_weekday, by=c("panelist_id"), all.x=TRUE)
rm(time_weekday)

web_visits_final <- merge( n_domains_user ,SITES20, by="panelist_id")
saveRDS(n_domains_user, file = ".\\data\\web_visits_prepared_small.rds")

rm(n_domains_user, SITES20, web_visits)


saveRDS(web_visits_final, file = ".\\data\\web_visits_prepared.rds")
wv_prep <- readRDS(file = ".\\data\\web_visits_prepared.rds")
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################

