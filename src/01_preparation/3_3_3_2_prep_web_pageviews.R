library(pacman)
p_load(tidyverse, foreign, haven, data.table, dummies, urltools, parallel, stringr)

setwd("\\\\nas.uni-mannheim.de\\uni-shares\\swnsswml\\Respondi\\")

#source(".\\code\\3_3_3_1_prep_web_pageviews.R")

###############################################################################s
###############################################################################
web_pageviews <- readRDS(file = ".\\data\\web_pageviews_prep.rds")
n_domains_user <- web_pageviews %>%
  group_by(panelist_id) %>%
  summarise(
    dom_tot_p_n = n(),
    dom_tot_p_dur = sum(active_seconds, na.rm = TRUE)
  )
#web_pageviews <- merge(web_pageviews, n_domains_user, by="panelist_id")

#limit to 20 most used domains
SITES20 <- web_pageviews %>%
  group_by(panelist_id, domain) %>%
  summarise(count_visits = n(),
            time_spent = sum(active_seconds, na.rm=TRUE)
            )
SITES20 <- merge(n_domains_user, SITES20, by=c("panelist_id"))
#share of visits and duration per user per domain
X2 <- transmute(SITES20,
                dom_rel_p_n = count_visits/dom_tot_p_n,
                dom_rel_p_dur = time_spent/dom_tot_p_dur
    )
SITES20 <- cbind(SITES20,X2)
X2 <- web_pageviews %>% 
  group_by(panelist_id, domain) %>% 
  filter(row_number() == 1) %>%
  select(panelist_id, domain, starts_with("dom_rel"))
SITES20 <- merge(SITES20, X2, by = c("panelist_id", "domain"))
rm(X2)

SITES20$panelist_id <- as.numeric(SITES20$panelist_id)

SITES20 <- SITES20 %>%
  arrange(-(panelist_id), -(time_spent),-(count_visits) )%>%
  ungroup() %>%
  group_by(panelist_id) %>%
  arrange(panelist_id, -(time_spent),-(count_visits) ) %>%
  filter(row_number() >= 1, row_number() <= 5)
#how many different domains remain?
length(unique(SITES20$domain)) #8k

SITES20_count <- SITES20
SITES20_count <- SITES20_count %>%
  select(panelist_id, domain, count_visits)
SITES20_count <- spread(SITES20_count, domain, count_visits)

SITES20_time <- SITES20
SITES20_time <- SITES20_time %>%
  select(panelist_id, domain, time_spent)
SITES20_time <- spread(SITES20_time, domain, time_spent)

SITES20_d_rel_n <- SITES20
SITES20_d_rel_n <- SITES20_d_rel_n %>%
  select(panelist_id, domain, dom_rel_p_n)
SITES20_d_rel_n <- spread(SITES20_d_rel_n, domain, dom_rel_p_n)

SITES20_d_rel_dur <- SITES20
SITES20_d_rel_dur <- SITES20_d_rel_dur %>%
  select(panelist_id, domain, dom_rel_p_dur)
SITES20_d_rel_dur <- spread(SITES20_d_rel_dur, domain, dom_rel_p_dur)

CNT_names <- names(SITES20_count)
TM_names <- names(SITES20_time)
DN_names <- names(SITES20_d_rel_n)
DD_names <- names(SITES20_d_rel_dur)

CNT_names <- paste0('CNT_', CNT_names)
TM_names <- paste0('TM_', TM_names)
DN_names <- paste0('DN_', DN_names)
DD_names <- paste0('DD_', DD_names)

SITES20_count <- set_names(SITES20_count, nm = CNT_names)
SITES20_time <- set_names(SITES20_time, nm = TM_names)
SITES20_d_rel_n <- set_names(SITES20_d_rel_n, nm = DN_names)
SITES20_d_rel_dur <- set_names(SITES20_d_rel_dur, nm = DD_names)

SITES20_count <- rename(SITES20_count, panelist_id = CNT_panelist_id)
SITES20_time <- rename(SITES20_time, panelist_id = TM_panelist_id)
SITES20_d_rel_n <- rename(SITES20_d_rel_n, panelist_id = DN_panelist_id)
SITES20_d_rel_dur <- rename(SITES20_d_rel_dur, panelist_id = DD_panelist_id)

rm(CNT_names, TM_names, DN_names, DD_names)
SITES20 <- merge(SITES20_count, SITES20_time, by="panelist_id")
SITES20 <- merge(SITES20, SITES20_d_rel_n, by="panelist_id")
SITES20 <- merge(SITES20, SITES20_d_rel_dur, by="panelist_id")

rm(SITES20_count, SITES20_time,SITES20_d_rel_n,SITES20_d_rel_dur)

SITES20[is.na(SITES20)] <- 0


####################################################################################

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

saveRDS(SITES20, file = ".\\data\\wep_pageviews_prepared.RDS")

