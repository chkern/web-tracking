##################################################################################

library(plyr)
library(tidyverse)
library(foreign)
library(haven)
library(data.table)
library(magrittr)

setwd("\\\\nas.uni-mannheim.de\\uni-shares\\swnsswml\\Respondi\\")

survey_w1 <- read_dta(file = ".\\survey_daten\\survey_data_w1.dta")

tracking_data <- readRDS(file = ".\\data\\data_prep_final_small.rds")

tracking_full <- readRDS(file = ".\\data\\data_prep_final.rds")

news_media <- readRDS(file = ".\\data\\news_media_final.RDS")

load(".\\background_info\\background.RData")

##################################################################################
# Prepare data (y = sinus)
##################################################################################

# y

survey_w1$sinus_mil <- as_factor(survey_w1$sinus_mil, labels = "values")
survey_w1$sinus_mil <- droplevels(survey_w1$sinus_mil)
summary(survey_w1$sinus_mil)

survey_w1$sinus_mil <- revalue(survey_w1$sinus_mil, c("Sozialökologisches Milieu (S?-K)" = "Sozialoekologisches Milieu (SOEK)",
                                                      "Bürgerliche Mitte (B?oM)" = "Buergerliche Mitte (BUEM)",
                                                      "Prekäres Milieu (PRE)" = "Prekaeres Milieu (PRE)"))

survey_w1$panelist_id %<>% as.character()
y <- select(survey_w1, panelist_id, sinus_mil)

# Background data

back <- select(background, panelist_id, m_1000, m_1001, m_1006, md_2806,
               md_0004, md_0006, md_1171, md_1172, md_1174, md_1175, md_1176,
               md_1181, md_1185, md_1223, md_1264, md_1634, md_1635, md_1660, 
               md_2172, md_2189, md_2473, md_2861, md_1000)

back$panelist_id %<>% as.character()
back <- droplevels(back)

# Tracking data small

names0 <- names(tracking_data[,2:ncol(tracking_data)])

# Tracking data full

tracking_full <- tracking_full[ , -which(names(tracking_full) %in% names0)]

tracking_full$data <- "Mobile or Web views"
tracking_full$data <- factor(tracking_full$data, levels = c("Mobile or Web views", "Mobile views Missing", "Web views/pages Missing"))
tracking_full$data[is.na(tracking_full$mv_CNT_accuweather.com)] <- "Mobile views Missing"
tracking_full$data[is.na(tracking_full$wv_CNT_1und1.de)] <- "Web views/pages Missing"

tracking_full[is.na(tracking_full)] <- 0

# Merge

data <- left_join(back, y)
data <- left_join(data, tracking_data)
data <- left_join(data, tracking_full)
data <- left_join(data, news_media)

sum(is.na(data))
nrow(data)

##################################################################################
# Descriptive - Tracking data (small)
##################################################################################

summary(data[,c(24,25,26,41,42,54,55,192:212)])

# Tables

data %>%
  group_by(sinus_mil) %>% 
  summarise(Mean_mob=mean(mv_ever_mob, na.rm = T), Mean_tab=mean(mv_ever_tab, na.rm = T),
            Mean_utype_d=mean(mv_utype_ho_rel_dur, na.rm = T), Mean_utype__n=mean(mv_utype_ho_rel_n, na.rm = T),
            Mean_wifi_d=mean(mv_con_wifi_rel_dur, na.rm = T), Mean_wifi_n=mean(mv_con_wifi_rel_n, na.rm = T))

data %>%
  group_by(sinus_mil) %>% 
  summarise(Mean_n=mean(mv_dom_tot_p_n, na.rm = T), Median_n=median(mv_dom_tot_p_n, na.rm = T), Std_n=sd(mv_dom_tot_p_n, na.rm = T),
            Mean_d=mean(mv_dom_tot_p_dur, na.rm = T), Median_d=median(mv_dom_tot_p_dur, na.rm = T), Std_d=sd(mv_dom_tot_p_dur, na.rm = T))

data %>%
  group_by(sinus_mil) %>% 
  summarise(Mean=mean(mv_tot_12to6.x, na.rm = T), Median=median(mv_tot_12to6.x, na.rm = T), Std=sd(mv_tot_12to6.x, na.rm = T),
            Mean_t=mean(mv_tot_t_time12to6.x, na.rm = T), Median_t=median(mv_tot_t_time12to6.x, na.rm = T), Std_t=sd(mv_tot_t_time12to6.x, na.rm = T))

data %>%
  group_by(sinus_mil) %>% 
  summarise(Mean_end=mean(mv_time_weekend.x, na.rm = T), Median_end=median(mv_time_weekend.x, na.rm = T), Std_end=sd(mv_time_weekend.x, na.rm = T),
            Mean_day=mean(mv_time_weekday.x, na.rm = T), Median_day=median(mv_time_weekday.x, na.rm = T), Std_day=sd(mv_time_weekday.x, na.rm = T))

data %>%
  group_by(sinus_mil) %>% 
  summarise(Mean=mean(mv_tot_12to6.y, na.rm = T), Median=median(mv_tot_12to6.y, na.rm = T), Std=sd(mv_tot_12to6.y, na.rm = T),
            Mean_t=mean(mv_tot_t_time12to6.y, na.rm = T), Median_t=median(mv_tot_t_time12to6.y, na.rm = T), Std_t=sd(mv_tot_t_time12to6.y, na.rm = T))

data %>%
  group_by(sinus_mil) %>% 
  summarise(Mean_end=mean(mv_time_weekend.y, na.rm = T), Median_end=median(mv_time_weekend.y, na.rm = T), Std_end=sd(mv_time_weekend.y, na.rm = T),
            Mean_day=mean(mv_time_weekday.y, na.rm = T), Median_day=median(mv_time_weekday.y, na.rm = T), Std_day=sd(mv_time_weekday.y, na.rm = T))

data %>%
  group_by(sinus_mil) %>% 
  summarise(Mean=mean(wp_tot_12to6, na.rm = T), Median=median(wp_tot_12to6, na.rm = T), Std=sd(wp_tot_12to6, na.rm = T),
            Mean_t=mean(wp_tot_t_time12to6, na.rm = T), Median_t=median(wp_tot_t_time12to6, na.rm = T), Std_t=sd(wp_tot_t_time12to6, na.rm = T))

data %>%
  group_by(sinus_mil) %>% 
  summarise(Mean_end=mean(wp_time_weekend, na.rm = T), Median_end=median(wp_time_weekend, na.rm = T), Std_end=sd(wp_time_weekend, na.rm = T),
            Mean_day=mean(wp_time_weekday, na.rm = T), Median_day=median(wp_time_weekday, na.rm = T), Std_day=sd(wp_time_weekday, na.rm = T))

data %>%
  group_by(sinus_mil) %>% 
  summarise(Mean_n=mean(wv_dom_tot_p_n, na.rm = T), Median_n=median(wv_dom_tot_p_n, na.rm = T), Std_n=sd(wv_dom_tot_p_n, na.rm = T),
            Mean_d=mean(wv_dom_tot_p_dur, na.rm = T), Median_d=median(wv_dom_tot_p_dur, na.rm = T), Std_d=sd(wv_dom_tot_p_dur, na.rm = T))

data %>%
  group_by(sinus_mil) %>% 
  summarise(Mean=mean(wv_pviews_p_user, na.rm = T), Median=median(wv_pviews_p_user, na.rm = T), Std=sd(wv_pviews_p_user, na.rm = T))

data %>%
  group_by(sinus_mil) %>% 
  summarise(Mean=mean(wv_tot_12to6, na.rm = T), Median=median(wv_tot_12to6, na.rm = T), Std=sd(wv_tot_12to6, na.rm = T),
            Mean_t=mean(wv_tot_t_time12to6, na.rm = T), Median_t=median(wv_tot_t_time12to6, na.rm = T), Std_t=sd(wv_tot_t_time12to6, na.rm = T))

data %>%
group_by(sinus_mil) %>% 
  summarise(Mean_end=mean(wv_time_weekend, na.rm = T), Median_end=median(wv_time_weekend, na.rm = T), Std_end=sd(wv_time_weekend, na.rm = T),
            Mean_day=mean(wv_time_weekday, na.rm = T), Median_day=median(wv_time_weekday, na.rm = T), Std_day=sd(wv_time_weekday, na.rm = T))

# Graphs (templates)

ggplot(data, aes(mv_dom_tot_p_n, fill = sinus_mil, colour = sinus_mil)) +
  geom_density(position = "stack") +
  xlim(0, 10000) + 
  labs(fill = "Sinus Milieus",
       colour = "Sinus Milieus",
       x = "x label")

ggsave("g1.png", width = 8, height = 6)

ggplot(data, aes(mv_dom_tot_p_n, fill = sinus_mil, colour = sinus_mil)) +
  geom_density(position = "fill") + 
  labs(fill = "Sinus Milieus",
       colour = "Sinus Milieus",
       x = "x label")

ggsave("g2.png", width = 8, height = 6)

ggplot(data, aes(y = mv_dom_tot_p_n, x = sinus_mil, fill = sinus_mil)) +
  geom_boxplot() +
  ylim(0, 5000) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())  + 
  labs(fill = "Sinus Milieus",
       y = "y label")

ggsave("g3.png", width = 8, height = 6)

##################################################################################
# Descriptive - News-Media data
##################################################################################

summary(data[,c(773,774,785,786,797,798,777,778,789,790,801,802)])

# Tables

data %>%
  group_by(sinus_mil) %>% 
  summarise(Mean_n=mean(mv_fb_tot_n, na.rm = T), Median_n=median(mv_fb_tot_n, na.rm = T), Std_n=sd(mv_fb_tot_n, na.rm = T),
            Mean_d=mean(mv_fb_tot_d, na.rm = T), Median_d=median(mv_fb_tot_d, na.rm = T), Std_d=sd(mv_fb_tot_d, na.rm = T))

data %>%
  group_by(sinus_mil) %>% 
  summarise(Mean_n=mean(wp_fb_tot_n, na.rm = T), Median_n=median(wp_fb_tot_n, na.rm = T), Std_n=sd(wp_fb_tot_n, na.rm = T),
            Mean_d=mean(wp_fb_tot_d, na.rm = T), Median_d=median(wp_fb_tot_d, na.rm = T), Std_d=sd(wp_fb_tot_d, na.rm = T))

data %>%
  group_by(sinus_mil) %>% 
  summarise(Mean_n=mean(wv_fb_tot_n, na.rm = T), Median_n=median(wv_fb_tot_n, na.rm = T), Std_n=sd(wv_fb_tot_n, na.rm = T),
            Mean_d=mean(wv_fb_tot_d, na.rm = T), Median_d=median(wv_fb_tot_d, na.rm = T), Std_d=sd(wv_fb_tot_d, na.rm = T))

# Check also for Twitter, Instagram, Reddit usage

data %>%
  group_by(sinus_mil) %>% 
  summarise(Mean_n=mean(mv_news_tot_n, na.rm = T), Median_n=median(mv_news_tot_n, na.rm = T), Std_n=sd(mv_news_tot_n, na.rm = T),
            Mean_d=mean(mv_news_tot_d, na.rm = T), Median_d=median(mv_news_tot_d, na.rm = T), Std_d=sd(mv_news_tot_d, na.rm = T))

data %>%
  group_by(sinus_mil) %>% 
  summarise(Mean_n=mean(wp_news_tot_n, na.rm = T), Median_n=median(wp_news_tot_n, na.rm = T), Std_n=sd(wp_news_tot_n, na.rm = T),
            Mean_d=mean(wp_news_tot_d, na.rm = T), Median_d=median(wp_news_tot_d, na.rm = T), Std_d=sd(wp_news_tot_d, na.rm = T))

data %>%
  group_by(sinus_mil) %>% 
  summarise(Mean_n=mean(wv_news_tot_n, na.rm = T), Median_n=median(wv_news_tot_n, na.rm = T), Std_n=sd(wv_news_tot_n, na.rm = T),
            Mean_d=mean(wv_news_tot_d, na.rm = T), Median_d=median(wv_news_tot_d, na.rm = T), Std_d=sd(wv_news_tot_d, na.rm = T))

# Graphs

##################################################################################
# Descriptive - App classification
##################################################################################

##################################################################################
# Factor and/or Cluster analysis
##################################################################################
