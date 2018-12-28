##################################################################################

library(tidyverse)
library(ggmosaic)
library(haven)
library(rtf)

# Set path
setwd("Y:\\Respondi\\RESPONDI_w3\\")

##################################################################################
# Prepare data
##################################################################################

# load socio demographic information
back <- readRDS("./background_info/background_small.RDS")
# load tracking data (small)
tracking_small <- readRDS(file = "./data/general_usage_info_final.rds")
# load the tracking data (full)
tracking <- readRDS(file = "./data/apps_and_sites_final.rds")
# load the wave 3 survey data
survey_w3 <- read_dta(file = "./survey_daten/survey_data_w3.dta")
# load media data
news_media <- readRDS(file = "./data/news_media_final.RDS")
# load fake data
fake <- readRDS(file = "./data/fake_final.RDS")
# load fake data
oeffrecht <- readRDS(file = "./data/oeffrecht_final.rds")

# Select some more background information from survey data and merge to first background data
back2 <- select(survey_w3, panelist_id,
                why_unemp, child_in_hh)
back <- merge(back, back2, by = "panelist_id")
back$why_unemp <- as.factor(back$why_unemp)
back$child_in_hh <- as.factor(back$child_in_hh)
rm(back2)

# Fill all missings in categorial columns with "NONE", so we can keep them in our analysis
back[,c("gender", "state", "hh_size", "num_children", "net_inc", "hh_inc", "accom_type", "legal_status", 
        "edu_school", "edu_voc", "emp_type", "occup", "twn_size", "job_dep", "job_status", "industry", 
        "why_unemp", "child_in_hh")] <-
  lapply(back[,c("gender", "state", "hh_size", "num_children", "net_inc", "hh_inc", "accom_type", "legal_status", 
                 "edu_school", "edu_voc","emp_type", "occup", "twn_size", "job_dep", "job_status", "industry", 
                 "why_unemp", "child_in_hh")], function(back){`levels<-`(addNA(back),c(levels(back),"None"))})

# Clean factor levels
for(i in c("net_inc", "hh_inc", "accom_type", "gender", "state", "hh_size", "num_children", "edu_school", "edu_voc",
            "emp_type", "occup", "twn_size", "job_dep", "job_status", "industry", "why_unemp", "child_in_hh")){
  levels(back[,i]) <- gsub("[^a-zA-Z0-9]", "",levels(back[,i]))
}
back <- droplevels(back)

# put datasets together

X_back_track <- merge(back, fake, by = "panelist_id")
X_back_track <- merge(X_back_track, news_media, by = "panelist_id")
X_back_track <- merge(X_back_track, oeffrecht, by = "panelist_id")
X_back_track <- merge(X_back_track, tracking, by = "panelist_id")
X_back_track <- merge(X_back_track, tracking_small, by = "panelist_id")
names(X_back_track) <- gsub("[^a-zA-Z0-9]", "", names(X_back_track))
X_back_track <- X_back_track[, !duplicated(colnames(X_back_track))]

news_media <- merge(fake, news_media, by="panelist_id")
news_media <- merge(news_media, oeffrecht, by="panelist_id")
rm(oeffrecht, fake)

names(back) <- gsub("[^a-zA-Z0-9]", "", names(back))
names(news_media) <- gsub("[^a-zA-Z0-9]", "", names(news_media))
names(tracking) <- gsub("[^a-zA-Z0-9]", "", names(tracking))
names(tracking_small) <- gsub("[^a-zA-Z0-9]", "", names(tracking_small))

# Blocks of features
survey_noID <- back %>% 
  select(-panelistid)
survey_noID <- survey_noID[, !duplicated(colnames(survey_noID))]
survey_demo  <- names(survey_noID)
rm(survey_noID)

news_media_noID <- news_media %>% 
  select(-panelistid)
news_media_noID <- news_media_noID[, !duplicated(colnames(news_media_noID))]
track_news_media  <- names(news_media_noID)
rm(news_media_noID)

tracking <- tracking[, !duplicated(colnames(tracking))]
tracking_noID <- tracking %>% 
  select(-panelistid)
tracking_noID <- tracking_noID[, !duplicated(colnames(tracking_noID))]
track_apps_domains  <- names(tracking_noID)
rm(tracking_noID)

tracking_small <- tracking_small[, !duplicated(colnames(tracking_small))]
tracking_small_noID <- tracking_small %>% 
  select(-panelistid)
track_general  <- names(tracking_small_noID)
rm(tracking_small_noID)

# Attach Ys to X variables
Y <- survey_w3 %>% 
  select(c(voted, AFD, LEFT, party_affiliation, panelist_id)) %>% 
  rename(panelistid = panelist_id)

Y$voted <- as.factor(Y$voted)
levels(Y$voted) <- c("not_voted", "voted")
Y$AFD <- as.factor(Y$AFD)
levels(Y$AFD) <- c("not_AFD", "AFD")
Y$LEFT <- as.factor(Y$LEFT)
levels(Y$LEFT) <- c("not_LEFT", "LEFT")
Y$party_affiliation <- as.factor(Y$party_affiliation)
levels(Y$party_affiliation) <- c("CDU","SPD","GREEN","FDP","LEFT","AFD","Other")
levels(Y$party_affiliation) <- c(levels(Y$party_affiliation), "not voted")
Y$party_affiliation[Y$voted == "not_voted"] <- "not voted"

X_back_track <- merge(X_back_track, Y, by = "panelistid")

# Add undecided from wave 2
setwd("Y:\\Respondi\\RESPONDI_w2\\")
survey_w2 <- read_dta(file = "./survey_daten/survey_data_w2.dta")

Y2 <- survey_w2 %>% 
  select(c(undecided, panelist_id)) %>% 
  rename(panelistid = panelist_id)

Y2$undecided <- as.factor(Y2$undecided)
levels(Y2$undecided) <- c("decided", "undecided")

X_back_track <- merge(X_back_track, Y2, by = "panelistid")

##################################################################################
# Data exploration
##################################################################################

# Descriptive stats (outcomes)

summary(X_back_track$undecided)
summary(X_back_track$voted)
summary(X_back_track$AFD)
summary(X_back_track$LEFT)

p <- ggplot(X_back_track) +
  geom_mosaic(aes(x = product(undecided, party_affiliation), fill = party_affiliation), na.rm = TRUE) +
  labs(y = "", x = "") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))
p + geom_label(data = ggplot_build(p)$data[[1]], aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label = .wt))

ggsave("d_outcomes.png", width = 8, height = 6)

# Descriptive stats (outcomes and tracking general)

ggplot(X_back_track) +
  geom_mosaic(aes(x = product(mvevermob, party_affiliation), fill = party_affiliation, conds = product(undecided)), na.rm = TRUE) +
  labs(y = "", x = "") +
  theme(legend.position = "none")

ggplot(X_back_track) +
  geom_mosaic(aes(x = product(mvevertab, party_affiliation), fill = party_affiliation, conds = product(undecided)), na.rm = TRUE) +
  labs(y = "", x = "") +
  theme(legend.position = "none")

ggplot(X_back_track) +
  geom_boxplot(aes(x = party_affiliation, y = mvkindmobdur, fill = party_affiliation), na.rm = TRUE) +
  labs(y = "", x = "") +
  theme(legend.position = "none") + 
  coord_cartesian(ylim = c(0, 2000000)) +
  facet_grid(undecided ~ .)

ggplot(X_back_track) +
  geom_boxplot(aes(x = party_affiliation, y = mvkindmobn, fill = party_affiliation), na.rm = TRUE) +
  labs(y = "", x = "") +
  theme(legend.position = "none") + 
  coord_cartesian(ylim = c(0, 30000)) +
  facet_grid(undecided ~ .)
