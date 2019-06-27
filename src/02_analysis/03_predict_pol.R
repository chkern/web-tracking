##################################################################################

library(tidyverse)
library(haven)
library(caret)

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

# delete variables with hardly any variation

# zero_var_track <- nearZeroVar(tracking, freqCut = 99.95/0.05, uniqueCut = 0.1, names = TRUE, allowParallel = T)
# tracking <- tracking[ , -which(names(tracking) %in% zero_var_track)]
# rm(zero_var_track)

# put datasets together

X_back_track <- merge(back, fake, by = "panelist_id")
X_back_track <- merge(X_back_track, news_media, by = "panelist_id")
X_back_track <- merge(X_back_track, oeffrecht, by = "panelist_id")
X_back_track <- merge(X_back_track, tracking, by = "panelist_id")
X_back_track <- merge(X_back_track, tracking_small, by = "panelist_id")
names(X_back_track) <- gsub("[^a-zA-Z0-9]", "", names(X_back_track))
X_back_track <- X_back_track[, !duplicated(colnames(X_back_track))]

sum(is.na(X_back_track))
X_back_track[is.na(X_back_track)] <- 0

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
  select(c(voted, AFD, LEFT, CDU, SPD, GREEN, FDP, panelist_id)) %>% 
  rename(panelistid = panelist_id)

Y$voted <- as.factor(Y$voted)
levels(Y$voted) <- c("not_voted", "voted")
Y$AFD <- as.factor(Y$AFD)
levels(Y$AFD) <- c("not_AFD", "AFD")
Y$LEFT <- as.factor(Y$LEFT)
levels(Y$LEFT) <- c("not_LEFT", "LEFT")
Y$CDU <- as.factor(Y$CDU)
levels(Y$CDU) <- c("not_CDU", "CDU")
Y$SPD <- as.factor(Y$SPD)
levels(Y$SPD) <- c("not_SPD", "SPD")
Y$GREEN <- as.factor(Y$GREEN)
levels(Y$GREEN) <- c("not_GREEN", "GREEN")
Y$FDP <- as.factor(Y$FDP)
levels(Y$FDP) <- c("not_FDP", "FDP")

X_back_track <- merge(X_back_track, Y, by = "panelistid")

##################################################################################
# Train-test split
##################################################################################

set.seed(243082)
trainIndex <- sample(1:nrow(X_back_track), 0.75*nrow(X_back_track))

X_back_track_train <- X_back_track[trainIndex,]
X_back_track_test <- X_back_track[-trainIndex,]

save(X_back_track_train, X_back_track_test, 
     survey_demo,
     track_general,
     track_news_media,
     track_apps_domains,
     file = "prep_pol.Rdata")
