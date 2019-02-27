##################################################################################

library(tidyverse)
library(haven)
library(caret)
library(rpart)
library(MLmetrics)
library(e1071)
library(partykit)
library(xgboost)
library(ranger)
library(pROC)
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
  select(c(voted, AFD, LEFT, panelist_id)) %>% 
  rename(panelistid = panelist_id)

Y$voted <- as.factor(Y$voted)
levels(Y$voted) <- c("not_voted", "voted")
Y$AFD <- as.factor(Y$AFD)
levels(Y$AFD) <- c("not_AFD", "AFD")
Y$LEFT <- as.factor(Y$LEFT)
levels(Y$LEFT) <- c("not_LEFT", "LEFT")

X_back_track <- merge(X_back_track, Y, by = "panelistid")

##################################################################################
# Train-test split
##################################################################################

set.seed(243082)
trainIndex <- sample(1:nrow(X_back_track), 0.75*nrow(X_back_track))

X_back_track_train <- X_back_track[trainIndex,]
X_back_track_test <- X_back_track[-trainIndex,]

# Caret Setup

evalStats <- function(...) c(twoClassSummary(...),
                             defaultSummary(...),
                             mnLogLoss(...))

ctrl1  <- trainControl(method = "cv",
                       number = 10,
                       summaryFunction = evalStats,
                       classProbs = TRUE,
                       verboseIter = TRUE)

ctrl2  <- trainControl(method = "cv",
                       number = 10,
                       summaryFunction = multiClassSummary,
                       classProbs = TRUE,
                       verboseIter = TRUE)

#caretSBF$summary <- fiveStats
#sbf_ctrl <- sbfControl(functions = caretSBF,
#                       method = "cv",
#                       number = 10)

##################################################################################
# Models - Trees
##################################################################################

# Voted - track_news_media

X_back_track_train_c <- X_back_track_train[!is.na(X_back_track_train$voted),]

model_v1 <- paste("voted ~",paste(track_news_media,collapse="+"))

set.seed(543856)
eval(parse(text=paste("tree_v1 <- rpart(",model_v1,",
                      data = X_back_track_train_c,
                      control = rpart.control(minsplit = 10,
                      minbucket = 3,
                      cp = 0.001,
                      maxdepth = 4))")))

printcp(tree_v1)
party_tree_v1 <- as.party(tree_v1)
plot(party_tree_v1, gp = gpar(fontsize = 8.5))

# Voted - track_apps_domains

model_v2 <- paste("voted ~",paste(track_apps_domains,collapse="+"))

set.seed(543856)
eval(parse(text=paste("tree_v2 <- rpart(",model_v2,",
                      data = X_back_track_train_c,
                      control = rpart.control(minsplit = 10,
                      minbucket = 3,
                      cp = 0.001,
                      maxdepth = 4))")))

printcp(tree_v2)
party_tree_v2 <- as.party(tree_v2)
plot(party_tree_v2, gp = gpar(fontsize = 8.5))

# AFD - track_fake

X_back_track_train_c <- X_back_track_train[!is.na(X_back_track_train$AFD),]

model_a1 <- paste("AFD ~",paste(track_news_media,collapse="+"))

set.seed(543856)
eval(parse(text=paste("tree_a1 <- rpart(",model_a1,",
                      data = X_back_track_train_c,
                      control = rpart.control(minsplit = 10,
                      minbucket = 3,
                      cp = 0.001,
                      maxdepth = 4))")))

printcp(tree_a1)
party_tree_a1 <- as.party(tree_a1)
plot(party_tree_a1, gp = gpar(fontsize = 8.5))

# AFD - track_apps_domains

model_a2 <- paste("AFD ~",paste(track_apps_domains,collapse="+"))

set.seed(543856)
eval(parse(text=paste("tree_a2 <- rpart(",model_a2,",
                      data = X_back_track_train_c,
                      control = rpart.control(minsplit = 10,
                      minbucket = 3,
                      cp = 0.001,
                      maxdepth = 4))")))

printcp(tree_a2)
party_tree_a2 <- as.party(tree_a2)
plot(party_tree_a2, gp = gpar(fontsize = 8.5))

##################################################################################
# Models - XGBoost & RF
##################################################################################

# Voted

model_v1 <- paste("voted ~", paste(survey_demo, collapse="+"))
model_v2 <- paste(model_v1, paste("+"), paste(track_general, collapse="+"))
model_v3 <- paste(model_v1, paste("+"), paste(track_news_media, collapse="+"))
model_v4 <- paste(model_v1, paste("+"), paste(track_apps_domains, collapse="+"))
model_v5 <- paste("voted ~", paste(track_general, collapse="+"))
model_v5 <- paste(model_v5, paste("+"), paste(track_news_media, collapse="+"))
model_v5 <- paste(model_v5, paste("+"), paste(track_apps_domains, collapse="+"))
model_v6 <- paste(model_v5, paste("+"), paste(survey_demo, collapse="+"))

# RF Grid

small <- ncol(model.matrix(eval(parse(text=model_v1)), X_back_track_train))
med <- ncol(model.matrix(eval(parse(text=model_v2)), X_back_track_train))
full <-  small + length(track_apps_domains)

rf_grid <- expand.grid(mtry = c(round(sqrt(small)), round(log2(small)), round(sqrt(med)), round(log2(med)), round(sqrt(full)), round(log2(full))),
                       splitrule = c("gini", "extratrees"),
                       min.node.size = c(1, 5))

# XGBoost Grid

xgb_grid0 <- expand.grid(max_depth = c(1, 2, 3, 5, 7, 9),
                         nrounds = 100,
                         eta = 0.1,
                         min_child_weight = 1:5,
                         subsample = 0.7,
                         gamma = c(0, 0.5),
                         colsample_bytree = c(0.7, 1))

set.seed(303493)
eval(parse(text=paste("xgb_v0a <- train(",model_v1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid0,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_v0a
plot(xgb_v0a)

set.seed(303493)
eval(parse(text=paste("xgb_v0b <- train(",model_v6,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid0,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_v0b
plot(xgb_v0b)

xgb_grid <- expand.grid(max_depth = c(1, 2, 3, 5, 7, 9),
                        nrounds = c(500, 750, 1000, 1500, 2000),
                        eta = c(0.005, 0.01, 0.025),
                        min_child_weight = 5,
                        subsample = 0.7,
                        gamma = 0,
                        colsample_bytree = c(0.7, 1))

# Voted - survey_demo

set.seed(303493)
eval(parse(text=paste("xgb_v1 <- train(",model_v1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_v1
plot(xgb_v1)

set.seed(303493)
eval(parse(text=paste("rf_v1 <- train(",model_v1,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_v1
plot(rf_v1)

# Voted - survey_demo + track_general

set.seed(303493)
eval(parse(text=paste("xgb_v2 <- train(",model_v2,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_v2
plot(xgb_v2)

set.seed(303493)
eval(parse(text=paste("rf_v2 <- train(",model_v2,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_v2
plot(rf_v2)

# Voted - survey_demo + track_news_media

set.seed(303493)
eval(parse(text=paste("xgb_v3 <- train(",model_v3,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_v3
plot(xgb_v3)

set.seed(303493)
eval(parse(text=paste("rf_v3 <- train(",model_v3,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_v3
plot(rf_v3)

# Voted - survey_demo + track_apps_domains

set.seed(303493)
eval(parse(text=paste("xgb_v4 <- train(",model_v4,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_v4
plot(xgb_v4)

set.seed(303493)
eval(parse(text=paste("rf_v4 <- train(",model_v4,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_v4
plot(rf_v4)

# Voted - only tracking data

set.seed(303493)
eval(parse(text=paste("xgb_v5 <- train(",model_v5,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_v5
plot(xgb_v5)

set.seed(303493)
eval(parse(text=paste("rf_v5 <- train(",model_v5,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_v5
plot(rf_v5)

# Voted - tracking data + survey_demo

set.seed(303493)
eval(parse(text=paste("xgb_v6 <- train(",model_v6,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_v6
plot(xgb_v6)

set.seed(303493)
eval(parse(text=paste("rf_v6 <- train(",model_v6,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_v6
plot(rf_v6)

# Voted - tracking data + survey_demo w. feature selection

#set.seed(303493)
#eval(parse(text=paste("xgb_v8 <- sbf(",model_v7,",
#            data = X_back_track_train,
#            method = 'xgbTree',
#            trControl = ctrl1,
#            sbfControl = sbf_ctrl,
#            tuneGrid = xgb_grid,
#            metric = 'logLoss',
#            na.action = na.omit)")))

#xgb_v8
#xgb_v8$fit
#plot(xgb_v8$fit)
#plot(varImp(xgb_v8$fit), top = 10)

# AFD

model_a1 <- paste("AFD ~", paste(survey_demo, collapse="+"))
model_a2 <- paste(model_a1, paste("+"), paste(track_general, collapse="+"))
model_a3 <- paste(model_a1, paste("+"), paste(track_news_media, collapse="+"))
model_a4 <- paste(model_a1, paste("+"), paste(track_apps_domains, collapse="+"))
model_a5 <- paste("AFD ~", paste(track_general, collapse="+"))
model_a5 <- paste(model_a5, paste("+"), paste(track_news_media, collapse="+"))
model_a5 <- paste(model_a5, paste("+"), paste(track_apps_domains, collapse="+"))
model_a6 <- paste(model_a5, paste("+"), paste(survey_demo, collapse="+"))

# XGBoost Grid

xgb_grid0 <- expand.grid(max_depth = c(1, 2, 3, 5, 7, 9),
                         nrounds = 100,
                         eta = 0.1,
                         min_child_weight = 1:5,
                         subsample = 0.7,
                         gamma = c(0, 0.5),
                         colsample_bytree = c(0.7, 1))

set.seed(303493)
eval(parse(text=paste("xgb_a0a <- train(",model_a1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid0,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_a0a
plot(xgb_a0a)

set.seed(303493)
eval(parse(text=paste("xgb_a0b <- train(",model_a6,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid0,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_a0b
plot(xgb_a0b)

xgb_grid <- expand.grid(max_depth = c(1, 2, 3, 5, 7, 9),
                        nrounds = c(500, 750, 1000, 1500, 2000),
                        eta = c(0.005, 0.01, 0.025),
                        min_child_weight = 5,
                        subsample = 0.7,
                        gamma = 0,
                        colsample_bytree = c(0.7, 1))

# AFD - survey_demo

set.seed(303493)
eval(parse(text=paste("xgb_a1 <- train(",model_a1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_a1
plot(xgb_a1)

set.seed(303493)
eval(parse(text=paste("rf_a1 <- train(",model_a1,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_a1
plot(rf_a1)

# AFD - survey_demo + track_general

set.seed(303493)
eval(parse(text=paste("xgb_a2 <- train(",model_a2,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_a2
plot(xgb_va)

set.seed(303493)
eval(parse(text=paste("rf_a2 <- train(",model_a2,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_a2
plot(rf_a2)

# AFD - survey_demo + track_news_media

set.seed(303493)
eval(parse(text=paste("xgb_a3 <- train(",model_a3,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_a3
plot(xgb_a3)

set.seed(303493)
eval(parse(text=paste("rf_a3 <- train(",model_a3,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_a3
plot(rf_a3)

# AFD - survey_demo + track_apps_domains

set.seed(303493)
eval(parse(text=paste("xgb_a4 <- train(",model_a4,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_a4
plot(xgb_a4)

set.seed(303493)
eval(parse(text=paste("rf_a4 <- train(",model_a4,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_a4
plot(rf_a4)

# AFD - only tracking data

set.seed(303493)
eval(parse(text=paste("xgb_a5 <- train(",model_a5,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_a5
plot(xgb_a5)

set.seed(303493)
eval(parse(text=paste("rf_a5 <- train(",model_a5,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_a5
plot(rf_a5)

# AFD - tracking data + survey_demo

set.seed(303493)
eval(parse(text=paste("xgb_a6 <- train(",model_a6,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_a6
plot(xgb_a6)

set.seed(303493)
eval(parse(text=paste("rf_a6 <- train(",model_a6,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_a6
plot(rf_a6)

# LEFT

model_l1 <- paste("LEFT ~", paste(survey_demo, collapse="+"))
model_l2 <- paste(model_l1, paste("+"), paste(track_general, collapse="+"))
model_l3 <- paste(model_l1, paste("+"), paste(track_news_media, collapse="+"))
model_l4 <- paste(model_l1, paste("+"), paste(track_apps_domains, collapse="+"))
model_l5 <- paste("LEFT ~", paste(track_general, collapse="+"))
model_l5 <- paste(model_l5, paste("+"), paste(track_news_media, collapse="+"))
model_l5 <- paste(model_l5, paste("+"), paste(track_apps_domains, collapse="+"))
model_l6 <- paste(model_l5, paste("+"), paste(survey_demo, collapse="+"))

# XGBoost Grid

xgb_grid0 <- expand.grid(max_depth = c(1, 2, 3, 5, 7, 9),
                         nrounds = 100,
                         eta = 0.1,
                         min_child_weight = 1:5,
                         subsample = 0.7,
                         gamma = c(0, 0.5),
                         colsample_bytree = c(0.7, 1))

set.seed(303493)
eval(parse(text=paste("xgb_l0a <- train(",model_l1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid0,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_l0a
plot(xgb_l0a)

set.seed(303493)
eval(parse(text=paste("xgb_l0b <- train(",model_l6,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid0,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_l0b
plot(xgb_l0b)

xgb_grid <- expand.grid(max_depth = c(1, 2, 3, 5, 7, 9),
                        nrounds = c(500, 750, 1000, 1500, 2000),
                        eta = c(0.005, 0.01, 0.025),
                        min_child_weight = 5,
                        subsample = 0.7,
                        gamma = 0,
                        colsample_bytree = c(0.7, 1))

# LEFT - survey_demo

set.seed(303493)
eval(parse(text=paste("xgb_l1 <- train(",model_l1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_l1
plot(xgb_l1)

set.seed(303493)
eval(parse(text=paste("rf_l1 <- train(",model_l1,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_l1
plot(rf_l1)

# LEFT - survey_demo + track_general

set.seed(303493)
eval(parse(text=paste("xgb_l2 <- train(",model_l2,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_l2
plot(xgb_l2)

set.seed(303493)
eval(parse(text=paste("rf_l2 <- train(",model_l2,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_l2
plot(rf_l2)

# LEFT - survey_demo + track_news_media

set.seed(303493)
eval(parse(text=paste("xgb_l3 <- train(",model_l3,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_l3
plot(xgb_l3)

set.seed(303493)
eval(parse(text=paste("rf_l3 <- train(",model_l3,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_l3
plot(rf_l3)

# LEFT - survey_demo + track_apps_domains

set.seed(303493)
eval(parse(text=paste("xgb_l4 <- train(",model_l4,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_l4
plot(xgb_l4)

set.seed(303493)
eval(parse(text=paste("rf_l4 <- train(",model_l4,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_l4
plot(rf_l4)

# LEFT - only tracking data

set.seed(303493)
eval(parse(text=paste("xgb_l5 <- train(",model_l5,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_l5
plot(xgb_l5)

set.seed(303493)
eval(parse(text=paste("rf_l5 <- train(",model_l5,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_l5
plot(rf_l5)

# LEFT - tracking data + survey_demo

set.seed(303493)
eval(parse(text=paste("xgb_l6 <- train(",model_l6,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_l6
plot(xgb_l6)

set.seed(303493)
eval(parse(text=paste("rf_l6 <- train(",model_l6,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_l6
plot(rf_l6)

##################################################################################
# Variable Importance
##################################################################################

plot(varImp(xgb_v6), top = 10)

imp_xgb_v6 <- varImp(xgb_v6)$importance
imp_xgb_v6 <- rownames_to_column(imp_xgb_v6, "varname")

imp_xgb_v6 <-
  imp_xgb_v6 %>%
  top_n(10, Overall) %>%
  mutate(order = 11 - row_number())

match(imp_xgb_v6$varname, names(X_back_track_train))
imp_xgb_v6$varname <- c("High school degree","Why unemployed","News rel. n","Age","Manual Worker","amazon.de","In own house","youtube.com","Google Search","wahlomat.de")

ggplot(imp_xgb_v6) +
  geom_point(aes(x = Overall, y = order)) + 
  geom_segment(aes(y = order, yend = order, x = 1, xend = Overall)) +
  labs(x = "Importance", y = "") +
  xlim(0, 100) +
  scale_y_continuous(
    breaks = imp_xgb_v6$order,
    labels = imp_xgb_v6$varname)
ggsave("p_imp_v.png", width = 6, height = 6)

plot(varImp(xgb_a6), top = 10)

imp_xgb_a6 <- varImp(xgb_a6)$importance
imp_xgb_a6 <- rownames_to_column(imp_xgb_a6, "varname")

imp_xgb_a6 <-
  imp_xgb_a6 %>%
  top_n(10, Overall) %>%
  mutate(order = 11 - row_number())

match(imp_xgb_a6$varname, names(X_back_track_train))
imp_xgb_a6$varname <- c("Fake rel. d", "High school degree", "deref-web-02.de", "ebay.de", "Age", "hclips.com", "dropbox.com", "ikea.com", "eventim.de", "Gender")

ggplot(imp_xgb_a6) +
  geom_point(aes(x = Overall, y = order)) + 
  geom_segment(aes(y = order, yend = order, x = 1, xend = Overall)) +
  labs(x = "Importance", y = "") +
  xlim(0, 100) +
  scale_y_continuous(
    breaks = imp_xgb_a6$order,
    labels = imp_xgb_a6$varname)
ggsave("p_imp_a.png", width = 6, height = 6)

plot(varImp(xgb_l6), top = 10)

imp_xgb_l6 <- varImp(xgb_l6)$importance
imp_xgb_l6 <- rownames_to_column(imp_xgb_l6, "varname")

imp_xgb_l6 <-
  imp_xgb_l6 %>%
  top_n(10, Overall) %>%
  mutate(order = 11 - row_number())

match(imp_xgb_l6$varname, names(X_back_track_train))
imp_xgb_l6$varname <- c("vice.com", "DB Navigator", "twitter.com", "dict.cc", "zeit.de", "kinox.to", "Fb rel n", "Video", "tvmovie.de", "wikimedia.org")

ggplot(imp_xgb_l6) +
  geom_point(aes(x = Overall, y = order)) + 
  geom_segment(aes(y = order, yend = order, x = 1, xend = Overall)) +
  labs(x = "Importance", y = "") +
  xlim(0, 100) +
  scale_y_continuous(
    breaks = imp_xgb_l6$order,
    labels = imp_xgb_l6$varname)
ggsave("p_imp_l.png", width = 6, height = 6)

##################################################################################
# Compare CV performance
##################################################################################

# CV plot - voted

resamps1 <- resamples(list(xgb_v1, xgb_v2, xgb_v3, xgb_v4, xgb_v5, xgb_v6))
summary(resamps1)

resamp1 <- 
  reshape(resamps1$values,
          direction = "long",
          varying = 2:ncol(resamps1$values),
          sep = "~",
          v.names = c("Accuracy", "Kappa", "logLoss", "ROC", "Sens", "Spec"),
          timevar = "model")

resamp1 <- 
  resamp1 %>%
  mutate(model = factor(model)) %>%
  mutate(model = fct_recode(model,
                            "Demo" = "1",
                            "Demo+Track_general" = "2",
                            "Demo+Track_news" = "3",
                            "Demo+Track_domains_apps" = "4",
                            "Tracking" = "5",
                            "Demo+Tracking" = "6")) %>%
  mutate(model = fct_relevel(model, "Tracking", after = 1))

ggplot(resamp1) +
  geom_boxplot(aes(y = ROC, x = fct_rev(model), fill = model)) +
  ylim(0, 1) +
  labs(x = "") +
  labs(y = "ROC-AUC") +
  coord_flip() + 
  scale_fill_manual(values = c("#F8766D", "#619CFF", "#00BA38", "#00BA38", "#00BA38", "#00BA38")) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
ggsave("p_resamp_v1.png", width = 7.5, height = 7)

ggplot(resamp1) +
  geom_boxplot(aes(y = logLoss, x = fct_rev(model), fill = model)) +
  ylim(0.2, 0.5) +
  labs(x = "") +
  labs(y = "logLoss") +
  coord_flip() + 
  scale_fill_manual(values = c("#F8766D", "#619CFF", "#00BA38", "#00BA38", "#00BA38", "#00BA38")) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
ggsave("p_resamp_v2.png", width = 7.5, height = 7)

difresamps1 <- diff(resamps1)
summary(difresamps1)$table$ROC
summary(difresamps1)$table$logLoss

# CV plot - AFD

resamps2 <- resamples(list(xgb_a1, xgb_a2, xgb_a3, xgb_a4, xgb_a5, xgb_a6))
summary(resamps2)

resamp2 <- 
  reshape(resamps2$values,
          direction = "long",
          varying = 2:ncol(resamps2$values),
          sep = "~",
          v.names = c("Accuracy", "Kappa", "logLoss", "ROC", "Sens", "Spec"),
          timevar = "model")

resamp2 <- 
  resamp2 %>%
  mutate(model = factor(model)) %>%
  mutate(model = fct_recode(model,
                            "Demo" = "1",
                            "Demo+Track_general" = "2",
                            "Demo+Track_news" = "3",
                            "Demo+Track_domains_apps" = "4",
                            "Tracking" = "5",
                            "Demo+Tracking" = "6")) %>%
  mutate(model = fct_relevel(model, "Tracking", after = 1))

ggplot(resamp2) +
  geom_boxplot(aes(y = ROC, x = fct_rev(model), fill = model)) +
  ylim(0, 1) +
  labs(x = "") +
  labs(y = "ROC-AUC") +
  coord_flip() + 
  scale_fill_manual(values = c("#F8766D", "#619CFF", "#00BA38", "#00BA38", "#00BA38", "#00BA38")) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
ggsave("p_resamp_a1.png", width = 7.5, height = 7)

ggplot(resamp2) +
  geom_boxplot(aes(y = logLoss, x = fct_rev(model), fill = model)) +
  ylim(0.2, 0.5) +
  labs(x = "") +
  labs(y = "logLoss") +
  coord_flip() + 
  scale_fill_manual(values = c("#F8766D", "#619CFF", "#00BA38", "#00BA38", "#00BA38", "#00BA38")) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
ggsave("p_resamp_a2.png", width = 7.5, height = 7)

difresamps2 <- diff(resamps2)
summary(difresamps2)$table$ROC
summary(difresamps2)$table$logLoss

# CV plot - LEFT

resamps3 <- resamples(list(xgb_l1, xgb_l2, xgb_l3, xgb_l4, xgb_l5, xgb_l6))
summary(resamps3)

resamp3 <- 
  reshape(resamps3$values,
          direction = "long",
          varying = 2:ncol(resamps3$values),
          sep = "~",
          v.names = c("Accuracy", "Kappa", "logLoss", "ROC", "Sens", "Spec"),
          timevar = "model")

resamp3 <- 
  resamp3 %>%
  mutate(model = factor(model)) %>%
  mutate(model = fct_recode(model,
                            "Demo" = "1",
                            "Demo+Track_general" = "2",
                            "Demo+Track_news" = "3",
                            "Demo+Track_domains_apps" = "4",
                            "Tracking" = "5",
                            "Demo+Tracking" = "6")) %>%
  mutate(model = fct_relevel(model, "Tracking", after = 1))

ggplot(resamp3) +
  geom_boxplot(aes(y = ROC, x = fct_rev(model), fill = model)) +
  ylim(0, 1) +
  labs(x = "") +
  labs(y = "ROC-AUC") +
  coord_flip() + 
  scale_fill_manual(values = c("#F8766D", "#619CFF", "#00BA38", "#00BA38", "#00BA38", "#00BA38")) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
ggsave("p_resamp_l1.png", width = 7.5, height = 7)

ggplot(resamp3) +
  geom_boxplot(aes(y = logLoss, x = fct_rev(model), fill = model)) +
  ylim(0.2, 0.5) +
  labs(x = "") +
  labs(y = "logLoss") +
  coord_flip() + 
  scale_fill_manual(values = c("#F8766D", "#619CFF", "#00BA38", "#00BA38", "#00BA38", "#00BA38")) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
ggsave("p_resamp_l2.png", width = 7.5, height = 7)

difresamps3 <- diff(resamps3)
summary(difresamps3)$table$ROC
summary(difresamps3)$table$logLoss

##################################################################################
# Predict in test data
##################################################################################

# X_back_track_test_v <- X_back_track_test[!is.na(X_back_track_test$voted),]
p_xgb_v1 <- predict(xgb_v1, newdata = X_back_track_test, type = "prob")
p_xgb_v2 <- predict(xgb_v2, newdata = X_back_track_test, type = "prob")
p_xgb_v3 <- predict(xgb_v3, newdata = X_back_track_test, type = "prob")
p_xgb_v4 <- predict(xgb_v4, newdata = X_back_track_test, type = "prob")
p_xgb_v5 <- predict(xgb_v5, newdata = X_back_track_test, type = "prob")
p_xgb_v6 <- predict(xgb_v6, newdata = X_back_track_test, type = "prob")

p_xgb_a1 <- predict(xgb_a1, newdata = X_back_track_test, type = "prob")
p_xgb_a2 <- predict(xgb_a2, newdata = X_back_track_test, type = "prob")
p_xgb_a3 <- predict(xgb_a3, newdata = X_back_track_test, type = "prob")
p_xgb_a4 <- predict(xgb_a4, newdata = X_back_track_test, type = "prob")
p_xgb_a5 <- predict(xgb_a5, newdata = X_back_track_test, type = "prob")
p_xgb_a6 <- predict(xgb_a6, newdata = X_back_track_test, type = "prob")

p_xgb_l1 <- predict(xgb_l1, newdata = X_back_track_test, type = "prob")
p_xgb_l2 <- predict(xgb_l2, newdata = X_back_track_test, type = "prob")
p_xgb_l3 <- predict(xgb_l3, newdata = X_back_track_test, type = "prob")
p_xgb_l4 <- predict(xgb_l4, newdata = X_back_track_test, type = "prob")
p_xgb_l5 <- predict(xgb_l5, newdata = X_back_track_test, type = "prob")
p_xgb_l6 <- predict(xgb_l6, newdata = X_back_track_test, type = "prob")

# ROC and LogLoss - voted

roc_xgb_v1 <- roc(response = X_back_track_test$voted, predictor = p_xgb_v1$voted)
roc_xgb_v2 <- roc(response = X_back_track_test$voted, predictor = p_xgb_v2$voted)
roc_xgb_v3 <- roc(response = X_back_track_test$voted, predictor = p_xgb_v3$voted)
roc_xgb_v4 <- roc(response = X_back_track_test$voted, predictor = p_xgb_v4$voted)
roc_xgb_v5 <- roc(response = X_back_track_test$voted, predictor = p_xgb_v5$voted)
roc_xgb_v6 <- roc(response = X_back_track_test$voted, predictor = p_xgb_v6$voted)

ggroc(list("Demo" = roc_xgb_v1, "Tracking" = roc_xgb_v5, "Demo+Tracking" = roc_xgb_v6)) +
  geom_abline(aes(intercept = 1, slope = 1)) +
  scale_colour_manual(name = "", values = c("#F8766D", "#00BA38", "#619CFF"),
                      breaks = c("Demo", "Tracking", "Demo+Tracking")) +
  theme(text = element_text(size = 13))

ggsave("p_roc_v.png", width = 7.5, height = 6)

p_xgb_v1$obs <- X_back_track_test$voted
p_xgb_v1$pred <- predict(xgb_v1, newdata = X_back_track_test)
p_xgb_v2$obs <- X_back_track_test$voted
p_xgb_v2$pred <- predict(xgb_v2, newdata = X_back_track_test)
p_xgb_v3$obs <- X_back_track_test$voted
p_xgb_v3$pred <- predict(xgb_v3, newdata = X_back_track_test)
p_xgb_v4$obs <- X_back_track_test$voted
p_xgb_v4$pred <- predict(xgb_v4, newdata = X_back_track_test)
p_xgb_v5$obs <- X_back_track_test$voted
p_xgb_v5$pred <- predict(xgb_v5, newdata = X_back_track_test)
p_xgb_v6$obs <- X_back_track_test$voted
p_xgb_v6$pred <- predict(xgb_v6, newdata = X_back_track_test)

perf_v1 <- cbind(twoClassSummary(drop_na(p_xgb_v1), lev = levels(p_xgb_v1$obs))[[1]],
                 mnLogLoss(p_xgb_v1, lev = levels(p_xgb_v1$obs))[[1]])
perf_v2 <- cbind(twoClassSummary(drop_na(p_xgb_v2), lev = levels(p_xgb_v2$obs))[[1]],
                 mnLogLoss(p_xgb_v2, lev = levels(p_xgb_v2$obs))[[1]])
perf_v3 <- cbind(twoClassSummary(drop_na(p_xgb_v3), lev = levels(p_xgb_v3$obs))[[1]],
                 mnLogLoss(p_xgb_v3, lev = levels(p_xgb_v3$obs))[[1]])
perf_v4 <- cbind(twoClassSummary(drop_na(p_xgb_v4), lev = levels(p_xgb_v4$obs))[[1]],
                 mnLogLoss(p_xgb_v4, lev = levels(p_xgb_v4$obs))[[1]])
perf_v5 <- cbind(twoClassSummary(drop_na(p_xgb_v5), lev = levels(p_xgb_v5$obs))[[1]],
                 mnLogLoss(p_xgb_v5, lev = levels(p_xgb_v5$obs))[[1]])
perf_v6 <- cbind(twoClassSummary(drop_na(p_xgb_v6), lev = levels(p_xgb_v6$obs))[[1]],
                 mnLogLoss(p_xgb_v6, lev = levels(p_xgb_v6$obs))[[1]])

tab <- rbind(perf_v1, perf_v2, perf_v3, perf_v4, perf_v5, perf_v6)
tab

rtffile <- RTF("perf_v.doc")
addTable(rtffile, round(tab, digits = 3))
done(rtffile)

# ROC and LogLoss - AFD

roc_xgb_a1 <- roc(response = X_back_track_test$AFD, predictor = p_xgb_a1$AFD)
roc_xgb_a2 <- roc(response = X_back_track_test$AFD, predictor = p_xgb_a2$AFD)
roc_xgb_a3 <- roc(response = X_back_track_test$AFD, predictor = p_xgb_a3$AFD)
roc_xgb_a4 <- roc(response = X_back_track_test$AFD, predictor = p_xgb_a4$AFD)
roc_xgb_a5 <- roc(response = X_back_track_test$AFD, predictor = p_xgb_a5$AFD)
roc_xgb_a6 <- roc(response = X_back_track_test$AFD, predictor = p_xgb_a6$AFD)

ggroc(list("Demo" = roc_xgb_a1, "Tracking" = roc_xgb_a5, "Demo+Tracking" = roc_xgb_a6)) +
  geom_abline(aes(intercept = 1, slope = 1)) +
  scale_colour_manual(name = "", values = c("#F8766D", "#00BA38", "#619CFF"),
                      breaks = c("Demo", "Tracking", "Demo+Tracking")) +
  theme(text = element_text(size = 13))

ggsave("p_roc_a.png", width = 7.5, height = 6)

p_xgb_a1$obs <- X_back_track_test$AFD
p_xgb_a1$pred <- predict(xgb_a1, newdata = X_back_track_test)
p_xgb_a2$obs <- X_back_track_test$AFD
p_xgb_a2$pred <- predict(xgb_a2, newdata = X_back_track_test)
p_xgb_a3$obs <- X_back_track_test$AFD
p_xgb_a3$pred <- predict(xgb_a3, newdata = X_back_track_test)
p_xgb_a4$obs <- X_back_track_test$AFD
p_xgb_a4$pred <- predict(xgb_a4, newdata = X_back_track_test)
p_xgb_a5$obs <- X_back_track_test$AFD
p_xgb_a5$pred <- predict(xgb_a5, newdata = X_back_track_test)
p_xgb_a6$obs <- X_back_track_test$AFD
p_xgb_a6$pred <- predict(xgb_a6, newdata = X_back_track_test)

perf_a1 <- cbind(twoClassSummary(drop_na(p_xgb_a1), lev = levels(p_xgb_a1$obs))[[1]],
                 mnLogLoss(p_xgb_a1, lev = levels(p_xgb_a1$obs))[[1]])
perf_a2 <- cbind(twoClassSummary(drop_na(p_xgb_a2), lev = levels(p_xgb_a2$obs))[[1]],
                 mnLogLoss(p_xgb_a2, lev = levels(p_xgb_a2$obs))[[1]])
perf_a3 <- cbind(twoClassSummary(drop_na(p_xgb_a3), lev = levels(p_xgb_a3$obs))[[1]],
                 mnLogLoss(p_xgb_a3, lev = levels(p_xgb_a3$obs))[[1]])
perf_a4 <- cbind(twoClassSummary(drop_na(p_xgb_a4), lev = levels(p_xgb_a4$obs))[[1]],
                 mnLogLoss(p_xgb_a4, lev = levels(p_xgb_a4$obs))[[1]])
perf_a5 <- cbind(twoClassSummary(drop_na(p_xgb_a5), lev = levels(p_xgb_a5$obs))[[1]],
                 mnLogLoss(p_xgb_a5, lev = levels(p_xgb_a5$obs))[[1]])
perf_a6 <- cbind(twoClassSummary(drop_na(p_xgb_a6), lev = levels(p_xgb_a6$obs))[[1]],
                 mnLogLoss(p_xgb_a6, lev = levels(p_xgb_a6$obs))[[1]])

tab <- rbind(perf_a1, perf_a2, perf_a3, perf_a4, perf_a5, perf_a6)
tab

rtffile <- RTF("perf_a.doc")
addTable(rtffile, round(tab, digits = 3))
done(rtffile)

# ROC and LogLoss - LEFT

roc_xgb_l1 <- roc(response = X_back_track_test$LEFT, predictor = p_xgb_l1$LEFT)
roc_xgb_l2 <- roc(response = X_back_track_test$LEFT, predictor = p_xgb_l2$LEFT)
roc_xgb_l3 <- roc(response = X_back_track_test$LEFT, predictor = p_xgb_l3$LEFT)
roc_xgb_l4 <- roc(response = X_back_track_test$LEFT, predictor = p_xgb_l4$LEFT)
roc_xgb_l5 <- roc(response = X_back_track_test$LEFT, predictor = p_xgb_l5$LEFT)
roc_xgb_l6 <- roc(response = X_back_track_test$LEFT, predictor = p_xgb_l6$LEFT)

ggroc(list("Demo" = roc_xgb_l1, "Tracking" = roc_xgb_l5, "Demo+Tracking" = roc_xgb_l6)) +
  geom_abline(aes(intercept = 1, slope = 1)) +
  scale_colour_manual(name = "", values = c("#F8766D", "#00BA38", "#619CFF"),
                      breaks = c("Demo", "Tracking", "Demo+Tracking")) +
  theme(text = element_text(size = 13))

ggsave("p_roc_l.png", width = 7.5, height = 6)

p_xgb_l1$obs <- X_back_track_test$LEFT
p_xgb_l1$pred <- predict(xgb_l1, newdata = X_back_track_test)
p_xgb_l2$obs <- X_back_track_test$LEFT
p_xgb_l2$pred <- predict(xgb_l2, newdata = X_back_track_test)
p_xgb_l3$obs <- X_back_track_test$LEFT
p_xgb_l3$pred <- predict(xgb_l3, newdata = X_back_track_test)
p_xgb_l4$obs <- X_back_track_test$LEFT
p_xgb_l4$pred <- predict(xgb_l4, newdata = X_back_track_test)
p_xgb_l5$obs <- X_back_track_test$LEFT
p_xgb_l5$pred <- predict(xgb_l5, newdata = X_back_track_test)
p_xgb_l6$obs <- X_back_track_test$LEFT
p_xgb_l6$pred <- predict(xgb_l6, newdata = X_back_track_test)

perf_l1 <- cbind(twoClassSummary(drop_na(p_xgb_l1), lev = levels(p_xgb_l1$obs))[[1]],
                 mnLogLoss(p_xgb_l1, lev = levels(p_xgb_l1$obs))[[1]])
perf_l2 <- cbind(twoClassSummary(drop_na(p_xgb_l2), lev = levels(p_xgb_l2$obs))[[1]],
                 mnLogLoss(p_xgb_l2, lev = levels(p_xgb_l2$obs))[[1]])
perf_l3 <- cbind(twoClassSummary(drop_na(p_xgb_l3), lev = levels(p_xgb_l3$obs))[[1]],
                 mnLogLoss(p_xgb_l3, lev = levels(p_xgb_l3$obs))[[1]])
perf_l4 <- cbind(twoClassSummary(drop_na(p_xgb_l4), lev = levels(p_xgb_l4$obs))[[1]],
                 mnLogLoss(p_xgb_l4, lev = levels(p_xgb_l4$obs))[[1]])
perf_l5 <- cbind(twoClassSummary(drop_na(p_xgb_l5), lev = levels(p_xgb_l5$obs))[[1]],
                 mnLogLoss(p_xgb_l5, lev = levels(p_xgb_l5$obs))[[1]])
perf_l6 <- cbind(twoClassSummary(drop_na(p_xgb_l6), lev = levels(p_xgb_l6$obs))[[1]],
                 mnLogLoss(p_xgb_l6, lev = levels(p_xgb_l6$obs))[[1]])

tab <- rbind(perf_l1, perf_l2, perf_l3, perf_l4, perf_l5, perf_l6)
tab

rtffile <- RTF("perf_l.doc")
addTable(rtffile, round(tab, digits = 3))
done(rtffile)

# Performance at "optimal" threshold - voted

prop.table(table(X_back_track_train$voted))
roc_xgb_v1_t <- coords(roc_xgb_v1, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.9))

c_xgb_v1 <- as.factor(ifelse(p_xgb_v1$voted > roc_xgb_v1_t[[1]], "voted", "not_voted"))
c_xgb_v2 <- as.factor(ifelse(p_xgb_v2$voted > roc_xgb_v1_t[[1]], "voted", "not_voted"))
c_xgb_v3 <- as.factor(ifelse(p_xgb_v3$voted > roc_xgb_v1_t[[1]], "voted", "not_voted"))
c_xgb_v4 <- as.factor(ifelse(p_xgb_v4$voted > roc_xgb_v1_t[[1]], "voted", "not_voted"))
c_xgb_v5 <- as.factor(ifelse(p_xgb_v5$voted > roc_xgb_v1_t[[1]], "voted", "not_voted"))
c_xgb_v6 <- as.factor(ifelse(p_xgb_v6$voted > roc_xgb_v1_t[[1]], "voted", "not_voted"))

cm1 <- confusionMatrix(c_xgb_v1, X_back_track_test$voted, positive = "not_voted", mode = "everything")
cm2 <- confusionMatrix(c_xgb_v2, X_back_track_test$voted, positive = "not_voted", mode = "everything")
cm3 <- confusionMatrix(c_xgb_v3, X_back_track_test$voted, positive = "not_voted", mode = "everything")
cm4 <- confusionMatrix(c_xgb_v4, X_back_track_test$voted, positive = "not_voted", mode = "everything")
cm5 <- confusionMatrix(c_xgb_v5, X_back_track_test$voted, positive = "not_voted", mode = "everything")
cm6 <- confusionMatrix(c_xgb_v6, X_back_track_test$voted, positive = "not_voted", mode = "everything")

Demo <- c(cm1$overall[1], cm1$byClass[c(1:2,5,7)], cm1$overall[2])
Demo_Tracking_general <- c(cm2$overall[1], cm2$byClass[c(1:2,5,7)], cm2$overall[2])
Demo_Tracking_news <- c(cm3$overall[1], cm3$byClass[c(1:2,5,7)], cm3$overall[2])
Demo_Tracking_apps <- c(cm4$overall[1], cm4$byClass[c(1:2,5,7)], cm4$overall[2])
Tracking <- c(cm5$overall[1], cm5$byClass[c(1:2,5,7)], cm5$overall[2])
Demo_Tracking <- c(cm6$overall[1], cm6$byClass[c(1:2,5,7)], cm6$overall[2])

tab <- rbind(Demo, Tracking, Demo_Tracking_general, Demo_Tracking_news, Demo_Tracking_apps, Demo_Tracking)
tab

rtffile <- RTF("t_perf_v.doc")
addTable(rtffile, cbind(rownames(tab),round(tab, digits = 3)))
done(rtffile)

# Performance at "optimal" threshold - AFD

prop.table(table(X_back_track_train$AFD))
roc_xgb_a1_t <- coords(roc_xgb_a1, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.15))

c_xgb_a1 <- as.factor(ifelse(p_xgb_a1$AFD > roc_xgb_a1_t[[1]], "AFD", "not_AFD"))
c_xgb_a2 <- as.factor(ifelse(p_xgb_a2$AFD > roc_xgb_a1_t[[1]], "AFD", "not_AFD"))
c_xgb_a3 <- as.factor(ifelse(p_xgb_a3$AFD > roc_xgb_a1_t[[1]], "AFD", "not_AFD"))
c_xgb_a4 <- as.factor(ifelse(p_xgb_a4$AFD > roc_xgb_a1_t[[1]], "AFD", "not_AFD"))
c_xgb_a5 <- as.factor(ifelse(p_xgb_a5$AFD > roc_xgb_a1_t[[1]], "AFD", "not_AFD"))
c_xgb_a6 <- as.factor(ifelse(p_xgb_a6$AFD > roc_xgb_a1_t[[1]], "AFD", "not_AFD"))

cm1 <- confusionMatrix(c_xgb_a1, X_back_track_test$AFD, positive = "AFD", mode = "everything")
cm2 <- confusionMatrix(c_xgb_a2, X_back_track_test$AFD, positive = "AFD", mode = "everything")
cm3 <- confusionMatrix(c_xgb_a3, X_back_track_test$AFD, positive = "AFD", mode = "everything")
cm4 <- confusionMatrix(c_xgb_a4, X_back_track_test$AFD, positive = "AFD", mode = "everything")
cm5 <- confusionMatrix(c_xgb_a5, X_back_track_test$AFD, positive = "AFD", mode = "everything")
cm6 <- confusionMatrix(c_xgb_a6, X_back_track_test$AFD, positive = "AFD", mode = "everything")

Demo <- c(cm1$overall[1], cm1$byClass[c(1:2,5,7)], cm1$overall[2])
Demo_Tracking_general <- c(cm2$overall[1], cm2$byClass[c(1:2,5,7)], cm2$overall[2])
Demo_Tracking_news <- c(cm3$overall[1], cm3$byClass[c(1:2,5,7)], cm3$overall[2])
Demo_Tracking_apps <- c(cm4$overall[1], cm4$byClass[c(1:2,5,7)], cm4$overall[2])
Tracking <- c(cm5$overall[1], cm5$byClass[c(1:2,5,7)], cm5$overall[2])
Demo_Tracking <- c(cm6$overall[1], cm6$byClass[c(1:2,5,7)], cm6$overall[2])

tab <- rbind(Demo, Tracking, Demo_Tracking_general, Demo_Tracking_news, Demo_Tracking_apps, Demo_Tracking)
tab

rtffile <- RTF("t_perf_a.doc")
addTable(rtffile, cbind(rownames(tab),round(tab, digits = 3)))
done(rtffile)

# Performance at "optimal" threshold - LEFT

prop.table(table(X_back_track_train$LEFT))
roc_xgb_l1_t <- coords(roc_xgb_l1, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.15))

c_xgb_l1 <- as.factor(ifelse(p_xgb_l1$LEFT > roc_xgb_l1_t[[1]], "LEFT", "not_LEFT"))
c_xgb_l2 <- as.factor(ifelse(p_xgb_l2$LEFT > roc_xgb_l1_t[[1]], "LEFT", "not_LEFT"))
c_xgb_l3 <- as.factor(ifelse(p_xgb_l3$LEFT > roc_xgb_l1_t[[1]], "LEFT", "not_LEFT"))
c_xgb_l4 <- as.factor(ifelse(p_xgb_l4$LEFT > roc_xgb_l1_t[[1]], "LEFT", "not_LEFT"))
c_xgb_l5 <- as.factor(ifelse(p_xgb_l5$LEFT > roc_xgb_l1_t[[1]], "LEFT", "not_LEFT"))
c_xgb_l6 <- as.factor(ifelse(p_xgb_l6$LEFT > roc_xgb_l1_t[[1]], "LEFT", "not_LEFT"))

cm1 <- confusionMatrix(c_xgb_l1, X_back_track_test$LEFT, positive = "LEFT", mode = "everything")
cm2 <- confusionMatrix(c_xgb_l2, X_back_track_test$LEFT, positive = "LEFT", mode = "everything")
cm3 <- confusionMatrix(c_xgb_l3, X_back_track_test$LEFT, positive = "LEFT", mode = "everything")
cm4 <- confusionMatrix(c_xgb_l4, X_back_track_test$LEFT, positive = "LEFT", mode = "everything")
cm5 <- confusionMatrix(c_xgb_l5, X_back_track_test$LEFT, positive = "LEFT", mode = "everything")
cm6 <- confusionMatrix(c_xgb_l6, X_back_track_test$LEFT, positive = "LEFT", mode = "everything")

Demo <- c(cm1$overall[1], cm1$byClass[c(1:2,5,7)], cm1$overall[2])
Demo_Tracking_general <- c(cm2$overall[1], cm2$byClass[c(1:2,5,7)], cm2$overall[2])
Demo_Tracking_news <- c(cm3$overall[1], cm3$byClass[c(1:2,5,7)], cm3$overall[2])
Demo_Tracking_apps <- c(cm4$overall[1], cm4$byClass[c(1:2,5,7)], cm4$overall[2])
Tracking <- c(cm5$overall[1], cm5$byClass[c(1:2,5,7)], cm5$overall[2])
Demo_Tracking <- c(cm6$overall[1], cm6$byClass[c(1:2,5,7)], cm6$overall[2])

tab <- rbind(Demo, Tracking, Demo_Tracking_general, Demo_Tracking_news, Demo_Tracking_apps, Demo_Tracking)
tab

rtffile <- RTF("t_perf_l.doc")
addTable(rtffile, cbind(rownames(tab),round(tab, digits = 3)))
done(rtffile)

##################################################################################
# Predict in test data by subgroups
##################################################################################

# add party affiliation
Yadd3 <- select(survey_w3, party_affiliation, panelist_id)
Yadd3$party_affiliation <- as.factor(Yadd3$party_affiliation)
levels(Yadd3$party_affiliation) <- c("CDU","SPD","GREEN","FDP","LEFT","AFD","Other")

X_back_track_test <- left_join(X_back_track_test, Yadd3, by = c("panelistid" = "panelist_id"))

# add undecided from wave 2
setwd("Y:\\Respondi\\RESPONDI_w2\\")
survey_w2 <- read_dta(file = "./survey_daten/survey_data_w2.dta")
Yadd2 <- select(survey_w2, undecided, panelist_id)
Yadd2$undecided <- as.factor(Yadd2$undecided)
levels(Yadd2$undecided) <- c("decided", "undecided")

X_back_track_test <- left_join(X_back_track_test, Yadd2, by = c("panelistid" = "panelist_id"))

# AFD by undecided
confusionMatrix(c_xgb_a1[X_back_track_test$undecided == "decided"], X_back_track_test$AFD[X_back_track_test$undecided == "decided"], positive = "AFD", mode = "everything")
confusionMatrix(c_xgb_a1[X_back_track_test$undecided == "undecided"], X_back_track_test$AFD[X_back_track_test$undecided == "undecided"], positive = "AFD", mode = "everything")
confusionMatrix(c_xgb_a6[X_back_track_test$undecided == "decided"], X_back_track_test$AFD[X_back_track_test$undecided == "decided"], positive = "AFD", mode = "everything")
confusionMatrix(c_xgb_a6[X_back_track_test$undecided == "undecided"], X_back_track_test$AFD[X_back_track_test$undecided == "undecided"], positive = "AFD", mode = "everything")

# LEFT by undecided
confusionMatrix(c_xgb_l1[X_back_track_test$undecided == "decided"], X_back_track_test$LEFT[X_back_track_test$undecided == "decided"], positive = "LEFT", mode = "everything")
confusionMatrix(c_xgb_l1[X_back_track_test$undecided == "undecided"], X_back_track_test$LEFT[X_back_track_test$undecided == "undecided"], positive = "LEFT", mode = "everything")
confusionMatrix(c_xgb_l6[X_back_track_test$undecided == "decided"], X_back_track_test$LEFT[X_back_track_test$undecided == "decided"], positive = "LEFT", mode = "everything")
confusionMatrix(c_xgb_l6[X_back_track_test$undecided == "undecided"], X_back_track_test$LEFT[X_back_track_test$undecided == "undecided"], positive = "LEFT", mode = "everything")

# AFD by voting
addmargins(prop.table(table(c_xgb_a1, X_back_track_test$voted), 2))
addmargins(prop.table(table(c_xgb_a5, X_back_track_test$voted), 2))
addmargins(prop.table(table(c_xgb_a6, X_back_track_test$voted), 2))

# LEFT by voting
addmargins(prop.table(table(c_xgb_l1, X_back_track_test$voted), 2))
addmargins(prop.table(table(c_xgb_l5, X_back_track_test$voted), 2))
addmargins(prop.table(table(c_xgb_l6, X_back_track_test$voted), 2))

# party affiliation by AFD
tab1 <- rbind(table(c_xgb_a1, X_back_track_test$party_affiliation), prop.table(table(c_xgb_a1, X_back_track_test$party_affiliation), 1))
tab2 <- rbind(table(c_xgb_a5, X_back_track_test$party_affiliation), prop.table(table(c_xgb_a5, X_back_track_test$party_affiliation), 1))
tab3 <- rbind(table(c_xgb_a6, X_back_track_test$party_affiliation), prop.table(table(c_xgb_a6, X_back_track_test$party_affiliation), 1))

tab <- rbind(tab1, tab2, tab3)
tab

rownames(tab) <- c()
rtffile <- RTF("parties_a.doc")
addTable(rtffile, round(tab, digits = 3))
done(rtffile)

# party affiliation by LEFT
tab1 <- rbind(table(c_xgb_l1, X_back_track_test$party_affiliation), prop.table(table(c_xgb_l1, X_back_track_test$party_affiliation), 1))
tab2 <- rbind(table(c_xgb_l5, X_back_track_test$party_affiliation), prop.table(table(c_xgb_l5, X_back_track_test$party_affiliation), 1))
tab3 <- rbind(table(c_xgb_l6, X_back_track_test$party_affiliation), prop.table(table(c_xgb_l6, X_back_track_test$party_affiliation), 1))

tab <- rbind(tab1, tab2, tab3)
tab

rownames(tab) <- c()
rtffile <- RTF("parties_l.doc")
addTable(rtffile, round(tab, digits = 3))
done(rtffile)
