##################################################################################

library(plyr)
library(tidyverse)
library(haven)
library(dummies)
library(car)
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
setwd("~/Respondi/RESPONDI_w3")
# setwd("X:\\Respondi\\RESPONDI_w3")
# load the socio demographic information and limit to "standard" sociodemographics
load("./background_info/background.RData")
back <- select(background, panelist_id, m_1000, m_1001, m_1006, md_2806,
               md_0004, md_0006, md_1171, md_1172, md_1174, md_1175, md_1176,
               md_1181, md_1223, md_1264, md_1634, md_1635, md_1660, 
               md_2172, md_2189, md_2473, md_2861, md_1000)
back <- droplevels(back)
rm(background)

# load the tracking data (small)
tracking_small <- readRDS(file = "./data/data_prep_final_small.rds")

# load the tracking data (full)
tracking <- readRDS(file = "./data/data_prep_final.rds")
tracking <- tracking[,c(1,189:ncol(tracking))]

# load the wave 3 survey data
survey_w3 <- read_dta(file = "./survey_daten/survey_data_w3.dta")

##################################################################################
# Prepare data
##################################################################################

# Select some more background information from survey data and merge to first background data

back2 <- select(survey_w3, panelist_id,
                why_unemp, child_in_hh)
back <- merge(back, back2, by = "panelist_id")
back$why_unemp <- as.factor(back$why_unemp)
back$child_in_hh <- as.factor(back$child_in_hh)
rm(back2)

# Rename to more meaningful variable names

back <- rename(back,
               age = md_2806,
               gender = md_0004,
               state = md_0006,
               hh_size = md_1172,
               num_children = md_1174,
               net_inc = m_1000,
               hh_inc = m_1001,
               accom_type = m_1006,
               legal_status = md_1171,
               edu_school = md_1175,
               edu_voc = md_1176,
               emp_type = md_1181,
               occup = md_1223,
               twn_size = md_1264,
               job_dep = md_1634,
               job_status = md_1635,
               no_empees = md_1660,
               num_cars = md_2172,
               num_bike = md_2189,
               insur_priv  = md_2473,
               insur_pub  = md_2861,
               industry = md_1000)

# Fill all missings in categorial columns with "NONE", so we can keep them in our analysis

back[,c("gender", "state", "hh_size", "num_children", "net_inc", "hh_inc", "accom_type", "legal_status", 
        "edu_school", "edu_voc", "emp_type", "occup", "twn_size", "job_dep", "job_status", "no_empees",
        "num_cars", "num_bike", "insur_priv", "insur_pub", "industry", "why_unemp", "child_in_hh")] <-
    lapply(back[,c("gender", "state", "hh_size", "num_children", "net_inc", "hh_inc", "accom_type", "legal_status", 
                  "edu_school", "edu_voc","emp_type", "occup", "twn_size", "job_dep", "job_status", "no_empees",
                  "num_cars", "num_bike", "insur_priv", "insur_pub", "industry", "why_unemp", "child_in_hh")], function(back){`levels<-`(addNA(back),c(levels(back),"None"))})

# Clean factor levels

levels(back$gender) <- gsub("[^a-zA-Z0-9]", "", levels(back$gender))
levels(back$state) <- gsub("[^a-zA-Z0-9]", "", levels(back$state))
levels(back$hh_size) <- gsub("[^a-zA-Z0-9]", "", levels(back$hh_size))
levels(back$num_children) <- gsub("[^a-zA-Z0-9]", "", levels(back$num_children))
levels(back$net_inc) <- gsub("[^a-zA-Z0-9]", "", levels(back$net_inc))
levels(back$hh_inc) <- gsub("[^a-zA-Z0-9]", "", levels(back$hh_inc))
levels(back$accom_type) <- gsub("[^a-zA-Z0-9]", "", levels(back$accom_type))
levels(back$edu_school) <- gsub("[^a-zA-Z0-9]", "", levels(back$edu_school))
levels(back$edu_voc) <- gsub("[^a-zA-Z0-9]", "", levels(back$edu_voc))
levels(back$emp_type) <- gsub("[^a-zA-Z0-9]", "", levels(back$emp_type))
levels(back$occup) <- gsub("[^a-zA-Z0-9]", "", levels(back$occup))
levels(back$twn_size) <- gsub("[^a-zA-Z0-9]", "", levels(back$twn_size))
levels(back$edu_school) <- gsub("[^a-zA-Z0-9]", "", levels(back$edu_school))
levels(back$edu_voc) <- gsub("[^a-zA-Z0-9]", "", levels(back$edu_voc))
levels(back$job_dep) <- gsub("[^a-zA-Z0-9]", "", levels(back$job_dep))
levels(back$job_status) <- gsub("[^a-zA-Z0-9]", "", levels(back$job_status))
levels(back$no_empees) <- gsub("[^a-zA-Z0-9]", "", levels(back$no_empees))
levels(back$num_cars) <- gsub("[^a-zA-Z0-9]", "", levels(back$num_cars))
levels(back$num_bike) <- gsub("[^a-zA-Z0-9]", "", levels(back$num_bike))
levels(back$insur_priv) <- gsub("[^a-zA-Z0-9]", "", levels(back$insur_priv))
levels(back$insur_pub) <- gsub("[^a-zA-Z0-9]", "", levels(back$insur_pub))
levels(back$industry) <- gsub("[^a-zA-Z0-9]", "", levels(back$industry))
levels(back$why_unemp) <- gsub("[^a-zA-Z0-9]", "", levels(back$why_unemp))
levels(back$child_in_hh) <- gsub("[^a-zA-Z0-9]", "", levels(back$child_in_hh))
back <- droplevels(back)

# delete variables with hardly any variation

zero_var_track <- nearZeroVar(tracking, freqCut = 99.95/0.05, uniqueCut = 0.1, names = TRUE, allowParallel = T)
tracking <- tracking[ , -which(names(tracking) %in% zero_var_track)]
rm(zero_var_track)

# put datasets together

X_back_track <- merge(tracking_small, back, by = "panelist_id")
X_back_track <- merge(X_back_track, tracking, by = "panelist_id")

sum(is.na(X_back_track))
X_back_track[is.na(X_back_track)] <- 0

# Blocks of features (careful with column numbers)

background  <- names(X_back_track[409:432])
track_general <- names(X_back_track[2:164])
track_fb_news <- names(X_back_track[c(165:176,341:376)])
track_apps <- names(X_back_track[181:340])
track_oeff_fake <- names(X_back_track[377:407])

names(X_back_track)[433:ncol(X_back_track)] <- gsub("[^a-zA-Z0-9]", "", names(X_back_track)[433:ncol(X_back_track)])
X_back_track <- X_back_track[, !duplicated(colnames(X_back_track))]
track_domains <- names(X_back_track[433:ncol(X_back_track)])

# Attach Ys to X variables
Y <- survey_w3[,c(1,368:371)]

Y$voted <- as.factor(Y$voted)
levels(Y$voted) <- c("not_voted", "voted")
Y$party_affiliation <- as.factor(Y$party_affiliation)
levels(Y$party_affiliation) <- c("CDU","SPD","GREEN","FDP","LEFT","AFD","Other")
Y$AFD <- as.factor(Y$AFD)
levels(Y$AFD) <- c("not_AFD", "AFD")
Y$LEFT <- as.factor(Y$LEFT)
levels(Y$LEFT) <- c("not_LEFT", "LEFT")
  
X_back_track <- merge(X_back_track, Y, by = "panelist_id")

##################################################################################
# Data exploration
##################################################################################

X_back_track_e <- X_back_track

s <- summary(X_back_track_e$t_d_E_mails)
X_back_track_e$E_mails <- cut(X_back_track_e$t_d_E_mails, breaks = c(-1,0,s[5],s[6]))
s <- summary(X_back_track_e$t_d_Facebook)
X_back_track_e$Facebook <- cut(X_back_track_e$t_d_Facebook, breaks = c(-1,0,s[5],s[6]))
s <- summary(X_back_track_e$t_d_Games)
X_back_track_e$Games <- cut(X_back_track_e$t_d_Games, breaks = c(-1,0,s[5],s[6]))
s <- summary(X_back_track_e$t_d_News)
X_back_track_e$News <- cut(X_back_track_e$t_d_News, breaks = c(-1,0,s[5],s[6]))
s <- summary(X_back_track_e$t_d_Shopping)
X_back_track_e$Shopping <- cut(X_back_track_e$t_d_Shopping, breaks = c(-1,0,s[5],s[6]))
s <- summary(X_back_track_e$t_d_Social_Media)
X_back_track_e$Social_Media <- cut(X_back_track_e$t_d_Social_Media, breaks = c(-1,0,s[5],s[6]))
s <- summary(X_back_track_e$t_d_WhatsApp)
X_back_track_e$WhatsApp <- cut(X_back_track_e$t_d_WhatsApp, breaks = c(-1,0,s[5],s[6]))
s <- summary(X_back_track_e$t_d_YouTube)
X_back_track_e$YouTube <- cut(X_back_track_e$t_d_YouTube, breaks = c(-1,0,s[5],s[6]))

ggplot(X_back_track_e, aes(E_mails)) + geom_bar(aes(fill=voted))
ggplot(X_back_track_e, aes(Facebook)) + geom_bar(aes(fill=voted))
ggplot(X_back_track_e, aes(Games)) + geom_bar(aes(fill=voted))
ggplot(X_back_track_e, aes(News)) + geom_bar(aes(fill=voted))
ggplot(X_back_track_e, aes(Shopping)) + geom_bar(aes(fill=voted))
ggplot(X_back_track_e, aes(Social_Media)) + geom_bar(aes(fill=voted))
ggplot(X_back_track_e, aes(WhatsApp)) + geom_bar(aes(fill=voted))
ggplot(X_back_track_e, aes(YouTube)) + geom_bar(aes(fill=voted))

##################################################################################
# Train-test split
##################################################################################

set.seed(243082)
trainIndex <- sample(1:nrow(X_back_track), 0.75*nrow(X_back_track))

X_back_track_train <- X_back_track[trainIndex,]
X_back_track_test <- X_back_track[-trainIndex,]

# Caret Setup

fiveStats <- function(...) c(twoClassSummary(...),
                             defaultSummary(...))

ctrl1  <- trainControl(method = "cv",
                       number = 10,
                       summaryFunction = fiveStats,
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

# Voted - track_fb_news

X_back_track_train_c <- X_back_track_train[!is.na(X_back_track_train$voted),]

model_v1 <- paste("voted ~",paste(track_fb_news,collapse="+"))

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

# Voted - track_apps

model_v2 <- paste("voted ~",paste(track_apps,collapse="+"))

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

# Voted - track_oeff_fake

model_v3 <- paste("voted ~",paste(track_oeff_fake,collapse="+"))

set.seed(543856)
eval(parse(text=paste("tree_v3 <- rpart(",model_v3,",
                data = X_back_track_train_c,
                control = rpart.control(minsplit = 10,
                                       minbucket = 3,
                                       cp = 0.001,
                                       maxdepth = 4))")))

printcp(tree_v3)
party_tree_v3 <- as.party(tree_v3)
plot(party_tree_v3, gp = gpar(fontsize = 8.5))

# Voted - track_domains

model_v4 <- paste("voted ~",paste(track_domains,collapse="+"))

set.seed(543856)
eval(parse(text=paste("tree_v4 <- rpart(",model_v4,",
                data = X_back_track_train_c,
                control = rpart.control(minsplit = 10,
                                       minbucket = 3,
                                       cp = 0.001,
                                       maxdepth = 4))")))

printcp(tree_v4)
party_tree_v4 <- as.party(tree_v4)
plot(party_tree_v4, gp = gpar(fontsize = 8.5))

# AFD - track_oeff_fake

X_back_track_train_c <- X_back_track_train[!is.na(X_back_track_train$AFD),]

model_a3 <- paste("AFD ~",paste(track_oeff_fake,collapse="+"))

set.seed(543856)
eval(parse(text=paste("tree_a3 <- rpart(",model_a3,",
                data = X_back_track_train_c,
                control = rpart.control(minsplit = 10,
                                       minbucket = 3,
                                       cp = 0.001,
                                       maxdepth = 4))")))

printcp(tree_a3)
party_tree_a3 <- as.party(tree_a3)
plot(party_tree_a3, gp = gpar(fontsize = 8.5))

# AFD - track_domains

model_a4 <- paste("AFD ~",paste(track_domains,collapse="+"))

set.seed(543856)
eval(parse(text=paste("tree_a4 <- rpart(",model_a4,",
                data = X_back_track_train_c,
                control = rpart.control(minsplit = 10,
                                       minbucket = 3,
                                       cp = 0.001,
                                       maxdepth = 4))")))

printcp(tree_a4)
party_tree_a4 <- as.party(tree_a4)
plot(party_tree_a4, gp = gpar(fontsize = 8.5))

# LEFT - track_oeff_fake

model_l3 <- paste("LEFT ~",paste(track_oeff_fake,collapse="+"))

set.seed(543856)
eval(parse(text=paste("tree_l3 <- rpart(",model_l3,",
                data = X_back_track_train_c,
                control = rpart.control(minsplit = 10,
                                       minbucket = 3,
                                       cp = 0.001,
                                       maxdepth = 4))")))

printcp(tree_l3)
party_tree_l3 <- as.party(tree_l3)
plot(party_tree_l3, gp = gpar(fontsize = 8.5))

# LEFT - track_domains

model_l4 <- paste("LEFT ~",paste(track_domains,collapse="+"))

set.seed(543856)
eval(parse(text=paste("tree_l4 <- rpart(",model_l4,",
                data = X_back_track_train_c,
                control = rpart.control(minsplit = 10,
                                       minbucket = 3,
                                       cp = 0.001,
                                       maxdepth = 4))")))

printcp(tree_l4)
party_tree_l4 <- as.party(tree_l4)
plot(party_tree_l4, gp = gpar(fontsize = 8.5))

##################################################################################
# Models - XGBoost
##################################################################################

# Voted

model_v1 <- paste("voted ~",paste(background,collapse="+"))
model_v2 <- paste(model_v1,paste("+"),paste(track_general,collapse="+"))
model_v3 <- paste(model_v1,paste("+"),paste(track_fb_news,collapse="+"))
model_v4 <- paste(model_v1,paste("+"),paste(track_apps,collapse="+"))
model_v5 <- paste(model_v1,paste("+"),paste(track_oeff_fake,collapse="+"))
model_v6 <- paste("voted ~",paste(track_general,collapse="+"))
model_v6 <- paste(model_v6,paste("+"),paste(track_fb_news,collapse="+"))
model_v6 <- paste(model_v6,paste("+"),paste(track_apps,collapse="+"))
model_v6 <- paste(model_v6,paste("+"),paste(track_oeff_fake,collapse="+"))
model_v6 <- paste(model_v6,paste("+"),paste(track_domains,collapse="+"))
model_v7 <- paste(model_v6,paste("+"),paste(background,collapse="+"))

small <- ncol(model.matrix(eval(parse(text=model_v1)), X_back_track_train))
med <- ncol(model.matrix(eval(parse(text=model_v2)), X_back_track_train))
full <-  small + length(track_domains)

xgb_grid <- expand.grid(max_depth = c(1, 2, 3, 5, 7),
                        nrounds = c(500, 750, 1000, 1250, 1500),
                        eta = c(0.005, 0.01, 0.025),
                        min_child_weight = 5,
                        subsample = 0.7,
                        gamma = 0,
                        colsample_bytree = c(0.7, 1))

rf_grid <- expand.grid(mtry = c(round(sqrt(small)), round(log2(small)), round(sqrt(med)), round(log2(med)), round(sqrt(full)), round(log2(full))),
                       splitrule = c("gini", "extratrees"),
                       min.node.size = c(1, 5))

# Voted - background

set.seed(303493)
eval(parse(text=paste("xgb_v1 <- train(",model_v1,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = xgb_grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_v1
plot(xgb_v1)

set.seed(303493)
eval(parse(text=paste("rf_v1 <- train(",model_v1,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_v1
plot(rf_v1)

# Voted - background + track_general

set.seed(303493)
eval(parse(text=paste("xgb_v2 <- train(",model_v2,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = xgb_grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_v2
plot(xgb_v2)

set.seed(303493)
eval(parse(text=paste("rf_v2 <- train(",model_v2,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_v2
plot(rf_v2)

# Voted - background + track_fb_news

set.seed(303493)
eval(parse(text=paste("xgb_v3 <- train(",model_v3,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = xgb_grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_v3
plot(xgb_v3)

set.seed(303493)
eval(parse(text=paste("rf_v3 <- train(",model_v3,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_v3
plot(rf_v3)

# Voted - background + track_apps

set.seed(303493)
eval(parse(text=paste("xgb_v4 <- train(",model_v4,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = xgb_grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_v4
plot(xgb_v4)

set.seed(303493)
eval(parse(text=paste("rf_v4 <- train(",model_v4,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_v4
plot(rf_v4)

# Voted - background + track_oeff_fake

set.seed(303493)
eval(parse(text=paste("xgb_v5 <- train(",model_v5,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = xgb_grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_v5
plot(xgb_v5)

set.seed(303493)
eval(parse(text=paste("rf_v5 <- train(",model_v5,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_v5
plot(rf_v5)

# Voted - only tracking data

set.seed(303493)
eval(parse(text=paste("xgb_v6 <- train(",model_v6,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = xgb_grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_v6
plot(xgb_v6)

set.seed(303493)
eval(parse(text=paste("rf_v6 <- train(",model_v6,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_v6
plot(rf_v6)

# Voted - tracking full + background

set.seed(303493)
eval(parse(text=paste("xgb_v7 <- train(",model_v7,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = xgb_grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_v7
plot(xgb_v7)

set.seed(303493)
eval(parse(text=paste("rf_v7 <- train(",model_v7,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_v7
plot(rf_v7)

# Voted - tracking full + background w. feature selection

#set.seed(303493)
#eval(parse(text=paste("xgb_v8 <- sbf(",model_v7,",
#            data = X_back_track_train,
#            method = 'xgbTree',
#            trControl = ctrl1,
#            sbfControl = sbf_ctrl,
#            tuneGrid = xgb_grid,
#            metric = 'ROC',
#            na.action = na.omit)")))

#xgb_v8
#xgb_v8$fit
#plot(xgb_v8$fit)
#plot(varImp(xgb_v8$fit), top = 10)

# AFD

model_a1 <- paste("AFD ~",paste(background,collapse="+"))
model_a2 <- paste(model_a1,paste("+"),paste(track_general,collapse="+"))
model_a3 <- paste(model_a1,paste("+"),paste(track_fb_news,collapse="+"))
model_a4 <- paste(model_a1,paste("+"),paste(track_apps,collapse="+"))
model_a5 <- paste(model_a1,paste("+"),paste(track_oeff_fake,collapse="+"))
model_a6 <- paste("AFD ~",paste(track_general,collapse="+"))
model_a6 <- paste(model_a6,paste("+"),paste(track_fb_news,collapse="+"))
model_a6 <- paste(model_a6,paste("+"),paste(track_apps,collapse="+"))
model_a6 <- paste(model_a6,paste("+"),paste(track_oeff_fake,collapse="+"))
model_a6 <- paste(model_a6,paste("+"),paste(track_domains,collapse="+"))
model_a7 <- paste(model_a6,paste("+"),paste(background,collapse="+"))

# AFD - background

set.seed(303493)
eval(parse(text=paste("xgb_a1 <- train(",model_a1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_a1
plot(xgb_a1)

set.seed(303493)
eval(parse(text=paste("rf_a1 <- train(",model_a1,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_a1
plot(rf_a1)

# AFD - background + track_general

set.seed(303493)
eval(parse(text=paste("xgb_a2 <- train(",model_a2,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_a2
plot(xgb_va)

set.seed(303493)
eval(parse(text=paste("rf_a2 <- train(",model_a2,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_a2
plot(rf_a2)

# AFD - background + track_fb_news

set.seed(303493)
eval(parse(text=paste("xgb_a3 <- train(",model_a3,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_a3
plot(xgb_a3)

set.seed(303493)
eval(parse(text=paste("rf_a3 <- train(",model_a3,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_a3
plot(rf_a3)

# AFD - background + track_apps

set.seed(303493)
eval(parse(text=paste("xgb_a4 <- train(",model_a4,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = xgb_grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_a4
plot(xgb_a4)

set.seed(303493)
eval(parse(text=paste("rf_a4 <- train(",model_a4,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_a4
plot(rf_a4)

# AFD - background + track_oeff_fake

set.seed(303493)
eval(parse(text=paste("xgb_a5 <- train(",model_a5,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_a5
plot(xgb_a5)

set.seed(303493)
eval(parse(text=paste("rf_a5 <- train(",model_a5,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_a5
plot(rf_a5)

# AFD - only tracking data

set.seed(303493)
eval(parse(text=paste("xgb_a6 <- train(",model_a6,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = xgb_grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_a6
plot(xgb_a6)

set.seed(303493)
eval(parse(text=paste("rf_a6 <- train(",model_a6,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_a6
plot(rf_a6)

# AFD - tracking full + background

set.seed(303493)
eval(parse(text=paste("xgb_a7 <- train(",model_a7,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = xgb_grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_a7
plot(xgb_a7)

set.seed(303493)
eval(parse(text=paste("rf_a7 <- train(",model_a7,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_a7
plot(rf_a7)

# LEFT

model_l1 <- paste("LEFT ~",paste(background,collapse="+"))
model_l2 <- paste(model_l1,paste("+"),paste(track_general,collapse="+"))
model_l3 <- paste(model_l1,paste("+"),paste(track_fb_news,collapse="+"))
model_l4 <- paste(model_l1,paste("+"),paste(track_apps,collapse="+"))
model_l5 <- paste(model_l1,paste("+"),paste(track_oeff_fake,collapse="+"))
model_l6 <- paste("LEFT ~",paste(track_general,collapse="+"))
model_l6 <- paste(model_l6,paste("+"),paste(track_fb_news,collapse="+"))
model_l6 <- paste(model_l6,paste("+"),paste(track_apps,collapse="+"))
model_l6 <- paste(model_l6,paste("+"),paste(track_oeff_fake,collapse="+"))
model_l6 <- paste(model_l6,paste("+"),paste(track_domains,collapse="+"))
model_l7 <- paste(model_l6,paste("+"),paste(background,collapse="+"))

# LEFT - background

set.seed(303493)
eval(parse(text=paste("xgb_l1 <- train(",model_l1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_l1
plot(xgb_l1)

set.seed(303493)
eval(parse(text=paste("rf_l1 <- train(",model_l1,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_l1
plot(rf_l1)

# LEFT - background + track_general

set.seed(303493)
eval(parse(text=paste("xgb_l2 <- train(",model_l2,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_l2
plot(xgb_l2)

set.seed(303493)
eval(parse(text=paste("rf_l2 <- train(",model_l2,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_l2
plot(rf_l2)

# LEFT - background + track_fb_news

set.seed(303493)
eval(parse(text=paste("xgb_l3 <- train(",model_l3,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_l3
plot(xgb_l3)

set.seed(303493)
eval(parse(text=paste("rf_l3 <- train(",model_l3,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_l3
plot(rf_l3)

# LEFT - background + track_apps

set.seed(303493)
eval(parse(text=paste("xgb_l4 <- train(",model_l4,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = xgb_grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_l4
plot(xgb_l4)

set.seed(303493)
eval(parse(text=paste("rf_l4 <- train(",model_l4,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_l4
plot(rf_l4)

# LEFT - background + track_oeff_fake

set.seed(303493)
eval(parse(text=paste("xgb_l5 <- train(",model_l5,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_l5
plot(xgb_l5)

set.seed(303493)
eval(parse(text=paste("rf_l5 <- train(",model_l5,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_l5
plot(rf_l5)

# LEFT - only tracking data

set.seed(303493)
eval(parse(text=paste("xgb_l6 <- train(",model_l6,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = xgb_grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_l6
plot(xgb_l6)

set.seed(303493)
eval(parse(text=paste("rf_l6 <- train(",model_l6,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl1,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_l6
plot(rf_l6)

# LEFT - tracking full + background

set.seed(303493)
eval(parse(text=paste("xgb_l7 <- train(",model_l7,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = xgb_grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_l7
plot(xgb_l7)

set.seed(303493)
eval(parse(text=paste("rf_l7 <- train(",model_l7,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'ROC',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_l7
plot(rf_l7)

# Party affiliation

model_p1 <- paste("party_affiliation ~",paste(background,collapse="+"))
model_p5 <- paste(model_p1,paste("+"),paste(track_oeff_fake,collapse="+"))
model_p6 <- paste("party_affiliation ~",paste(track_general,collapse="+"))
model_p6 <- paste(model_p6,paste("+"),paste(track_fb_news,collapse="+"))
model_p6 <- paste(model_p6,paste("+"),paste(track_apps,collapse="+"))
model_p6 <- paste(model_p6,paste("+"),paste(track_oeff_fake,collapse="+"))
model_p6 <- paste(model_p6,paste("+"),paste(track_domains,collapse="+"))
model_p7 <- paste(model_p6,paste("+"),paste(background,collapse="+"))

# Party affiliation - background data

set.seed(303493)
eval(parse(text=paste("xgb_p1 <- train(",model_p1,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl2,
                tuneGrid = xgb_grid,
                metric = 'AUC',
                na.action = na.omit)")))

xgb_p1
plot(xgb_p1)
confusionMatrix(xgb_p1)

set.seed(303493)
eval(parse(text=paste("rf_p1 <- train(",model_p1,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl2,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_p1
plot(rf_p1)
confusionMatrix(rf_p1)

# Party affiliation - background + track_oeff_fake

set.seed(303493)
eval(parse(text=paste("xgb_p5 <- train(",model_p5,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl2,
                tuneGrid = xgb_grid,
                metric = 'AUC',
                na.action = na.omit)")))

xgb_p5
plot(xgb_p5)
confusionMatrix(xgb_p5)

set.seed(303493)
eval(parse(text=paste("rf_p5 <- train(",model_p5,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl2,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_p5
plot(rf_p5)
confusionMatrix(rf_p5)

# Party affiliation - only tracking data

set.seed(303493)
eval(parse(text=paste("xgb_p6 <- train(",model_p6,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl2,
                tuneGrid = xgb_grid,
                metric = 'AUC',
                na.action = na.omit)")))

xgb_p6
plot(xgb_p6)
confusionMatrix(xgb_p6)

set.seed(303493)
eval(parse(text=paste("rf_p6 <- train(",model_p6,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl2,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_p6
plot(rf_p6)
confusionMatrix(rf_p6)

# Party affiliation - tracking full + background

set.seed(303493)
eval(parse(text=paste("xgb_p7 <- train(",model_p7,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl2,
                tuneGrid = xgb_grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_p7
plot(xgb_p7)
confusionMatrix(xgb_p7)

set.seed(303493)
eval(parse(text=paste("rf_p7 <- train(",model_p7,",
                data = X_back_track_train,
                method = 'ranger',
                trControl = ctrl2,
                tuneGrid = rf_grid,
                metric = 'ROC',
                importance = 'impurity',
                na.action = na.omit)")))

rf_p7
plot(rf_p7)
confusionMatrix(rf_p7)

##################################################################################
# Variable Importance
##################################################################################

plot(varImp(xgb_v7), top = 10)

imp_xgb_v7 <- varImp(xgb_v7)$importance
imp_xgb_v7 <- rownames_to_column(imp_xgb_v7, "varname")

imp_xgb_v7 <-
  imp_xgb_v7 %>%
  top_n(10, Overall) %>%
  mutate(order = 11 - row_number())

match(imp_xgb_v7$varname, names(X_back_track_train))
imp_xgb_v7$varname <- c("Tracking news", "Demo: Manual Worker", "Demo: Age", "Demo: High school degree", "Demo: Own house", "Tracking news", "Tracking domain", "Tracking apps", "Tracking domain", "Tracking domain")

ggplot(imp_xgb_v7) +
  geom_point(aes(x = Overall, y = order)) + 
  geom_segment(aes(y = order, yend = order, x = 1, xend = Overall)) +
  labs(x = "Importance", y = "") +
  xlim(0, 100) +
  scale_y_continuous(
    breaks = imp_xgb_v7$order,
    labels = imp_xgb_v7$varname)

ggsave("p_imp_v7.png", width = 6, height = 6)

plot(varImp(xgb_a7), top = 10)

imp_xgb_a7 <- varImp(xgb_a7)$importance
imp_xgb_a7 <- rownames_to_column(imp_xgb_a7, "varname")

imp_xgb_a7 <-
  imp_xgb_a7 %>%
  top_n(10, Overall) %>%
  mutate(order = 11 - row_number())

match(imp_xgb_a7$varname, names(X_back_track_train))
imp_xgb_a7$varname <- c("Tracking fake", "Demo: High school degree", "Tracking domain", "Demo: Gender", "Tracking domain", "Tracking domain", "Tracking domain", "Tracking domain", "Tracking domain", "Tracking domain")

ggplot(imp_xgb_a7) +
  geom_point(aes(x = Overall, y = order)) + 
  geom_segment(aes(y = order, yend = order, x = 1, xend = Overall)) +
  labs(x = "Importance", y = "") +
  xlim(0, 100) +
  scale_y_continuous(
    breaks = imp_xgb_a7$order,
    labels = imp_xgb_a7$varname)

ggsave("p_imp_a7.png", width = 6, height = 6)

plot(varImp(xgb_l7), top = 10)

imp_xgb_l7 <- varImp(xgb_l7)$importance
imp_xgb_l7 <- rownames_to_column(imp_xgb_l7, "varname")

imp_xgb_l7 <-
  imp_xgb_l7 %>%
  top_n(10, Overall) %>%
  mutate(order = 11 - row_number())

match(imp_xgb_l7$varname, names(X_back_track_train))
imp_xgb_l7$varname <- c("Tracking domain", "Tracking fake", "Tracking news", "Tracking apps", "Tracking domain", "Tracking domain", "Tracking domain", "Tracking apps", "Tracking apps", "Tracking domain")

ggplot(imp_xgb_l7) +
  geom_point(aes(x = Overall, y = order)) + 
  geom_segment(aes(y = order, yend = order, x = 1, xend = Overall)) +
  labs(x = "Importance", y = "") +
  xlim(0, 100) +
  scale_y_continuous(
    breaks = imp_xgb_l7$order,
    labels = imp_xgb_l7$varname)

ggsave("p_imp_l7.png", width = 6, height = 6)

# plot(varImp(xgb_p7), top = 10)

##################################################################################
# Compare CV performance
##################################################################################

# CV plot - voted

resamps1 <- resamples(list(xgb_v1, xgb_v2, xgb_v3, xgb_v4, xgb_v5, xgb_v6, xgb_v7))
summary(resamps1)

resamp1 <- 
  reshape(resamps1$values,
          direction = "long",
          varying = 2:ncol(resamps1$values),
          sep = "~",
          v.names = c("Accuracy", "Kappa", "ROC", "Sens", "Spec"),
          timevar = "model")

resamp1 <- 
  resamp1 %>%
  mutate(model = factor(model)) %>%
  mutate(model = fct_recode(model,
                            "Demo" = "1",
                            "Demo+Tracking_general" = "2",
                            "Demo+Tracking_news" = "3",
                            "Demo+Tracking_apps" = "4",
                            "Demo+Tracking_fake" = "5",
                            "Tracking" = "6",
                            "Demo+Tracking" = "7")) %>%
  mutate(model = fct_relevel(model, "Tracking", after = 1))

p_resamp_v <- resamp1 %>%
  ggplot() +
  geom_boxplot(aes(y = ROC, x = fct_rev(model), fill = model)) +
  ylim(0, 1) +
  labs(x = "") +
  labs(y = "ROC-AUC") +
  coord_flip() + 
  scale_fill_manual(values = c("#F8766D", "#619CFF", "#00BA38", "#00BA38", "#00BA38", "#00BA38", "#00BA38")) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))

ggsave("p_resamp_v.png", p_resamp_v, width = 7.5, height = 7)

# CV plot - AFD

resamps2 <- resamples(list(xgb_a1, xgb_a2, xgb_a3, xgb_a4, xgb_a5, xgb_a6, xgb_a7))
summary(resamps2)

resamp2 <- 
  reshape(resamps2$values,
          direction = "long",
          varying = 2:ncol(resamps2$values),
          sep = "~",
          v.names = c("Accuracy", "Kappa", "ROC", "Sens", "Spec"),
          timevar = "model")

resamp2 <- 
  resamp2 %>%
  mutate(model = factor(model)) %>%
  mutate(model = fct_recode(model,
                            "Demo" = "1",
                            "Demo+Tracking_general" = "2",
                            "Demo+Tracking_news" = "3",
                            "Demo+Tracking_apps" = "4",
                            "Demo+Tracking_fake" = "5",
                            "Tracking" = "6",
                            "Demo+Tracking" = "7")) %>%
  mutate(model = fct_relevel(model, "Tracking", after = 1))

p_resamp_a <- resamp2 %>%
  ggplot() +
  geom_boxplot(aes(y = ROC, x = fct_rev(model), fill = model)) +
  ylim(0, 1) +
  labs(x = "") +
  labs(y = "ROC-AUC") +
  coord_flip() + 
  scale_fill_manual(values = c("#F8766D", "#619CFF", "#00BA38", "#00BA38", "#00BA38", "#00BA38", "#00BA38")) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))

ggsave("p_resamp_a.png", p_resamp_a, width = 7.5, height = 7)

# CV plot - LEFT

resamps3 <- resamples(list(xgb_l1, xgb_l2, xgb_l3, xgb_l4, xgb_l5, xgb_l6, xgb_l7))
summary(resamps3)

resamp3 <- 
  reshape(resamps3$values,
          direction = "long",
          varying = 2:ncol(resamps3$values),
          sep = "~",
          v.names = c("Accuracy", "Kappa", "ROC", "Sens", "Spec"),
          timevar = "model")

resamp3 <- 
  resamp3 %>%
  mutate(model = factor(model)) %>%
  mutate(model = fct_recode(model,
                            "Demo" = "1",
                            "Demo+Tracking_general" = "2",
                            "Demo+Tracking_news" = "3",
                            "Demo+Tracking_apps" = "4",
                            "Demo+Tracking_fake" = "5",
                            "Tracking" = "6",
                            "Demo+Tracking" = "7")) %>%
  mutate(model = fct_relevel(model, "Tracking", after = 1))

p_resamp_l <- resamp3 %>%
  ggplot() +
  geom_boxplot(aes(y = ROC, x = fct_rev(model), fill = model)) +
  ylim(0, 1) +
  labs(x = "") +
  labs(y = "ROC-AUC") +
  coord_flip() + 
  scale_fill_manual(values = c("#F8766D", "#619CFF", "#00BA38", "#00BA38", "#00BA38", "#00BA38", "#00BA38")) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))

ggsave("p_resamp_l.png", p_resamp_l, width = 7.5, height = 7)

##################################################################################
# Predict in test data
##################################################################################

X_back_track_test_v <- X_back_track_test[!is.na(X_back_track_test$voted),]
p_xgb_v1 <- predict(xgb_v1, newdata = X_back_track_test_v, type = "prob")
p_xgb_v2 <- predict(xgb_v2, newdata = X_back_track_test_v, type = "prob")
p_xgb_v3 <- predict(xgb_v3, newdata = X_back_track_test_v, type = "prob")
p_xgb_v4 <- predict(xgb_v4, newdata = X_back_track_test_v, type = "prob")
p_xgb_v5 <- predict(xgb_v5, newdata = X_back_track_test_v, type = "prob")
p_xgb_v6 <- predict(xgb_v6, newdata = X_back_track_test_v, type = "prob")
p_xgb_v7 <- predict(xgb_v7, newdata = X_back_track_test_v, type = "prob")

X_back_track_test_a <- X_back_track_test[!is.na(X_back_track_test$AFD),]
p_xgb_a1 <- predict(xgb_a1, newdata = X_back_track_test_a, type = "prob")
p_xgb_a2 <- predict(xgb_a2, newdata = X_back_track_test_a, type = "prob")
p_xgb_a3 <- predict(xgb_a3, newdata = X_back_track_test_a, type = "prob")
p_xgb_a4 <- predict(xgb_a4, newdata = X_back_track_test_a, type = "prob")
p_xgb_a5 <- predict(xgb_a5, newdata = X_back_track_test_a, type = "prob")
p_xgb_a6 <- predict(xgb_a6, newdata = X_back_track_test_a, type = "prob")
p_xgb_a7 <- predict(xgb_a7, newdata = X_back_track_test_a, type = "prob")

X_back_track_test_l <- X_back_track_test[!is.na(X_back_track_test$LEFT),]
p_xgb_l1 <- predict(xgb_l1, newdata = X_back_track_test_l, type = "prob")
p_xgb_l2 <- predict(xgb_l2, newdata = X_back_track_test_l, type = "prob")
p_xgb_l3 <- predict(xgb_l3, newdata = X_back_track_test_l, type = "prob")
p_xgb_l4 <- predict(xgb_l4, newdata = X_back_track_test_l, type = "prob")
p_xgb_l5 <- predict(xgb_l5, newdata = X_back_track_test_l, type = "prob")
p_xgb_l6 <- predict(xgb_l6, newdata = X_back_track_test_l, type = "prob")
p_xgb_l7 <- predict(xgb_l7, newdata = X_back_track_test_l, type = "prob")

X_back_track_test_p <- X_back_track_test[!is.na(X_back_track_test$party_affiliation),]
c_xgb_p1 <- predict(xgb_p1, newdata = X_back_track_test_p)
c_xgb_p5 <- predict(xgb_p5, newdata = X_back_track_test_p)
c_xgb_p6 <- predict(xgb_p6, newdata = X_back_track_test_p)
p_xgb_p7 <- predict(xgb_p7, newdata = X_back_track_test_p)

# ROC curves - voted

roc_xgb_v1 <- roc(response = X_back_track_test_v$voted, predictor = p_xgb_v1$voted)
roc_xgb_v2 <- roc(response = X_back_track_test_v$voted, predictor = p_xgb_v2$voted)
roc_xgb_v3 <- roc(response = X_back_track_test_v$voted, predictor = p_xgb_v3$voted)
roc_xgb_v4 <- roc(response = X_back_track_test_v$voted, predictor = p_xgb_v4$voted)
roc_xgb_v5 <- roc(response = X_back_track_test_v$voted, predictor = p_xgb_v5$voted)
roc_xgb_v6 <- roc(response = X_back_track_test_v$voted, predictor = p_xgb_v6$voted)
roc_xgb_v7 <- roc(response = X_back_track_test_v$voted, predictor = p_xgb_v7$voted)

ggroc(list("Demo" = roc_xgb_v1, "Tracking" = roc_xgb_v6, "Demo+Tracking" = roc_xgb_v7)) +
  geom_abline(aes(intercept = 1, slope = 1)) +
  scale_colour_manual(name = "", values = c("#F8766D", "#00BA38", "#619CFF"),
                      breaks = c("Demo", "Tracking", "Demo+Tracking"))

ggsave("p_roc_v.png", width = 7.5, height = 6)

# ROC curves - AFD

roc_xgb_a1 <- roc(response = X_back_track_test_a$AFD, predictor = p_xgb_a1$AFD)
roc_xgb_a2 <- roc(response = X_back_track_test_a$AFD, predictor = p_xgb_a2$AFD)
roc_xgb_a3 <- roc(response = X_back_track_test_a$AFD, predictor = p_xgb_a3$AFD)
roc_xgb_a4 <- roc(response = X_back_track_test_a$AFD, predictor = p_xgb_a4$AFD)
roc_xgb_a5 <- roc(response = X_back_track_test_a$AFD, predictor = p_xgb_a5$AFD)
roc_xgb_a6 <- roc(response = X_back_track_test_a$AFD, predictor = p_xgb_a6$AFD)
roc_xgb_a7 <- roc(response = X_back_track_test_a$AFD, predictor = p_xgb_a7$AFD)

ggroc(list("Demo" = roc_xgb_a1, "Tracking" = roc_xgb_a6, "Demo+Tracking" = roc_xgb_a7)) +
  geom_abline(aes(intercept = 1, slope = 1)) +
  scale_colour_manual(name = "", values = c("#F8766D", "#00BA38", "#619CFF"),
                      breaks = c("Demo", "Tracking", "Demo+Tracking"))

ggsave("p_roc_a.png", width = 7.5, height = 6)

# ROC curves - LEFT

roc_xgb_l1 <- roc(response = X_back_track_test_l$LEFT, predictor = p_xgb_l1$LEFT)
roc_xgb_l2 <- roc(response = X_back_track_test_l$LEFT, predictor = p_xgb_l2$LEFT)
roc_xgb_l3 <- roc(response = X_back_track_test_l$LEFT, predictor = p_xgb_l3$LEFT)
roc_xgb_l4 <- roc(response = X_back_track_test_l$LEFT, predictor = p_xgb_l4$LEFT)
roc_xgb_l5 <- roc(response = X_back_track_test_l$LEFT, predictor = p_xgb_l5$LEFT)
roc_xgb_l6 <- roc(response = X_back_track_test_l$LEFT, predictor = p_xgb_l6$LEFT)
roc_xgb_l7 <- roc(response = X_back_track_test_l$LEFT, predictor = p_xgb_l7$LEFT)

ggroc(list("Demo" = roc_xgb_l1, "Tracking" = roc_xgb_l6, "Demo+Tracking" = roc_xgb_l7)) +
  geom_abline(aes(intercept = 1, slope = 1)) +
  scale_colour_manual(name = "", values = c("#F8766D", "#00BA38", "#619CFF"),
                      breaks = c("Demo", "Tracking", "Demo+Tracking"))

ggsave("p_roc_l.png", width = 7.5, height = 6)

# Performance at "optimal" threshold - voted

prop.table(table(X_back_track_train$voted))
roc_xgb_v1_t <- coords(roc_xgb_v1, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.9))
roc_xgb_v2_t <- coords(roc_xgb_v2, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.9))
roc_xgb_v3_t <- coords(roc_xgb_v3, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.9))
roc_xgb_v4_t <- coords(roc_xgb_v4, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.9))
roc_xgb_v5_t <- coords(roc_xgb_v5, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.9))
roc_xgb_v6_t <- coords(roc_xgb_v6, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.9))
roc_xgb_v7_t <- coords(roc_xgb_v7, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.9))

c_xgb_v1 <- as.factor(ifelse(p_xgb_v1$voted > roc_xgb_v1_t[1], "voted", "not_voted"))
c_xgb_v2 <- as.factor(ifelse(p_xgb_v2$voted > roc_xgb_v2_t[1], "voted", "not_voted"))
c_xgb_v3 <- as.factor(ifelse(p_xgb_v3$voted > roc_xgb_v3_t[1], "voted", "not_voted"))
c_xgb_v4 <- as.factor(ifelse(p_xgb_v4$voted > roc_xgb_v4_t[1], "voted", "not_voted"))
c_xgb_v5 <- as.factor(ifelse(p_xgb_v5$voted > roc_xgb_v5_t[1], "voted", "not_voted"))
c_xgb_v6 <- as.factor(ifelse(p_xgb_v6$voted > roc_xgb_v6_t[1], "voted", "not_voted"))
c_xgb_v7 <- as.factor(ifelse(p_xgb_v7$voted > roc_xgb_v7_t[1], "voted", "not_voted"))

cm1 <- confusionMatrix(c_xgb_v1, X_back_track_test_v$voted, positive = "not_voted", mode = "everything")
cm2 <- confusionMatrix(c_xgb_v2, X_back_track_test_v$voted, positive = "not_voted", mode = "everything")
cm3 <- confusionMatrix(c_xgb_v3, X_back_track_test_v$voted, positive = "not_voted", mode = "everything")
cm4 <- confusionMatrix(c_xgb_v4, X_back_track_test_v$voted, positive = "not_voted", mode = "everything")
cm5 <- confusionMatrix(c_xgb_v5, X_back_track_test_v$voted, positive = "not_voted", mode = "everything")
cm6 <- confusionMatrix(c_xgb_v6, X_back_track_test_v$voted, positive = "not_voted", mode = "everything")
cm7 <- confusionMatrix(c_xgb_v7, X_back_track_test_v$voted, positive = "not_voted", mode = "everything")

Demo <- c(cm1$overall[1], cm1$byClass[c(1:2,5,7)], cm1$overall[2])
Demo_Tracking_general <- c(cm2$overall[1], cm2$byClass[c(1:2,5,7)], cm2$overall[2])
Demo_Tracking_news <- c(cm3$overall[1], cm3$byClass[c(1:2,5,7)], cm3$overall[2])
Demo_Tracking_apps <- c(cm4$overall[1], cm4$byClass[c(1:2,5,7)], cm4$overall[2])
Demo_Tracking_fake <- c(cm5$overall[1], cm5$byClass[c(1:2,5,7)], cm5$overall[2])
Tracking <- c(cm6$overall[1], cm6$byClass[c(1:2,5,7)], cm6$overall[2])
Demo_Tracking <- c(cm7$overall[1], cm7$byClass[c(1:2,5,7)], cm7$overall[2])

tab <- rbind(Demo, Tracking, Demo_Tracking_general, Demo_Tracking_news, Demo_Tracking_apps, Demo_Tracking_fake, Demo_Tracking)
tab

rtffile <- RTF("t_perf_v.doc")
addTable(rtffile, cbind(rownames(tab),round(tab, digits = 3)))
done(rtffile)

# Performance at "optimal" threshold - AFD

prop.table(table(X_back_track_train$AFD))
roc_xgb_a1_t <- coords(roc_xgb_a1, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.15))
roc_xgb_a2_t <- coords(roc_xgb_a2, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.15))
roc_xgb_a3_t <- coords(roc_xgb_a3, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.15))
roc_xgb_a4_t <- coords(roc_xgb_a4, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.15))
roc_xgb_a5_t <- coords(roc_xgb_a5, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.15))
roc_xgb_a6_t <- coords(roc_xgb_a6, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.15))
roc_xgb_a7_t <- coords(roc_xgb_a7, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.15))

c_xgb_a1 <- as.factor(ifelse(p_xgb_a1$AFD > roc_xgb_a1_t[1], "AFD", "not_AFD"))
c_xgb_a2 <- as.factor(ifelse(p_xgb_a2$AFD > roc_xgb_a2_t[1], "AFD", "not_AFD"))
c_xgb_a3 <- as.factor(ifelse(p_xgb_a3$AFD > roc_xgb_a3_t[1], "AFD", "not_AFD"))
c_xgb_a4 <- as.factor(ifelse(p_xgb_a4$AFD > roc_xgb_a4_t[1], "AFD", "not_AFD"))
c_xgb_a5 <- as.factor(ifelse(p_xgb_a5$AFD > roc_xgb_a5_t[1], "AFD", "not_AFD"))
c_xgb_a6 <- as.factor(ifelse(p_xgb_a6$AFD > roc_xgb_a6_t[1], "AFD", "not_AFD"))
c_xgb_a7 <- as.factor(ifelse(p_xgb_a7$AFD > roc_xgb_a7_t[1], "AFD", "not_AFD"))

cm1 <- confusionMatrix(c_xgb_a1, X_back_track_test_a$AFD, positive = "AFD", mode = "everything")
cm2 <- confusionMatrix(c_xgb_a2, X_back_track_test_a$AFD, positive = "AFD", mode = "everything")
cm3 <- confusionMatrix(c_xgb_a3, X_back_track_test_a$AFD, positive = "AFD", mode = "everything")
cm4 <- confusionMatrix(c_xgb_a4, X_back_track_test_a$AFD, positive = "AFD", mode = "everything")
cm5 <- confusionMatrix(c_xgb_a5, X_back_track_test_a$AFD, positive = "AFD", mode = "everything")
cm6 <- confusionMatrix(c_xgb_a6, X_back_track_test_a$AFD, positive = "AFD", mode = "everything")
cm7 <- confusionMatrix(c_xgb_a7, X_back_track_test_a$AFD, positive = "AFD", mode = "everything")

Demo <- c(cm1$overall[1], cm1$byClass[c(1:2,5,7)], cm1$overall[2])
Demo_Tracking_general <- c(cm2$overall[1], cm2$byClass[c(1:2,5,7)], cm2$overall[2])
Demo_Tracking_news <- c(cm3$overall[1], cm3$byClass[c(1:2,5,7)], cm3$overall[2])
Demo_Tracking_apps <- c(cm4$overall[1], cm4$byClass[c(1:2,5,7)], cm4$overall[2])
Demo_Tracking_fake <- c(cm5$overall[1], cm5$byClass[c(1:2,5,7)], cm5$overall[2])
Tracking <- c(cm6$overall[1], cm6$byClass[c(1:2,5,7)], cm6$overall[2])
Demo_Tracking <- c(cm7$overall[1], cm7$byClass[c(1:2,5,7)], cm7$overall[2])

tab <- rbind(Demo, Tracking, Demo_Tracking_general, Demo_Tracking_news, Demo_Tracking_apps, Demo_Tracking_fake, Demo_Tracking)
tab

rtffile <- RTF("t_perf_a.doc")
addTable(rtffile, cbind(rownames(tab),round(tab, digits = 3)))
done(rtffile)

# Performance at "optimal" threshold - LEFT

prop.table(table(X_back_track_train$LEFT))
roc_xgb_l1_t <- coords(roc_xgb_l1, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.15))
roc_xgb_l2_t <- coords(roc_xgb_l2, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.15))
roc_xgb_l3_t <- coords(roc_xgb_l3, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.15))
roc_xgb_l4_t <- coords(roc_xgb_l4, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.15))
roc_xgb_l5_t <- coords(roc_xgb_l5, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.15))
roc_xgb_l6_t <- coords(roc_xgb_l6, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.15))
roc_xgb_l7_t <- coords(roc_xgb_l7, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.15))

c_xgb_l1 <- as.factor(ifelse(p_xgb_l1$LEFT > roc_xgb_l1_t[1], "LEFT", "not_LEFT"))
c_xgb_l2 <- as.factor(ifelse(p_xgb_l2$LEFT > roc_xgb_l2_t[1], "LEFT", "not_LEFT"))
c_xgb_l3 <- as.factor(ifelse(p_xgb_l3$LEFT > roc_xgb_l3_t[1], "LEFT", "not_LEFT"))
c_xgb_l4 <- as.factor(ifelse(p_xgb_l4$LEFT > roc_xgb_l4_t[1], "LEFT", "not_LEFT"))
c_xgb_l5 <- as.factor(ifelse(p_xgb_l5$LEFT > roc_xgb_l5_t[1], "LEFT", "not_LEFT"))
c_xgb_l6 <- as.factor(ifelse(p_xgb_l6$LEFT > roc_xgb_l6_t[1], "LEFT", "not_LEFT"))
c_xgb_l7 <- as.factor(ifelse(p_xgb_l7$LEFT > roc_xgb_l7_t[1], "LEFT", "not_LEFT"))

cm1 <- confusionMatrix(c_xgb_l1, X_back_track_test_l$LEFT, positive = "LEFT", mode = "everything")
cm2 <- confusionMatrix(c_xgb_l2, X_back_track_test_l$LEFT, positive = "LEFT", mode = "everything")
cm3 <- confusionMatrix(c_xgb_l3, X_back_track_test_l$LEFT, positive = "LEFT", mode = "everything")
cm4 <- confusionMatrix(c_xgb_l4, X_back_track_test_l$LEFT, positive = "LEFT", mode = "everything")
cm5 <- confusionMatrix(c_xgb_l5, X_back_track_test_l$LEFT, positive = "LEFT", mode = "everything")
cm6 <- confusionMatrix(c_xgb_l6, X_back_track_test_l$LEFT, positive = "LEFT", mode = "everything")
cm7 <- confusionMatrix(c_xgb_l7, X_back_track_test_l$LEFT, positive = "LEFT", mode = "everything")

Demo <- c(cm1$overall[1], cm1$byClass[c(1:2,5,7)], cm1$overall[2])
Demo_Tracking_general <- c(cm2$overall[1], cm2$byClass[c(1:2,5,7)], cm2$overall[2])
Demo_Tracking_news <- c(cm3$overall[1], cm3$byClass[c(1:2,5,7)], cm3$overall[2])
Demo_Tracking_apps <- c(cm4$overall[1], cm4$byClass[c(1:2,5,7)], cm4$overall[2])
Demo_Tracking_fake <- c(cm5$overall[1], cm5$byClass[c(1:2,5,7)], cm5$overall[2])
Tracking <- c(cm6$overall[1], cm6$byClass[c(1:2,5,7)], cm6$overall[2])
Demo_Tracking <- c(cm7$overall[1], cm7$byClass[c(1:2,5,7)], cm7$overall[2])

tab <- rbind(Demo, Tracking, Demo_Tracking_general, Demo_Tracking_news, Demo_Tracking_apps, Demo_Tracking_fake, Demo_Tracking)
tab

rtffile <- RTF("t_perf_l.doc")
addTable(rtffile, cbind(rownames(tab),round(tab, digits = 3)))
done(rtffile)
