##################################################################################

library(plyr)
library(tidyverse)
library(foreign)
library(haven)
library(data.table)
library(dummies)
library(car)
library(caret)
library(partykit)
library(xgboost)
library(PRROC)

# Set path
setwd("X:\\Respondi\\RESPONDI_w3")

# load the socio demographic information and limit to "standard" sociodemographics
load(".\\background_info\\background.RData")
back <- select(background, panelist_id, m_1000, m_1001, m_1006, md_2806,
               md_0004, md_0006, md_1171, md_1172, md_1174, md_1175, md_1176,
               md_1181, md_1185, md_1223, md_1264, md_1634, md_1635, md_1660, 
               md_2172, md_2189, md_2473, md_2861, md_1000)
back$panelist_id <- as.character(back$panelist_id)
back <- droplevels(back)
rm(background)

# load the tracking data (small)
tracking_small <- readRDS(file = ".\\data\\data_prep_final_small.rds")

# load the tracking data (full)
tracking <- readRDS(file = ".\\data\\data_prep_final.rds")
tracking <- tracking[,c(1,189:ncol(tracking))]

# load the wave 3 survey data
survey_w3 <- read_dta(file = "survey_data_w3.dta")
survey_w3$panelist_id <- as.character(survey_w3$panelist_id)

survey_w3$sinus_mil <- NULL
survey_w3$sinus_submil <- NULL
survey_w3$weight_ges <- NULL

##################################################################################
# Prepare data
##################################################################################

# Select some more background information from survey data and merge to first background data
back2 <- select(survey_w3, panelist_id, age, f_state, education, occ_pos,
                why_unemp, size_hometown, hh_size, children, num_child_hh,
                male, west_ger, employed, child_in_hh)
back <- merge(back, back2, by="panelist_id", all = TRUE)
back <- select(back, -c(md_2806, md_0004, md_1172, md_0006, md_1174))

# Rename to more meaningful variable names
back <- rename(back,
               net_inc = m_1000,
               hh_inc = m_1001,
               accom_type = m_1006,
               legal_status = md_1171,
               edu_school = md_1175,
               edu_voc = md_1176,
               emp_type = md_1181,
               nationality = md_1185,
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
rm(back2)

# delete variables with hardly any variation
zero_var_back <- nearZeroVar(back, freqCut = 99.9/0.1, names = TRUE, allowParallel = T)
table(back$nationality)
back <- back[ , -which(names(back) %in% zero_var_back)]
rm(zero_var_back)

zero_var_track <- nearZeroVar(tracking, freqCut = 99.95/0.05, names = TRUE, allowParallel = T)
tracking <- tracking[ , -which(names(tracking) %in% zero_var_track)]
rm(zero_var_track)

# Some variables in the background data have weird "labelled" class
# correctly assign labelled class to factor class
subset_colclasses <- function(DF, colclasses="labelled") {
  DF[,sapply(DF, function(vec, test) class(vec) %in% test, test=colclasses)]
}

background_lab <- subset_colclasses(back)
background_lab <- as.data.frame(lapply(background_lab, as_factor, levels = "both"))
background_wo <- intersect(colnames(back),colnames(background_lab))
back <- back[,!(names(back) %in% background_wo)]
back <- cbind(back,background_lab)
rm(background_wo, background_lab, subset_colclasses)

# Fill all missings in categorial columns with "NONE", so we can keep them in our analysis
back[,c("net_inc", "hh_inc", "accom_type", "legal_status", "edu_school", "edu_voc",
        "emp_type", "occup", "twn_size", "job_dep", "job_status", "no_empees",
        "num_cars", "num_bike", "insur_priv", "insur_pub", "industry")] <-
    lapply(back[,c("net_inc", "hh_inc", "accom_type", "legal_status", "edu_school",
                  "edu_voc","emp_type", "occup", "twn_size", "job_dep", "job_status",
                  "no_empees","num_cars", "num_bike", "insur_priv", "insur_pub", 
                  "industry")], function(back){`levels<-`(addNA(back),c(levels(back),"None"))})

# Clean factor levels

levels(back$net_inc) <- gsub("[^a-zA-Z0-9]", "", levels(back$net_inc))
levels(back$hh_inc) <- gsub("[^a-zA-Z0-9]", "", levels(back$hh_inc))
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
levels(back$f_state) <- gsub("[^a-zA-Z0-9]", "", levels(back$f_state))
levels(back$education) <- gsub("[^a-zA-Z0-9]", "", levels(back$education))
levels(back$occ_pos) <- gsub("[^a-zA-Z0-9]", "", levels(back$occ_pos))
levels(back$why_unemp) <- gsub("[^a-zA-Z0-9]", "", levels(back$why_unemp))

# put datasets together

X_back_track <- merge(tracking_small, back, by="panelist_id")
X_back_track <- merge(X_back_track, tracking, by="panelist_id")

summary(is.na(X_back_track))
X_back_track[is.na(X_back_track)] <- 0

# Blocks of features (careful with column numbers)

background  <- names(X_back_track[409:438])
track_general <- names(X_back_track[2:164])
track_fb_news <- names(X_back_track[c(165:176,341:376)])
track_apps <- names(X_back_track[181:340])
track_oeff_fake <- names(X_back_track[377:407])

names(X_back_track)[439:ncol(X_back_track)] <- gsub("[^a-zA-Z0-9]", "", names(X_back_track)[439:ncol(X_back_track)])
X_back_track <- X_back_track[, !duplicated(colnames(X_back_track))]
track_domains <- names(X_back_track[439:ncol(X_back_track)])

# Attach Ys to X variables
Y <- survey_w3[,c(1,365:368)]

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

set.seed(100289)
trainIndex <- sample(1:nrow(X_back_track), 0.8*nrow(X_back_track))

X_back_track_train <- X_back_track[trainIndex,]
X_back_track_test <- X_back_track[-trainIndex,]

# Caret Setup

ctrl1  <- trainControl(method = "cv",
                       number = 10,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE,
                       verboseIter = TRUE)

ctrl2  <- trainControl(method = "cv",
                       number = 10,
                       summaryFunction = multiClassSummary,
                       classProbs = TRUE,
                       verboseIter = TRUE)

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

grid <- expand.grid(max_depth = c(1, 2, 3, 5, 7),
                    nrounds = c(500, 750, 1000, 1250, 1500),
                    eta = c(0.005, 0.01, 0.025),
                    min_child_weight = 5,
                    subsample = 0.7,
                    gamma = 0,
                    colsample_bytree = c(0.7, 1))

# Voted - background

model_v1 <- paste("voted ~",paste(background,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_v1 <- train(",model_v1,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_v1
plot(xgb_v1)
confusionMatrix(xgb_v1)

# Voted - background + track_general

model_v2 <- paste(model_v1,paste("+"),paste(track_general,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_v2 <- train(",model_v2,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_v2
plot(xgb_v2)
confusionMatrix(xgb_v2)

# Voted - background + track_fb_news

model_v3 <- paste(model_v1,paste("+"),paste(track_fb_news,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_v3 <- train(",model_v3,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_v3
plot(xgb_v3)
confusionMatrix(xgb_v3)

# Voted - background + track_apps

model_v4 <- paste(model_v1,paste("+"),paste(track_apps,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_v4 <- train(",model_v4,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_v4
plot(xgb_v4)
confusionMatrix(xgb_v4)

# Voted - background + track_oeff_fake

model_v5 <- paste(model_v1,paste("+"),paste(track_oeff_fake,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_v5 <- train(",model_v5,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_v5
plot(xgb_v5)
confusionMatrix(xgb_v5)

# Voted - only tracking data

model_v6 <- paste("voted ~",paste(track_general,collapse="+"))
model_v6 <- paste(model_v6,paste("+"),paste(track_fb_news,collapse="+"))
model_v6 <- paste(model_v6,paste("+"),paste(track_apps,collapse="+"))
model_v6 <- paste(model_v6,paste("+"),paste(track_oeff_fake,collapse="+"))
model_v6 <- paste(model_v6,paste("+"),paste(track_domains,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_v6 <- train(",model_v6,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_v6
plot(xgb_v6)
confusionMatrix(xgb_v6)

# AFD - background

model_a1 <- paste("AFD ~",paste(background,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_a1 <- train(",model_a1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_a1
plot(xgb_a1)
confusionMatrix(xgb_a1)

# AFD - background + track_general

model_a2 <- paste(model_a1,paste("+"),paste(track_general,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_a2 <- train(",model_a2,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_a2
plot(xgb_va)
confusionMatrix(xgb_a2)

# AFD - background + track_fb_news

model_a3 <- paste(model_a1,paste("+"),paste(track_fb_news,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_a3 <- train(",model_a3,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_a3
plot(xgb_a3)
confusionMatrix(xgb_a3)

# AFD - background + track_apps

model_a4 <- paste(model_a1,paste("+"),paste(track_apps,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_a4 <- train(",model_a4,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_a4
plot(xgb_a4)
confusionMatrix(xgb_a4)

# AFD - background + track_oeff_fake

model_a5 <- paste(model_a1,paste("+"),paste(track_oeff_fake,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_a5 <- train(",model_a5,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_a5
plot(xgb_a5)
confusionMatrix(xgb_a5)

# AFD - only tracking data

model_a6 <- paste("AFD ~",paste(track_general,collapse="+"))
model_a6 <- paste(model_a6,paste("+"),paste(track_fb_news,collapse="+"))
model_a6 <- paste(model_a6,paste("+"),paste(track_apps,collapse="+"))
model_a6 <- paste(model_a6,paste("+"),paste(track_oeff_fake,collapse="+"))
model_a6 <- paste(model_a6,paste("+"),paste(track_domains,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_a6 <- train(",model_a6,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_a6
plot(xgb_a6)
confusionMatrix(xgb_a6)

# LEFT - background

model_l1 <- paste("LEFT ~",paste(background,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_l1 <- train(",model_l1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_l1
plot(xgb_l1)
confusionMatrix(xgb_l1)

# LEFT - background + track_general

model_l2 <- paste(model_l1,paste("+"),paste(track_general,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_l2 <- train(",model_l2,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_l2
plot(xgb_l2)
confusionMatrix(xgb_l2)

# LEFT - background + track_fb_news

model_l3 <- paste(model_l1,paste("+"),paste(track_fb_news,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_l3 <- train(",model_l3,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_l3
plot(xgb_l3)
confusionMatrix(xgb_l3)

# LEFT - background + track_apps

model_l4 <- paste(model_l1,paste("+"),paste(track_apps,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_l4 <- train(",model_l4,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_l4
plot(xgb_l4)
confusionMatrix(xgb_l4)

# LEFT - background + track_oeff_fake

model_l5 <- paste(model_l1,paste("+"),paste(track_oeff_fake,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_l5 <- train(",model_l5,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_l5
plot(xgb_l5)
confusionMatrix(xgb_l5)

# LEFT - only tracking data

model_l6 <- paste("LEFT ~",paste(track_general,collapse="+"))
model_l6 <- paste(model_l6,paste("+"),paste(track_fb_news,collapse="+"))
model_l6 <- paste(model_l6,paste("+"),paste(track_apps,collapse="+"))
model_l6 <- paste(model_l6,paste("+"),paste(track_oeff_fake,collapse="+"))
model_l6 <- paste(model_l6,paste("+"),paste(track_domains,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_l6 <- train(",model_l6,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl1,
                tuneGrid = grid,
                metric = 'ROC',
                na.action = na.omit)")))

xgb_l6
plot(xgb_l6)
confusionMatrix(xgb_l6)

# Party affiliation - background data

model_p1 <- paste("party_affiliation ~",paste(background,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_p1 <- train(",model_p1,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl2,
                tuneGrid = grid,
                metric = 'AUC',
                na.action = na.omit)")))

xgb_p1
plot(xgb_p1)
confusionMatrix(xgb_p1)

# Party affiliation - background + track_oeff_fake

model_p5 <- paste(model_p1,paste("+"),paste(track_oeff_fake,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_p5 <- train(",model_p5,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl2,
                tuneGrid = grid,
                metric = 'AUC',
                na.action = na.omit)")))

xgb_p5
plot(xgb_p5)
confusionMatrix(xgb_p5)

# Party affiliation - only tracking data

model_p6 <- paste("party_affiliation ~",paste(track_general,collapse="+"))
model_p6 <- paste(model_p6,paste("+"),paste(track_fb_news,collapse="+"))
model_p6 <- paste(model_p6,paste("+"),paste(track_apps,collapse="+"))
model_p6 <- paste(model_p6,paste("+"),paste(track_oeff_fake,collapse="+"))
model_p6 <- paste(model_p6,paste("+"),paste(track_domains,collapse="+"))

set.seed(303493)
eval(parse(text=paste("xgb_p6 <- train(",model_p6,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl2,
                tuneGrid = grid,
                metric = 'AUC',
                na.action = na.omit)")))

xgb_p6
plot(xgb_p6)
confusionMatrix(xgb_p6)

##################################################################################
# Compare CV performance
##################################################################################

resamps1 <- resamples(list(XGBoost1 = xgb_v1,
                           XGBoost2 = xgb_v2,
                           XGBoost3 = xgb_v3,
                           XGBoost4 = xgb_v4,
                           XGBoost5 = xgb_v5,
                           XGBoost6 = xgb_v6))

summary(resamps1)
bwplot(resamps1, metric = "ROC", xlim = c(0,1))

resamps2 <- resamples(list(XGBoost1 = xgb_a1,
                           XGBoost2 = xgb_a2,
                           XGBoost3 = xgb_a3,
                           XGBoost4 = xgb_a4,
                           XGBoost5 = xgb_a5,
                           XGBoost6 = xgb_a6))

summary(resamps2)
bwplot(resamps2, metric = "ROC", xlim = c(0,1))

resamps3 <- resamples(list(XGBoost1 = xgb_l1,
                           XGBoost2 = xgb_l2,
                           XGBoost3 = xgb_l3,
                           XGBoost4 = xgb_l4,
                           XGBoost5 = xgb_l5,
                           XGBoost6 = xgb_l6))

summary(resamps3)
bwplot(resamps3, metric = "ROC", xlim = c(0,1))

resamps4 <- resamples(list(XGBoost1 = xgb_p1,
                           XGBoost5 = xgb_p5,
                           XGBoost6 = xgb_p6))

summary(resamps4)
bwplot(resamps4, metric = c("AUC", "prAUC"), xlim = c(0,1))

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

X_back_track_test_a <- X_back_track_test[!is.na(X_back_track_test$AFD),]
p_xgb_a1 <- predict(xgb_a1, newdata = X_back_track_test_a, type = "prob")
p_xgb_a2 <- predict(xgb_a2, newdata = X_back_track_test_a, type = "prob")
p_xgb_a3 <- predict(xgb_a3, newdata = X_back_track_test_a, type = "prob")
p_xgb_a4 <- predict(xgb_a4, newdata = X_back_track_test_a, type = "prob")
p_xgb_a5 <- predict(xgb_a5, newdata = X_back_track_test_a, type = "prob")
p_xgb_a6 <- predict(xgb_a6, newdata = X_back_track_test_a, type = "prob")

X_back_track_test_l <- X_back_track_test[!is.na(X_back_track_test$LEFT),]
p_xgb_l1 <- predict(xgb_l1, newdata = X_back_track_test_l, type = "prob")
p_xgb_l2 <- predict(xgb_l2, newdata = X_back_track_test_l, type = "prob")
p_xgb_l3 <- predict(xgb_l3, newdata = X_back_track_test_l, type = "prob")
p_xgb_l4 <- predict(xgb_l4, newdata = X_back_track_test_l, type = "prob")
p_xgb_l5 <- predict(xgb_l5, newdata = X_back_track_test_l, type = "prob")
p_xgb_l6 <- predict(xgb_l6, newdata = X_back_track_test_l, type = "prob")

X_back_track_test_l <- X_back_track_test[!is.na(X_back_track_test$party_affiliation),]
c_xgb_p1 <- predict(xgb_p1, newdata = X_back_track_test_p)
c_xgb_p5 <- predict(xgb_p5, newdata = X_back_track_test_p)
c_xgb_p6 <- predict(xgb_p6, newdata = X_back_track_test_p)

# ROC curves - voted

pos_v1 <- p_xgb_v1$voted[X_back_track_test_v$voted == "voted"]
neg_v1 <- p_xgb_v1$voted[X_back_track_test_v$voted == "not_voted"]
pos_v2 <- p_xgb_v2$voted[X_back_track_test_v$voted == "voted"]
neg_v2 <- p_xgb_v2$voted[X_back_track_test_v$voted == "not_voted"]
pos_v3 <- p_xgb_v3$voted[X_back_track_test_v$voted == "voted"]
neg_v3 <- p_xgb_v3$voted[X_back_track_test_v$voted == "not_voted"]
pos_v4 <- p_xgb_v4$voted[X_back_track_test_v$voted == "voted"]
neg_v4 <- p_xgb_v4$voted[X_back_track_test_v$voted == "not_voted"]
pos_v5 <- p_xgb_v5$voted[X_back_track_test_v$voted == "voted"]
neg_v5 <- p_xgb_v5$voted[X_back_track_test_v$voted == "not_voted"]
pos_v6 <- p_xgb_v6$voted[X_back_track_test_v$voted == "voted"]
neg_v6 <- p_xgb_v6$voted[X_back_track_test_v$voted == "not_voted"]

roc_v1 <- roc.curve(pos_v1, neg_v1, curve = T)
roc_v2 <- roc.curve(pos_v2, neg_v2, curve = T)
roc_v3 <- roc.curve(pos_v3, neg_v3, curve = T)
roc_v4 <- roc.curve(pos_v4, neg_v4, curve = T)
roc_v5 <- roc.curve(pos_v5, neg_v5, curve = T)
roc_v6 <- roc.curve(pos_v6, neg_v6, curve = T)

ggplot() + 
  geom_line(data=data.frame(roc_v1$curve), aes(x=X1,y=X2,color="black")) +
  geom_line(data=data.frame(roc_v2$curve), aes(x=X1,y=X2,color="grey50")) +
  geom_line(data=data.frame(roc_v3$curve), aes(x=X1,y=X2,color="forestgreen")) +
  geom_line(data=data.frame(roc_v4$curve), aes(x=X1,y=X2,color="orange")) +
  geom_line(data=data.frame(roc_v5$curve), aes(x=X1,y=X2,color="red")) +
  geom_line(data=data.frame(roc_v6$curve), aes(x=X1,y=X2,color="blue")) +
  labs(x = "1 - Specificity",y = "Sensitivity") +
  scale_colour_manual(name = "", values = c("black" = "black", "grey50" = "grey50", "forestgreen" = "forestgreen", "orange" =  "orange", "red" = "red", "blue" = "blue"),
                      breaks = c("black", "grey50", "forestgreen", "orange", "red", "blue"),
                      labels = c("black" = "xgb_v1", "grey50" = "xgb_v2", "forestgreen" = "xgb_v3", "orange" = "xgb_v4", "red" = "xgb_v5", "blue" = "xgb_v6"))

# ROC curves - AFD

pos_a1 <- p_xgb_a1$AFD[X_back_track_test_a$AFD == "AFD"]
neg_a1 <- p_xgb_a1$AFD[X_back_track_test_a$AFD == "not_AFD"]
pos_a2 <- p_xgb_a2$AFD[X_back_track_test_a$AFD == "AFD"]
neg_a2 <- p_xgb_a2$AFD[X_back_track_test_a$AFD == "not_AFD"]
pos_a3 <- p_xgb_a3$AFD[X_back_track_test_a$AFD == "AFD"]
neg_a3 <- p_xgb_a3$AFD[X_back_track_test_a$AFD == "not_AFD"]
pos_a4 <- p_xgb_a4$AFD[X_back_track_test_a$AFD == "AFD"]
neg_a4 <- p_xgb_a4$AFD[X_back_track_test_a$AFD == "not_AFD"]
pos_a5 <- p_xgb_a5$AFD[X_back_track_test_a$AFD == "AFD"]
neg_a5 <- p_xgb_a5$AFD[X_back_track_test_a$AFD == "not_AFD"]
pos_a6 <- p_xgb_a6$AFD[X_back_track_test_a$AFD == "AFD"]
neg_a6 <- p_xgb_a6$AFD[X_back_track_test_a$AFD == "not_AFD"]

roc_a1 <- roc.curve(pos_a1, neg_a1, curve = T)
roc_a2 <- roc.curve(pos_a2, neg_a2, curve = T)
roc_a3 <- roc.curve(pos_a3, neg_a3, curve = T)
roc_a4 <- roc.curve(pos_a4, neg_a4, curve = T)
roc_a5 <- roc.curve(pos_a5, neg_a5, curve = T)
roc_a6 <- roc.curve(pos_a6, neg_a6, curve = T)

ggplot() + 
  geom_line(data=data.frame(roc_a1$curve), aes(x=X1,y=X2,color="black")) +
  geom_line(data=data.frame(roc_a2$curve), aes(x=X1,y=X2,color="grey50")) +
  geom_line(data=data.frame(roc_a3$curve), aes(x=X1,y=X2,color="forestgreen")) +
  geom_line(data=data.frame(roc_a4$curve), aes(x=X1,y=X2,color="orange")) +
  geom_line(data=data.frame(roc_a5$curve), aes(x=X1,y=X2,color="red")) +
  geom_line(data=data.frame(roc_a6$curve), aes(x=X1,y=X2,color="blue")) +
  labs(x = "1 - Specificity",y = "Sensitivity") +
  scale_colour_manual(name = "", values = c("black" = "black", "grey50" = "grey50", "forestgreen" = "forestgreen", "orange" =  "orange", "red" = "red", "blue" = "blue"),
                      breaks = c("black", "grey50", "forestgreen", "orange", "red", "blue"),
                      labels = c("black" = "xgb_a1", "grey50" = "xgb_a2", "forestgreen" = "xgb_a3", "orange" = "xgb_a4", "red" = "xgb_a5", "blue" = "xgb_a6"))

# ROC curves - LEFT

pos_l1 <- p_xgb_l1$LEFT[X_back_track_test_l$LEFT == "LEFT"]
neg_l1 <- p_xgb_l1$LEFT[X_back_track_test_l$LEFT == "not_LEFT"]
pos_l2 <- p_xgb_l2$LEFT[X_back_track_test_l$LEFT == "LEFT"]
neg_l2 <- p_xgb_l2$LEFT[X_back_track_test_l$LEFT == "not_LEFT"]
pos_l3 <- p_xgb_l3$LEFT[X_back_track_test_l$LEFT == "LEFT"]
neg_l3 <- p_xgb_l3$LEFT[X_back_track_test_l$LEFT == "not_LEFT"]
pos_l4 <- p_xgb_l4$LEFT[X_back_track_test_l$LEFT == "LEFT"]
neg_l4 <- p_xgb_l4$LEFT[X_back_track_test_l$LEFT == "not_LEFT"]
pos_l5 <- p_xgb_l5$LEFT[X_back_track_test_l$LEFT == "LEFT"]
neg_l5 <- p_xgb_l5$LEFT[X_back_track_test_l$LEFT == "not_LEFT"]
pos_l6 <- p_xgb_l6$LEFT[X_back_track_test_l$LEFT == "LEFT"]
neg_l6 <- p_xgb_l6$LEFT[X_back_track_test_l$LEFT == "not_LEFT"]

roc_l1 <- roc.curve(pos_l1, neg_l1, curve = T)
roc_l2 <- roc.curve(pos_l2, neg_l2, curve = T)
roc_l3 <- roc.curve(pos_l3, neg_l3, curve = T)
roc_l4 <- roc.curve(pos_l4, neg_l4, curve = T)
roc_l5 <- roc.curve(pos_l5, neg_l5, curve = T)
roc_l6 <- roc.curve(pos_l6, neg_l6, curve = T)

ggplot() + 
  geom_line(data=data.frame(roc_l1$curve), aes(x=X1,y=X2,color="black")) +
  geom_line(data=data.frame(roc_l2$curve), aes(x=X1,y=X2,color="grey50")) +
  geom_line(data=data.frame(roc_l3$curve), aes(x=X1,y=X2,color="forestgreen")) +
  geom_line(data=data.frame(roc_l4$curve), aes(x=X1,y=X2,color="orange")) +
  geom_line(data=data.frame(roc_l5$curve), aes(x=X1,y=X2,color="red")) +
  geom_line(data=data.frame(roc_l6$curve), aes(x=X1,y=X2,color="blue")) +
  labs(x = "1 - Specificity",y = "Sensitivity") +
  scale_colour_manual(name = "", values = c("black" = "black", "grey50" = "grey50", "forestgreen" = "forestgreen", "orange" =  "orange", "red" = "red", "blue" = "blue"),
                      breaks = c("black", "grey50", "forestgreen", "orange", "red", "blue"),
                      labels = c("black" = "xgb_l1", "grey50" = "xgb_l2", "forestgreen" = "xgb_l3", "orange" = "xgb_l4", "red" = "xgb_l5", "blue" = "xgb_l6"))
