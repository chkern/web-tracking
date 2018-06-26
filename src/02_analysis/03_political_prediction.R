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
tracking <- readRDS(file = ".\\data\\data_prep_final_small.rds")

# load the wave 3 survey data
survey_w3 <- read_dta(file = ".\\survey_daten\\survey_data_w3.dta")
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

#zero_var_track <- nearZeroVar(X_back_track, freqCut = 99.9/0.1, names = TRUE, allowParallel = T)
#X_back_track <- X_back_track[ , -which(names(X_back_track) %in% zero_var_track)]
#rm(zero_var_track)

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

# put datasets together

X_back_track <- merge(tracking, back, by="panelist_id")

summary(is.na(X_back_track))
X_back_track[is.na(X_back_track)] <- 0

# Blocks of features (careful with column numbers)

background  <- names(X_back_track[409:438])
track_general <- names(X_back_track[2:164])
track_fb_news <- names(X_back_track[c(165:176,341:376)])
track_apps <- names(X_back_track[181:340])
track_oeff_fake <- names(X_back_track[377:407])

# Attach Ys to X variables
Y <- survey_w3[,c(1,365:368)]

Y$voted <- as.factor(Y$voted)
levels(Y$voted) <- c("not_voted", "voted")
Y$party_affiliation <- as.factor(Y$party_affiliation)
levels(Y$party_affiliation) <- c("CDU","SPD","GREEN","FDP","LEFT","AFD","Other")
Y$AFD <- as.factor(Y$AFD)
levels(Y$AFD) <- c("not_AFD", "AFD")
Y$left_socdem <- as.factor(Y$left_socdem)
levels(Y$left_socdem) <- c("not_left", "left")
  
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

model_a1 <- paste("voted ~",paste(track_fb_news,collapse="+"))

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

# Voted - track_apps

model_a2 <- paste("voted ~",paste(track_apps,collapse="+"))

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

# Voted - track_oeff_fake

model_a3 <- paste("voted ~",paste(track_oeff_fake,collapse="+"))

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

# AFD - track_oeff_fake

X_back_track_train_c <- X_back_track_train[!is.na(X_back_track_train$AFD),]

model_b3 <- paste("AFD ~",paste(track_oeff_fake,collapse="+"))

set.seed(543856)
eval(parse(text=paste("tree_b3 <- rpart(",model_b3,",
                data = X_back_track_train_c,
                control = rpart.control(minsplit = 10,
                                       minbucket = 3,
                                       cp = 0.001,
                                       maxdepth = 4))")))

printcp(tree_b3)
party_tree_b3 <- as.party(tree_b3)
plot(party_tree_b3, gp = gpar(fontsize = 8.5))

# left_socdem - track_oeff_fake

model_c3 <- paste("left_socdem ~",paste(track_oeff_fake,collapse="+"))

set.seed(543856)
eval(parse(text=paste("tree_c3 <- rpart(",model_c3,",
                data = X_back_track_train_c,
                control = rpart.control(minsplit = 10,
                                       minbucket = 3,
                                       cp = 0.001,
                                       maxdepth = 4))")))

printcp(tree_c3)
party_tree_c3 <- as.party(tree_c3)
plot(party_tree_c3, gp = gpar(fontsize = 8.5))

##################################################################################
# Models - XGBoost
##################################################################################

grid <- expand.grid(max_depth = c(3, 5, 7),
                    nrounds = c(1000, 1500, 2000),
                    eta = c(0.01, 0.05),
                    min_child_weight = 5,
                    subsample = 0.7,
                    gamma = 0,
                    colsample_bytree = c(0.7,1))

# Voted - background

model_v1 <- paste("voted ~",paste(background,collapse="+"))

set.seed(300193)
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

set.seed(300193)
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

set.seed(300193)
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

set.seed(300193)
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

set.seed(300193)
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

set.seed(300193)
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

set.seed(300193)
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

# AFD - background + track_oeff_fake

model_a5 <- paste(model_a1,paste("+"),paste(track_oeff_fake,collapse="+"))

set.seed(300193)
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

set.seed(300193)
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

# left_socdem - background

model_l1 <- paste("left_socdem ~",paste(background,collapse="+"))

set.seed(300193)
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

# left_socdem - background + track_oeff_fake

model_l5 <- paste(model_l1,paste("+"),paste(track_oeff_fake,collapse="+"))

set.seed(300193)
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

# left_socdem - only tracking data

model_l6 <- paste("left_socdem ~",paste(track_general,collapse="+"))
model_l6 <- paste(model_l6,paste("+"),paste(track_fb_news,collapse="+"))
model_l6 <- paste(model_l6,paste("+"),paste(track_apps,collapse="+"))
model_l6 <- paste(model_l6,paste("+"),paste(track_oeff_fake,collapse="+"))

set.seed(300193)
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

set.seed(300193)
eval(parse(text=paste("xgb_p1 <- train(",model_p1,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl2,
                tuneGrid = grid,
                metric = 'Kappa',
                na.action = na.omit)")))

xgb_p1
plot(xgb_p1)
confusionMatrix(xgb_p1)

# Party affiliation - background + track_oeff_fake

model_p5 <- paste(model_p1,paste("+"),paste(track_oeff_fake,collapse="+"))

set.seed(300193)
eval(parse(text=paste("xgb_p5 <- train(",model_p5,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl2,
                tuneGrid = grid,
                metric = 'Kappa',
                na.action = na.omit)")))

xgb_p5
plot(xgb_p5)
confusionMatrix(xgb_p5)

# Party affiliation - only tracking data

model_p6 <- paste("party_affiliation ~",paste(track_general,collapse="+"))
model_p6 <- paste(model_p6,paste("+"),paste(track_fb_news,collapse="+"))
model_p6 <- paste(model_p6,paste("+"),paste(track_apps,collapse="+"))
model_p6 <- paste(model_p6,paste("+"),paste(track_oeff_fake,collapse="+"))

set.seed(300193)
eval(parse(text=paste("xgb_p6 <- train(",model_p6,",
                data = X_back_track_train,
                method = 'xgbTree',
                trControl = ctrl2,
                tuneGrid = grid,
                metric = 'Kappa',
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
bwplot(resamps1, metric = c("Accuracy", "Kappa"))

resamps2 <- resamples(list(XGBoost1 = xgb_a1,
                           XGBoost5 = xgb_a5,
                           XGBoost6 = xgb_a6))

summary(resamps2)
bwplot(resamps2, metric = c("Accuracy", "Kappa"))

resamps3 <- resamples(list(XGBoost1 = xgb_l1,
                           XGBoost5 = xgb_l5,
                           XGBoost6 = xgb_l6))

summary(resamps3)
bwplot(resamps3, metric = c("Accuracy", "Kappa"))

resamps4 <- resamples(list(XGBoost1 = xgb_p1,
                           XGBoost5 = xgb_p5,
                           XGBoost6 = xgb_p6))

summary(resamps4)
bwplot(resamps4, metric = c("Accuracy", "Kappa"))

##################################################################################
# Predict in test data
##################################################################################

c_xgb_v1 <- predict(xgb_v1, newdata = X_back_track_test)
c_xgb_v2 <- predict(xgb_v2, newdata = X_back_track_test)
c_xgb_v3 <- predict(xgb_v3, newdata = X_back_track_test)
c_xgb_v4 <- predict(xgb_v4, newdata = X_back_track_test)
c_xgb_v5 <- predict(xgb_v5, newdata = X_back_track_test)
c_xgb_v6 <- predict(xgb_v6, newdata = X_back_track_test)

c_xgb_a1 <- predict(xgb_a1, newdata = X_back_track_test)
c_xgb_a5 <- predict(xgb_a5, newdata = X_back_track_test)
c_xgb_a6 <- predict(xgb_a6, newdata = X_back_track_test)

c_xgb_l1 <- predict(xgb_l1, newdata = X_back_track_test)
c_xgb_l5 <- predict(xgb_l5, newdata = X_back_track_test)
c_xgb_l6 <- predict(xgb_l6, newdata = X_back_track_test)

c_xgb_p1 <- predict(xgb_p1, newdata = X_back_track_test)
c_xgb_p5 <- predict(xgb_p5, newdata = X_back_track_test)
c_xgb_p6 <- predict(xgb_p6, newdata = X_back_track_test)

confusionMatrix(c_xgb_v1, X_back_track_test$voted)
confusionMatrix(c_xgb_v2, X_back_track_test$voted)
confusionMatrix(c_xgb_v3, X_back_track_test$voted)
confusionMatrix(c_xgb_v4, X_back_track_test$voted)
confusionMatrix(c_xgb_v5, X_back_track_test$voted)
confusionMatrix(c_xgb_v6, X_back_track_test$voted)

confusionMatrix(c_xgb_a1, X_back_track_test$party_affiliation)
confusionMatrix(c_xgb_a5, X_back_track_test$party_affiliation)
confusionMatrix(c_xgb_a6, X_back_track_test$party_affiliation)

confusionMatrix(c_xgb_l1, X_back_track_test$party_affiliation)
confusionMatrix(c_xgb_l5, X_back_track_test$party_affiliation)
confusionMatrix(c_xgb_l6, X_back_track_test$party_affiliation)

confusionMatrix(c_xgb_p1, X_back_track_test$party_affiliation)
confusionMatrix(c_xgb_p5, X_back_track_test$party_affiliation)
confusionMatrix(c_xgb_p6, X_back_track_test$party_affiliation)
