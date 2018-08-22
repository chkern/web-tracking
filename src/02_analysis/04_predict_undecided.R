##################################################################################

library(plyr)
library(tidyverse)
library(foreign)
library(haven)
library(data.table)
library(dummies)
library(car)
library(caret)
library(rpart)
library(MLmetrics)
library(e1071)
library(partykit)
library(xgboost)
library(ranger)
library(gridExtra)
library(pROC)

# Set path
setwd("~/Respondi/RESPONDI_w2")

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
tracking <- tracking[,c(1,186:ncol(tracking))]

# load the wave 3 survey data
survey_w3 <- read_dta(file = "./survey_daten/survey_data_w2.dta")

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
background  <- names(X_back_track[407:430])
track_general <- names(X_back_track[2:162])
track_fb_news <- names(X_back_track[c(163:174,341:374)])
track_apps <- names(X_back_track[179:338])
track_oeff_fake <- names(X_back_track[375:405])

names(X_back_track)[431:ncol(X_back_track)] <- gsub("[^a-zA-Z0-9]", "", names(X_back_track)[431:ncol(X_back_track)])
X_back_track <- X_back_track[, !duplicated(colnames(X_back_track))]
track_domains <- names(X_back_track[431:ncol(X_back_track)])

# Attach Ys to X variables
Y <- survey_w3[,c(1,ncol(survey_w3))]
Y$undecided <- as.factor(Y$undecided)
levels(Y$undecided) <- c("decided", "undecided")

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

ggplot(X_back_track_e, aes(E_mails)) + geom_bar(aes(fill=undecided))
ggplot(X_back_track_e, aes(Facebook)) + geom_bar(aes(fill=undecided))
ggplot(X_back_track_e, aes(Games)) + geom_bar(aes(fill=undecided))
ggplot(X_back_track_e, aes(News)) + geom_bar(aes(fill=undecided))
ggplot(X_back_track_e, aes(Shopping)) + geom_bar(aes(fill=undecided))
ggplot(X_back_track_e, aes(Social_Media)) + geom_bar(aes(fill=undecided))
ggplot(X_back_track_e, aes(WhatsApp)) + geom_bar(aes(fill=undecided))
ggplot(X_back_track_e, aes(YouTube)) + geom_bar(aes(fill=undecided))

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

# Undecided - track_fb_news

X_back_track_train_c <- X_back_track_train[!is.na(X_back_track_train$undecided),]

model_u1 <- paste("undecided ~",paste(track_fb_news,collapse="+"))

set.seed(543856)
eval(parse(text=paste("tree_u1 <- rpart(",model_u1,",
                      data = X_back_track_train_c,
                      control = rpart.control(minsplit = 10,
                      minbucket = 3,
                      cp = 0.001,
                      maxdepth = 4))")))

printcp(tree_u1)
party_tree_u1 <- as.party(tree_u1)
plot(party_tree_u1, gp = gpar(fontsize = 8.5))

# Undecided - track_apps

model_u2 <- paste("undecided ~",paste(track_apps,collapse="+"))

set.seed(543856)
eval(parse(text=paste("tree_u2 <- rpart(",model_u2,",
                      data = X_back_track_train_c,
                      control = rpart.control(minsplit = 10,
                      minbucket = 3,
                      cp = 0.001,
                      maxdepth = 4))")))

printcp(tree_u2)
party_tree_u2 <- as.party(tree_u2)
plot(party_tree_u2, gp = gpar(fontsize = 8.5))

# Undecided - track_oeff_fake

model_u3 <- paste("undecided ~",paste(track_oeff_fake,collapse="+"))

set.seed(543856)
eval(parse(text=paste("tree_u3 <- rpart(",model_u3,",
                      data = X_back_track_train_c,
                      control = rpart.control(minsplit = 10,
                      minbucket = 3,
                      cp = 0.001,
                      maxdepth = 4))")))

printcp(tree_u3)
party_tree_u3 <- as.party(tree_u3)
plot(party_tree_u3, gp = gpar(fontsize = 8.5))

# Undecided - track_domains

model_u4 <- paste("undecided ~",paste(track_domains,collapse="+"))

set.seed(543856)
eval(parse(text=paste("tree_u4 <- rpart(",model_u4,",
                      data = X_back_track_train_c,
                      control = rpart.control(minsplit = 10,
                      minbucket = 3,
                      cp = 0.001,
                      maxdepth = 4))")))

printcp(tree_u4)
party_tree_u4 <- as.party(tree_u4)
plot(party_tree_u4, gp = gpar(fontsize = 8.5))


##################################################################################
# Models - XGBoost
##################################################################################

# Undecided

model_u1 <- paste("undecided ~",paste(background,collapse="+"))
model_u2 <- paste(model_u1,paste("+"),paste(track_general,collapse="+"))
model_u3 <- paste(model_u1,paste("+"),paste(track_fb_news,collapse="+"))
model_u4 <- paste(model_u1,paste("+"),paste(track_apps,collapse="+"))
model_u5 <- paste(model_u1,paste("+"),paste(track_oeff_fake,collapse="+"))
model_u6 <- paste("undecided ~",paste(track_general,collapse="+"))
model_u6 <- paste(model_u6,paste("+"),paste(track_fb_news,collapse="+"))
model_u6 <- paste(model_u6,paste("+"),paste(track_apps,collapse="+"))
model_u6 <- paste(model_u6,paste("+"),paste(track_oeff_fake,collapse="+"))
model_u6 <- paste(model_u6,paste("+"),paste(track_domains,collapse="+"))
model_u7 <- paste(model_u6,paste("+"),paste(background,collapse="+"))

small <- ncol(model.matrix(eval(parse(text=model_u1)), X_back_track_train))
med <- ncol(model.matrix(eval(parse(text=model_u2)), X_back_track_train))
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

# Undecided - background

set.seed(303493)
eval(parse(text=paste("xgb_u1 <- train(",model_u1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_u1
plot(xgb_u1)

set.seed(303493)
eval(parse(text=paste("rf_u1 <- train(",model_u1,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'ROC',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_u1
plot(rf_u1)

# Undecided - background + track_general

set.seed(303493)
eval(parse(text=paste("xgb_u2 <- train(",model_u2,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_u2
plot(xgb_u2)

set.seed(303493)
eval(parse(text=paste("rf_u2 <- train(",model_u2,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'ROC',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_u2
plot(rf_u2)

# Undecided - background + track_fb_news

set.seed(303493)
eval(parse(text=paste("xgb_u3 <- train(",model_u3,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_u3
plot(xgb_u3)

set.seed(303493)
eval(parse(text=paste("rf_u3 <- train(",model_u3,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'ROC',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_u3
plot(rf_u3)

# Undecided - background + track_apps

set.seed(303493)
eval(parse(text=paste("xgb_u4 <- train(",model_u4,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_u4
plot(xgb_u4)

set.seed(303493)
eval(parse(text=paste("rf_u4 <- train(",model_u4,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'ROC',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_u4
plot(rf_u4)

# Undecided - background + track_oeff_fake

set.seed(303493)
eval(parse(text=paste("xgb_u5 <- train(",model_u5,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_u5
plot(xgb_u5)

set.seed(303493)
eval(parse(text=paste("rf_u5 <- train(",model_u5,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'ROC',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_u5
plot(rf_u5)

# Undecided - only tracking data

set.seed(303493)
eval(parse(text=paste("xgb_u6 <- train(",model_u6,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_u6
plot(xgb_u6)

set.seed(303493)
eval(parse(text=paste("rf_u6 <- train(",model_u6,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'ROC',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_u6
plot(rf_u6)

# Undecided - tracking full + background

set.seed(303493)
eval(parse(text=paste("xgb_u7 <- train(",model_u7,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'ROC',
                      na.action = na.omit)")))

xgb_u7
plot(xgb_u7)

set.seed(303493)
eval(parse(text=paste("rf_u7 <- train(",model_u7,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'ROC',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_u7
plot(rf_u7)

# Undecided - tracking full + background w. feature selection

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


##################################################################################
# Variable Importance
##################################################################################

plot(varImp(xgb_u1), top = 10)
plot(varImp(xgb_u2), top = 10)
plot(varImp(xgb_u3), top = 10)
plot(varImp(xgb_u4), top = 10)
plot(varImp(xgb_u5), top = 10)
plot(varImp(xgb_u6), top = 10)
plot(varImp(xgb_u7), top = 10)

plot(varImp(rf_u1), top = 10)
plot(varImp(rf_u2), top = 10)
plot(varImp(rf_u3), top = 10)
plot(varImp(rf_u4), top = 10)
plot(varImp(rf_u5), top = 10)
plot(varImp(rf_u6), top = 10)
plot(varImp(rf_u7), top = 10)

imp_xgb_u1 <- varImp(xgb_u1)$importance
imp_xgb_u1$type <- "XGBoost"
imp_xgb_u1 <- rownames_to_column(imp_xgb_u1, "varname")

imp_rf_u1 <- varImp(rf_u1)$importance
imp_rf_u1$type <- "Random Forest"
imp_rf_u1 <- rownames_to_column(imp_rf_u1, "varname")

imp_u1 <- rbind(imp_xgb_u1, imp_rf_u1)

imp_u1 <-
  imp_u1 %>%
  arrange(type, desc(Overall)) %>%
  group_by(type) %>%
  top_n(10, Overall) %>%
  ungroup() %>%
  mutate(order = row_number()) %>%
  mutate(rev_order = 21 - order)

p_imp_u1_1 <- 
  imp_u1 %>%
  filter(type == "XGBoost") %>%
  ggplot() +
  geom_point(aes(x = Overall, y = rev_order)) + 
  labs(x = "", y = "") +
  facet_grid(. ~ type) +
  xlim(0, 100) +
  scale_y_continuous(
    breaks = imp_u1$rev_order,
    labels = imp_u1$varname)

ggsave("p_imp_u1_1.png", width = 6, height = 6)

p_imp_u1_2 <- 
  imp_u1 %>%
  filter(type == "Random Forest") %>%
  ggplot() +
  geom_point(aes(x = Overall, y = rev_order)) + 
  labs(x = "", y = "") +
  facet_grid(. ~ type) +
  xlim(0, 100) +
  scale_y_continuous(
    breaks = imp_u1$rev_order,
    labels = imp_u1$varname)

ggsave("p_imp_u1_2.png", width = 6, height = 6)

##################################################################################
# Compare CV performance
##################################################################################

resamps1 <- resamples(list(xgb_u1, xgb_u2, xgb_u3, xgb_u4, xgb_u5, xgb_u6, xgb_u7,
                           rf_u1, rf_u2, rf_u3, rf_u4, rf_u5, rf_u6, rf_u7))

summary(resamps1)
bwplot(resamps1, metric = "ROC", xlim = c(0,1))

resamp1 <- 
  reshape(resamps1$values,
          direction = "long",
          varying = 2:ncol(resamps1$values),
          sep = "~",
          v.names = c("Accuracy","Kappa","ROC","Sens","Spec"),
          timevar = "model")

resamp1 <- 
  resamp1 %>%
  mutate(type = ifelse(model > 7, "Random Forest", "XGBoost")) %>%
  mutate(model = factor(model)) %>%
  mutate(model = fct_recode(model,
                            "Background" = "1",
                            "Back+General" = "2",
                            "Back+Fb_news" = "3",
                            "Back+Apps" = "4",
                            "Back+Fake" = "5",
                            "Track_only" = "6",
                            "Back+Track" = "7",
                            "Background" = "8",
                            "Back+General" = "9",
                            "Back+Fb_news" = "10",
                            "Back+Apps" = "11",
                            "Back+Fake" = "12",
                            "Track_only" = "13",
                            "Back+Track" = "14"))

p_resamp_u_1 <- resamp1 %>%
  filter(type == "XGBoost") %>%
  ggplot() +
  geom_boxplot(aes(y = ROC, x = as.factor(model))) +
  ylim(0, 1) +
  labs(x = "") +
  coord_flip() +
  facet_grid(. ~ type)

p_resamp_u_2 <- resamp1 %>%
  filter(type == "Random Forest") %>%
  ggplot() +
  geom_boxplot(aes(y = ROC, x = as.factor(model))) +
  ylim(0, 1) +
  labs(x = "") +
  coord_flip() +
  facet_grid(. ~ type)

p_resamp_u <- grid.arrange(p_resamp_u_1, p_resamp_u_2, ncol = 2)
ggsave("p_resamp_u.png", p_resamp_u, width = 9, height = 6)


##################################################################################
# Predict in test data
##################################################################################

X_back_track_test_u <- X_back_track_test[!is.na(X_back_track_test$undecided),]
p_xgb_u1 <- predict(xgb_u1, newdata = X_back_track_test_u, type = "prob")
p_xgb_u2 <- predict(xgb_u2, newdata = X_back_track_test_u, type = "prob")
p_xgb_u3 <- predict(xgb_u3, newdata = X_back_track_test_u, type = "prob")
p_xgb_u4 <- predict(xgb_u4, newdata = X_back_track_test_u, type = "prob")
p_xgb_u5 <- predict(xgb_u5, newdata = X_back_track_test_u, type = "prob")
p_xgb_u6 <- predict(xgb_u6, newdata = X_back_track_test_u, type = "prob")
p_xgb_u7 <- predict(xgb_u7, newdata = X_back_track_test_u, type = "prob")

p_rf_u1 <- predict(rf_u1, newdata = X_back_track_test_u, type = "prob")
p_rf_u2 <- predict(rf_u2, newdata = X_back_track_test_u, type = "prob")
p_rf_u3 <- predict(rf_u3, newdata = X_back_track_test_u, type = "prob")
p_rf_u4 <- predict(rf_u4, newdata = X_back_track_test_u, type = "prob")
p_rf_u5 <- predict(rf_u5, newdata = X_back_track_test_u, type = "prob")
p_rf_u6 <- predict(rf_u6, newdata = X_back_track_test_u, type = "prob")
p_rf_u7 <- predict(rf_u7, newdata = X_back_track_test_u, type = "prob")

# ROC curves - Undecided

roc_xgb_u1 <- roc(response = X_back_track_test_u$undecided, predictor = p_xgb_u1$undecided, smooth = T)
roc_xgb_u2 <- roc(response = X_back_track_test_u$undecided, predictor = p_xgb_u2$undecided, smooth = T)
roc_xgb_u3 <- roc(response = X_back_track_test_u$undecided, predictor = p_xgb_u3$undecided, smooth = T)
roc_xgb_u4 <- roc(response = X_back_track_test_u$undecided, predictor = p_xgb_u4$undecided, smooth = T)
roc_xgb_u5 <- roc(response = X_back_track_test_u$undecided, predictor = p_xgb_u5$undecided, smooth = T)
roc_xgb_u6 <- roc(response = X_back_track_test_u$undecided, predictor = p_xgb_u6$undecided, smooth = T)
roc_xgb_u7 <- roc(response = X_back_track_test_u$undecided, predictor = p_xgb_u7$undecided, smooth = T)

ggplot() + 
  geom_line(aes(x = 1 - roc_xgb_u1$specificities, y = roc_xgb_u1$sensitivities, color = "black")) +
  geom_line(aes(x = 1 - roc_xgb_u2$specificities, y = roc_xgb_u2$sensitivities, color = "grey50")) +
  geom_line(aes(x = 1 - roc_xgb_u3$specificities, y = roc_xgb_u3$sensitivities, color = "forestgreen")) +
  geom_line(aes(x = 1 - roc_xgb_u4$specificities, y = roc_xgb_u4$sensitivities, color = "orange")) +
  geom_line(aes(x = 1 - roc_xgb_u5$specificities, y = roc_xgb_u5$sensitivities, color = "red")) +
  geom_line(aes(x = 1 - roc_xgb_u6$specificities, y = roc_xgb_u6$sensitivities, color = "blue")) +
  geom_line(aes(x = 1 - roc_xgb_u7$specificities, y = roc_xgb_u7$sensitivities, color = "darkviolet")) +
  labs(x = "1 - Specificity", y = "Sensitivity") +
  geom_abline(aes(intercept = 0, slope = 1)) +
  scale_colour_manual(name = "", values = c("black" = "black", "grey50" = "grey50", "forestgreen" = "forestgreen", "orange" =  "orange", "red" = "red", "blue" = "blue", "darkviolet" = "darkviolet"),
                      breaks = c("black", "grey50", "forestgreen", "orange", "red", "blue", "darkviolet"),
                      labels = c("black" = "Background", "grey50" = "Back+General", "forestgreen" = "Back+Fb_news", "orange" = "Back+Apps", "red" = "Back+Fake", "blue" = "Track_only", "darkviolet" = "Back+Track"))

ggsave("p_roc_u1.png", width = 7.5, height = 6)

roc_rf_u1 <- roc(response = X_back_track_test_u$undecided, predictor = p_rf_u1$undecided, smooth = T)
roc_rf_u2 <- roc(response = X_back_track_test_u$undecided, predictor = p_rf_u2$undecided, smooth = T)
roc_rf_u3 <- roc(response = X_back_track_test_u$undecided, predictor = p_rf_u3$undecided, smooth = T)
roc_rf_u4 <- roc(response = X_back_track_test_u$undecided, predictor = p_rf_u4$undecided, smooth = T)
roc_rf_u5 <- roc(response = X_back_track_test_u$undecided, predictor = p_rf_u5$undecided, smooth = T)
roc_rf_u6 <- roc(response = X_back_track_test_u$undecided, predictor = p_rf_u6$undecided, smooth = T)
roc_rf_u7 <- roc(response = X_back_track_test_u$undecided, predictor = p_rf_u7$undecided, smooth = T)

ggplot() + 
  geom_line(aes(x = 1 - roc_rf_u1$specificities, y = roc_rf_u1$sensitivities, color = "black")) +
  geom_line(aes(x = 1 - roc_rf_u2$specificities, y = roc_rf_u2$sensitivities, color = "grey50")) +
  geom_line(aes(x = 1 - roc_rf_u3$specificities, y = roc_rf_u3$sensitivities, color = "forestgreen")) +
  geom_line(aes(x = 1 - roc_rf_u4$specificities, y = roc_rf_u4$sensitivities, color = "orange")) +
  geom_line(aes(x = 1 - roc_rf_u5$specificities, y = roc_rf_u5$sensitivities, color = "red")) +
  geom_line(aes(x = 1 - roc_rf_u6$specificities, y = roc_rf_u6$sensitivities, color = "blue")) +
  geom_line(aes(x = 1 - roc_rf_u7$specificities, y = roc_rf_u7$sensitivities, color = "darkviolet")) +
  labs(x = "1 - Specificity", y = "Sensitivity") +
  geom_abline(aes(intercept = 0, slope = 1)) +
  scale_colour_manual(name = "", values = c("black" = "black", "grey50" = "grey50", "forestgreen" = "forestgreen", "orange" =  "orange", "red" = "red", "blue" = "blue", "darkviolet" = "darkviolet"),
                      breaks = c("black", "grey50", "forestgreen", "orange", "red", "blue", "darkviolet"),
                      labels = c("black" = "Background", "grey50" = "Back+General", "forestgreen" = "Back+Fb_news", "orange" = "Back+Apps", "red" = "Back+Fake", "blue" = "Track_only", "darkviolet" = "Back+Track"))

ggsave("p_roc_u2.png", width = 7.5, height = 6)

