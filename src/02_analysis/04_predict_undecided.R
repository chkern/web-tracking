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
setwd("Y:\\Respondi\\RESPONDI_w2\\")


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
survey_w2 <- read_dta(file = "./survey_daten/survey_data_w2.dta")
# load media data
news_media <- readRDS(file = "./data/news_media_final.RDS")
# load fake data
fake <- readRDS(file = "./data/fake_final.RDS")
# load fake data
oeffrecht <- readRDS(file = "./data/oeffrecht_final.rds")

# Select some more background information from survey data and merge to first background data
back2 <- select(survey_w2, panelist_id,
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
levels(back$industry) <- gsub("[^a-zA-Z0-9]", "", levels(back$industry))
levels(back$why_unemp) <- gsub("[^a-zA-Z0-9]", "", levels(back$why_unemp))
levels(back$child_in_hh) <- gsub("[^a-zA-Z0-9]", "", levels(back$child_in_hh))
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

# Blocks of features (careful with column numbers)
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
Y <- survey_w2 %>% 
  select(c(undecided, panelist_id)) %>% 
  rename(panelistid = panelist_id)

Y$undecided <- as.factor(Y$undecided)
levels(Y$undecided) <- c("decided", "undecided")

X_back_track <- merge(X_back_track, Y, by = "panelistid")
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

X_back_track_train_c <- X_back_track_train[!is.na(X_back_track_train$undecided),]

model_u1 <- paste("undecided ~",paste(track_news_media,collapse="+"))

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

# Voted - track_apps_domains

model_u2 <- paste("undecided ~",paste(track_apps_domains,collapse="+"))

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



##################################################################################
# Models - XGBoost
##################################################################################

# undecided

model_u1 <- paste("undecided ~", paste(survey_demo, collapse="+"))
model_u2 <- paste(model_u1, paste("+"), paste(track_general, collapse="+"))
model_u3 <- paste(model_u1, paste("+"), paste(track_news_media, collapse="+"))
model_u4 <- paste(model_u1, paste("+"), paste(track_apps_domains, collapse="+"))
model_u5 <- paste("undecided ~", paste(track_general, collapse="+"))
model_u5 <- paste(model_u5, paste("+"), paste(track_news_media, collapse="+"))
model_u5 <- paste(model_u5, paste("+"), paste(track_apps_domains, collapse="+"))
model_u6 <- paste(model_u5, paste("+"), paste(survey_demo, collapse="+"))

small <- ncol(model.matrix(eval(parse(text=model_u1)), X_back_track_train))
med <- ncol(model.matrix(eval(parse(text=model_u2)), X_back_track_train))
full <-  small + length(track_apps_domains)

xgb_grid <- expand.grid(max_depth = c(1, 2, 3, 5, 7, 9),
                        nrounds = c(500, 750, 1000, 1500, 2000),
                        eta = c(0.005, 0.01, 0.025),
                        min_child_weight = 5,
                        subsample = 0.7,
                        gamma = 0,
                        colsample_bytree = c(0.7, 1))

rf_grid <- expand.grid(mtry = c(round(sqrt(small)), round(log2(small)), round(sqrt(med)), round(log2(med)), round(sqrt(full)), round(log2(full))),
                       splitrule = c("gini", "extratrees"),
                       min.node.size = c(1, 5))

# undecided - survey_demo

set.seed(303493)
eval(parse(text=paste("xgb_u1 <- train(",model_u1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_u1
plot(xgb_u1)

set.seed(303493)
eval(parse(text=paste("rf_u1 <- train(",model_u1,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_u1
plot(rf_u1)

# undecided - survey_demo + track_general

set.seed(303493)
eval(parse(text=paste("xgb_u2 <- train(",model_u2,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_u2
plot(xgb_u2)

set.seed(303493)
eval(parse(text=paste("rf_u2 <- train(",model_u2,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_u2
plot(rf_u2)

# undecided - survey_demo + track_news_media

set.seed(303493)
eval(parse(text=paste("xgb_u3 <- train(",model_u3,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_u3
plot(xgb_u3)

set.seed(303493)
eval(parse(text=paste("rf_u3 <- train(",model_u3,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_u3
plot(rf_u3)

# undecided - survey_demo + track_apps_domains

set.seed(303493)
eval(parse(text=paste("xgb_u4 <- train(",model_u4,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_u4
plot(xgb_u4)

set.seed(303493)
eval(parse(text=paste("rf_u4 <- train(",model_u4,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_u4
plot(rf_u4)

# undecided - only tracking data

set.seed(303493)
eval(parse(text=paste("xgb_u5 <- train(",model_u5,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_u5
plot(xgb_u5)

set.seed(303493)
eval(parse(text=paste("rf_u5 <- train(",model_u5,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_u5
plot(rf_u5)

# undecided - tracking data + survey_demo

set.seed(303493)
eval(parse(text=paste("xgb_u6 <- train(",model_u6,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl1,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_u6
plot(xgb_u6)

set.seed(303493)
eval(parse(text=paste("rf_u6 <- train(",model_u6,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl1,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_u6
plot(rf_u6)

save.image(".\\undecided_dec.RData")

##################################################################################
# Variable Importance
##################################################################################

plot(varImp(xgb_u7), top = 10)

imp_xgb_u7 <- varImp(xgb_u7)$importance
imp_xgb_u7 <- rownames_to_column(imp_xgb_u7, "varname")

imp_xgb_u7 <-
  imp_xgb_u7 %>%
  top_n(10, Overall) %>%
  mutate(order = 11 - row_number())

match(imp_xgb_u7$varname, names(X_back_track_train))
imp_xgb_u7$varname <- c("Demo: Gender", "Tracking domain", "Demo: Single, living w. partner", "Tracking domain", "Demo: Age", "Tracking apps", "Tracking domain", "Demo: HH Inc no answer", "Tracking apps", "Tracking domain")

ggplot(imp_xgb_u7) +
  geom_point(aes(x = Overall, y = order)) + 
  geom_segment(aes(y = order, yend = order, x = 1, xend = Overall)) +
  labs(x = "Importance", y = "") +
  xlim(0, 100) +
  scale_y_continuous(
    breaks = imp_xgb_u7$order,
    labels = imp_xgb_u7$varname)

ggsave("p_imp_u1.png", width = 6, height = 6)

##################################################################################
# Compare CV performance
##################################################################################

resamps1 <- resamples(list(xgb_u1, xgb_u2, xgb_u3, xgb_u4, xgb_u5, xgb_u6, xgb_u7))
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

p_resamp_u <- resamp1 %>%
  ggplot() +
  geom_boxplot(aes(y = ROC, x = fct_rev(model), fill = model)) +
  ylim(0, 1) +
  labs(x = "") +
  labs(y = "ROC-AUC") +
  coord_flip() + 
  scale_fill_manual(values = c("#F8766D", "#619CFF", "#00BA38", "#00BA38", "#00BA38", "#00BA38", "#00BA38")) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))

ggsave("p_resamp_u.png", p_resamp_u, width = 7.5, height = 7)

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

# ROC curves - Undecided

roc_xgb_u1 <- roc(response = X_back_track_test_u$undecided, predictor = p_xgb_u1$undecided)
roc_xgb_u2 <- roc(response = X_back_track_test_u$undecided, predictor = p_xgb_u2$undecided)
roc_xgb_u3 <- roc(response = X_back_track_test_u$undecided, predictor = p_xgb_u3$undecided)
roc_xgb_u4 <- roc(response = X_back_track_test_u$undecided, predictor = p_xgb_u4$undecided)
roc_xgb_u5 <- roc(response = X_back_track_test_u$undecided, predictor = p_xgb_u5$undecided)
roc_xgb_u6 <- roc(response = X_back_track_test_u$undecided, predictor = p_xgb_u6$undecided)
roc_xgb_u7 <- roc(response = X_back_track_test_u$undecided, predictor = p_xgb_u7$undecided)

ggroc(list("Demo" = roc_xgb_u1, "Tracking" = roc_xgb_u6, "Demo+Tracking" = roc_xgb_u7)) +
  geom_abline(aes(intercept = 1, slope = 1)) +
  scale_colour_manual(name = "", values = c("#F8766D", "#00BA38", "#619CFF"),
                      breaks = c("Demo", "Tracking", "Demo+Tracking"))

ggsave("p_roc_u.png", width = 7.5, height = 6)

# Performance at "optimal" threshold

prop.table(table(X_back_track_train$undecided))
roc_xgb_u1_t <- coords(roc_xgb_u1, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.2))
roc_xgb_u2_t <- coords(roc_xgb_u2, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.2))
roc_xgb_u3_t <- coords(roc_xgb_u3, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.2))
roc_xgb_u4_t <- coords(roc_xgb_u4, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.2))
roc_xgb_u5_t <- coords(roc_xgb_u5, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.2))
roc_xgb_u6_t <- coords(roc_xgb_u6, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.2))
roc_xgb_u7_t <- coords(roc_xgb_u7, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.2))

c_xgb_u1 <- as.factor(ifelse(p_xgb_u1$undecided > roc_xgb_u1_t[1], "undecided", "decided"))
c_xgb_u2 <- as.factor(ifelse(p_xgb_u2$undecided > roc_xgb_u2_t[1], "undecided", "decided"))
c_xgb_u3 <- as.factor(ifelse(p_xgb_u3$undecided > roc_xgb_u3_t[1], "undecided", "decided"))
c_xgb_u4 <- as.factor(ifelse(p_xgb_u4$undecided > roc_xgb_u4_t[1], "undecided", "decided"))
c_xgb_u5 <- as.factor(ifelse(p_xgb_u5$undecided > roc_xgb_u5_t[1], "undecided", "decided"))
c_xgb_u6 <- as.factor(ifelse(p_xgb_u6$undecided > roc_xgb_u6_t[1], "undecided", "decided"))
c_xgb_u7 <- as.factor(ifelse(p_xgb_u7$undecided > roc_xgb_u7_t[1], "undecided", "decided"))

cm1 <- confusionMatrix(c_xgb_u1, X_back_track_test_u$undecided, positive = "undecided", mode = "everything")
cm2 <- confusionMatrix(c_xgb_u2, X_back_track_test_u$undecided, positive = "undecided", mode = "everything")
cm3 <- confusionMatrix(c_xgb_u3, X_back_track_test_u$undecided, positive = "undecided", mode = "everything")
cm4 <- confusionMatrix(c_xgb_u4, X_back_track_test_u$undecided, positive = "undecided", mode = "everything")
cm5 <- confusionMatrix(c_xgb_u5, X_back_track_test_u$undecided, positive = "undecided", mode = "everything")
cm6 <- confusionMatrix(c_xgb_u6, X_back_track_test_u$undecided, positive = "undecided", mode = "everything")
cm7 <- confusionMatrix(c_xgb_u7, X_back_track_test_u$undecided, positive = "undecided", mode = "everything")

Demo <- c(cm1$overall[1], cm1$byClass[c(1:2,5,7)], cm1$overall[2])
Demo_Tracking_general <- c(cm2$overall[1], cm2$byClass[c(1:2,5,7)], cm2$overall[2])
Demo_Tracking_news <- c(cm3$overall[1], cm3$byClass[c(1:2,5,7)], cm3$overall[2])
Demo_Tracking_apps <- c(cm4$overall[1], cm4$byClass[c(1:2,5,7)], cm4$overall[2])
Demo_Tracking_fake <- c(cm5$overall[1], cm5$byClass[c(1:2,5,7)], cm5$overall[2])
Tracking <- c(cm6$overall[1], cm6$byClass[c(1:2,5,7)], cm6$overall[2])
Demo_Tracking <- c(cm7$overall[1], cm7$byClass[c(1:2,5,7)], cm7$overall[2])

tab <- rbind(Demo, Tracking, Demo_Tracking_general, Demo_Tracking_news, Demo_Tracking_apps, Demo_Tracking_fake, Demo_Tracking)
tab

rtffile <- RTF("t_perf_u.doc")
addTable(rtffile, cbind(rownames(tab),round(tab, digits = 3)))
done(rtffile)

