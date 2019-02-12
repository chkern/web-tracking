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

# put datasets together

X_back_track <- merge(fake, news_media, by = "panelist_id")
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
Y <- back %>% 
  select(panelistid, netinc, age, gender, state, legalstatus, numchildren, emptype)

Y$D_lowinc <- as.factor(ifelse(Y$netinc %in% c("[1] under 500 EUR", "[2] 500 to 1.000 EUR"), "low", "not_low"))
Y$D_lowinc[Y$netinc %in% c("[0] - please select -", "[98] No own income", "[99] No comment / answer")] <- NA
table(Y$netinc, Y$D_lowinc)

Y$D_highinc <- as.factor(ifelse(Y$netinc %in% c("[11] over 5.000 EUR", "[10] 4.500 to 5.000 EUR", "[9] 4.000 to 4.500 EUR", "[8] 3.500 to 4.000 EUR", "[7] 3.000 to 3.500 EUR"), "high", "not_high"))
Y$D_highinc[Y$netinc %in% c("[0] - please select -", "[98] No own income", "[99] No comment / answer")] <- NA
table(Y$netinc, Y$D_highinc)

Y$D_u25 <- as.factor(ifelse(Y$age <= 25, "u25", "o25"))
summary(Y$D_u25)

Y$D_o60 <- as.factor(ifelse(Y$age >= 60, "o60", "u60"))
summary(Y$D_o60)

Y$D_male <- as.factor(ifelse(Y$gender == "[1] male", "male", "female"))
summary(Y$D_male)

Y$D_east <- as.factor(ifelse(Y$state %in% c("[4] Brandenburg", "[8] Mecklenburg-Vorpommern", "[13] Sachsen", "[14] Sachsen-Anhalt", "[16] Thueringen"), "east", "west"))
Y$D_east[Y$state == "[0] - bitte auswählen -"] <- NA
table(Y$state, Y$D_east)

Y$D_married <- as.factor(ifelse(Y$legalstatus == "[1] Married", "married", "not_married"))
table(Y$D_married)
  
Y$D_nopartner <- as.factor(ifelse(Y$legalstatus %in% c("[4] Single, not living with partner", "[6] Divorced/widowed, not living with partner"), "no_partner", "partner"))
table(Y$D_nopartner)

Y$D_nochild <- as.factor(ifelse(Y$numchildren == "[98] No children", "no_children", "children"))
table(Y$D_nochild)

Y$D_unemp <- as.factor(ifelse(Y$emptype == "[7] Currently unemployed", "unemployed", "not_unemployed"))
Y$D_unemp[Y$emptype == "[0] - please select -"] <- NA
table(Y$emptype, Y$D_unemp)

Y$D_fulltime <- as.factor(ifelse(Y$emptype == "[1] Work full-time (30+ hours per week)", "fulltime", "not_fulltime"))
Y$D_fulltime[Y$emptype == "[0] - please select -"] <- NA
table(Y$emptype, Y$D_fulltime)

Y <- Y %>% 
  select(panelistid, D_lowinc, D_highinc, D_u25, D_o60, D_male, D_east, D_married, D_nopartner, D_nochild, D_unemp, D_fulltime)

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

ctrl  <- trainControl(method = "cv",
                      number = 10,
                      summaryFunction = evalStats,
                      classProbs = TRUE,
                      verboseIter = TRUE)

##################################################################################
# Models - XGBoost & RF
##################################################################################

xgb_grid <- expand.grid(max_depth = c(1, 2, 3, 5, 7, 9),
                        nrounds = c(500, 750, 1000, 1500, 2000),
                        eta = c(0.005, 0.01, 0.025),
                        min_child_weight = 5,
                        subsample = 0.7,
                        gamma = 0,
                        colsample_bytree = c(0.7, 1))

cols <- length(tracking)

rf_grid <- expand.grid(mtry = c(round(sqrt(cols)), round(log2(cols))),
                       splitrule = c("gini", "extratrees"),
                       min.node.size = c(1, 5))

# D_lowinc

model_li <- paste("D_lowinc ~", paste(track_general, collapse="+"))
model_li <- paste(model_li, paste("+"), paste(track_news_media, collapse="+"))
model_li <- paste(model_li, paste("+"), paste(track_apps_domains, collapse="+"))

# D_lowinc - only tracking data

set.seed(97643)
eval(parse(text=paste("xgb_li <- train(",model_li,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_li
plot(xgb_li)

set.seed(97643)
eval(parse(text=paste("rf_li <- train(",model_li,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_li
plot(rf_li)

# D_highinc

model_hi <- paste("D_highinc ~", paste(track_general, collapse="+"))
model_hi <- paste(model_hi, paste("+"), paste(track_news_media, collapse="+"))
model_hi <- paste(model_hi, paste("+"), paste(track_apps_domains, collapse="+"))

# D_highinc - only tracking data

set.seed(97643)
eval(parse(text=paste("xgb_hi <- train(",model_hi,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_hi
plot(xgb_hi)

set.seed(97643)
eval(parse(text=paste("rf_hi <- train(",model_hi,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_hi
plot(rf_hi)

# D_u25

model_ua <- paste("D_u25 ~", paste(track_general, collapse="+"))
model_ua <- paste(model_ua, paste("+"), paste(track_news_media, collapse="+"))
model_ua <- paste(model_ua, paste("+"), paste(track_apps_domains, collapse="+"))

# D_u25 - only tracking data

set.seed(97643)
eval(parse(text=paste("xgb_ua <- train(",model_ua,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_ua
plot(xgb_ua)

set.seed(97643)
eval(parse(text=paste("rf_ua <- train(",model_ua,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_ua
plot(rf_ua)

# D_o60

model_oa <- paste("D_o60 ~", paste(track_general, collapse="+"))
model_oa <- paste(model_oa, paste("+"), paste(track_news_media, collapse="+"))
model_oa <- paste(model_oa, paste("+"), paste(track_apps_domains, collapse="+"))

# D_o60 - only tracking data

set.seed(97643)
eval(parse(text=paste("xgb_oa <- train(",model_oa,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_oa
plot(xgb_oa)

set.seed(97643)
eval(parse(text=paste("rf_oa <- train(",model_oa,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_oa
plot(rf_oa)

# D_male

model_ma <- paste("D_male ~", paste(track_general, collapse="+"))
model_ma <- paste(model_ma, paste("+"), paste(track_news_media, collapse="+"))
model_ma <- paste(model_ma, paste("+"), paste(track_apps_domains, collapse="+"))

# D_male - only tracking data

set.seed(97643)
eval(parse(text=paste("xgb_ma <- train(",model_ma,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_ma
plot(xgb_ma)

set.seed(97643)
eval(parse(text=paste("rf_ma <- train(",model_ma,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_ma
plot(rf_ma)

# D_east

model_ea <- paste("D_east ~", paste(track_general, collapse="+"))
model_ea <- paste(model_ea, paste("+"), paste(track_news_media, collapse="+"))
model_ea <- paste(model_ea, paste("+"), paste(track_apps_domains, collapse="+"))

# D_east - only tracking data

set.seed(97643)
eval(parse(text=paste("xgb_ea <- train(",model_ea,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_ea
plot(xgb_ea)

set.seed(97643)
eval(parse(text=paste("rf_ea <- train(",model_ea,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_ea
plot(rf_ea)

# D_married

model_mr <- paste("D_married ~", paste(track_general, collapse="+"))
model_mr <- paste(model_mr, paste("+"), paste(track_news_media, collapse="+"))
model_mr <- paste(model_mr, paste("+"), paste(track_apps_domains, collapse="+"))

# D_married - only tracking data

set.seed(97643)
eval(parse(text=paste("xgb_mr <- train(",model_mr,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_mr
plot(xgb_mr)

set.seed(97643)
eval(parse(text=paste("rf_mr <- train(",model_mr,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_mr
plot(rf_mr)

# D_nopartner

model_np <- paste("D_nopartner ~", paste(track_general, collapse="+"))
model_np <- paste(model_np, paste("+"), paste(track_news_media, collapse="+"))
model_np <- paste(model_np, paste("+"), paste(track_apps_domains, collapse="+"))

# D_nopartner - only tracking data

set.seed(97643)
eval(parse(text=paste("xgb_np <- train(",model_np,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_np
plot(xgb_np)

set.seed(97643)
eval(parse(text=paste("rf_np <- train(",model_np,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_np
plot(rf_np)

# D_nochild

model_ch <- paste("D_nochild ~", paste(track_general, collapse="+"))
model_ch <- paste(model_ch, paste("+"), paste(track_news_media, collapse="+"))
model_ch <- paste(model_ch, paste("+"), paste(track_apps_domains, collapse="+"))

# D_nochild - only tracking data

set.seed(97643)
eval(parse(text=paste("xgb_ch <- train(",model_ch,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_ch
plot(xgb_ch)

set.seed(97643)
eval(parse(text=paste("rf_ch <- train(",model_ch,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_ch
plot(rf_ch)

# D_unemp

model_ue <- paste("D_unemp ~", paste(track_general, collapse="+"))
model_ue <- paste(model_ue, paste("+"), paste(track_news_media, collapse="+"))
model_ue <- paste(model_ue, paste("+"), paste(track_apps_domains, collapse="+"))

# D_unemp - only tracking data

set.seed(97643)
eval(parse(text=paste("xgb_ue <- train(",model_ue,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_ue
plot(xgb_ue)

set.seed(97643)
eval(parse(text=paste("rf_ue <- train(",model_ue,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_ue
plot(rf_ue)

# D_fulltime

model_ft <- paste("D_fulltime ~", paste(track_general, collapse="+"))
model_ft <- paste(model_ft, paste("+"), paste(track_news_media, collapse="+"))
model_ft <- paste(model_ft, paste("+"), paste(track_apps_domains, collapse="+"))

# D_fulltime - only tracking data

set.seed(97643)
eval(parse(text=paste("xgb_ft <- train(",model_ft,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_ft
plot(xgb_ft)

set.seed(97643)
eval(parse(text=paste("rf_ft <- train(",model_ft,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_ft
plot(rf_ft)

##################################################################################
# Compare CV performance
##################################################################################

resamps <- resamples(list(xgb_li, xgb_hi, xgb_ua, xgb_oa, xgb_ma, xgb_ea, xgb_mr, xgb_np, xgb_ch, xgb_ue, xgb_ft))
summary(resamps)

resamp <- 
  reshape(resamps$values,
          direction = "long",
          varying = 2:ncol(resamps$values),
          sep = "~",
          v.names = c("Accuracy", "Kappa", "logLoss", "ROC", "Sens", "Spec"),
          timevar = "model")

resamp <- 
  resamp %>%
  mutate(model = factor(model)) %>%
  mutate(model = fct_recode(model,
                            "Low inc." = "1",
                            "High inc." = "2",
                            "Under 25" = "3",
                            "Over 60" = "4",
                            "Male" = "5",
                            "East" = "6",
                            "Married" = "7",
                            "No partner" = "8",
                            "No children" = "9",
                            "Unemployed" = "10",
                            "Full-time emp." = "11")) %>%
  mutate(model = fct_relevel(model, "Low inc.",
                             "High inc.",
                             "Under 25",
                             "Over 60",
                             "Male",
                             "East",
                             "Married",
                             "No partner",
                             "No children",
                             "Unemployed",
                             "Full-time emp."))

ggplot(resamp) +
  geom_boxplot(aes(y = ROC, x = fct_rev(model)), fill = "#619CFF") +
  ylim(0, 1) +
  labs(x = "") +
  labs(y = "ROC-AUC") +
  coord_flip() + 
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
ggsave("p_resamp_d1.png", width = 7.5, height = 7)

ggplot(resamp) +
  geom_boxplot(aes(y = logLoss, x = fct_rev(model)), fill = "#619CFF") +
  ylim(0, 1) +
  labs(x = "") +
  labs(y = "logLoss") +
  coord_flip() + 
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
ggsave("p_resamp_d2.png", width = 7.5, height = 7)

##################################################################################
# Predict in test data
##################################################################################

p_xgb_li <- predict(xgb_li, newdata = X_back_track_test, type = "prob")
p_xgb_hi <- predict(xgb_hi, newdata = X_back_track_test, type = "prob")
p_xgb_ua <- predict(xgb_ua, newdata = X_back_track_test, type = "prob")
p_xgb_oa <- predict(xgb_oa, newdata = X_back_track_test, type = "prob")
p_xgb_ma <- predict(xgb_ma, newdata = X_back_track_test, type = "prob")
p_xgb_ea <- predict(xgb_ea, newdata = X_back_track_test, type = "prob")
p_xgb_mr <- predict(xgb_mr, newdata = X_back_track_test, type = "prob")
p_xgb_np <- predict(xgb_np, newdata = X_back_track_test, type = "prob")
p_xgb_ch <- predict(xgb_ch, newdata = X_back_track_test, type = "prob")
p_xgb_ue <- predict(xgb_ue, newdata = X_back_track_test, type = "prob")
p_xgb_ft <- predict(xgb_ft, newdata = X_back_track_test, type = "prob")

# ROC and LogLoss

roc_xgb_li <- roc(response = X_back_track_test$D_lowinc, predictor = p_xgb_li$low)
roc_xgb_hi <- roc(response = X_back_track_test$D_highinc, predictor = p_xgb_hi$high)
roc_xgb_ua <- roc(response = X_back_track_test$D_u25, predictor = p_xgb_ua$u25)
roc_xgb_oa <- roc(response = X_back_track_test$D_o60, predictor = p_xgb_oa$o60)
roc_xgb_ma <- roc(response = X_back_track_test$D_male, predictor = p_xgb_ma$male)
roc_xgb_ea <- roc(response = X_back_track_test$D_east, predictor = p_xgb_ea$east)
roc_xgb_mr <- roc(response = X_back_track_test$D_married, predictor = p_xgb_mr$married)
roc_xgb_np <- roc(response = X_back_track_test$D_nopartner, predictor = p_xgb_np$no_partner)
roc_xgb_ch <- roc(response = X_back_track_test$D_nochild, predictor = p_xgb_ch$no_children)
roc_xgb_ue <- roc(response = X_back_track_test$D_unemp, predictor = p_xgb_ue$unemployed)
roc_xgb_ft <- roc(response = X_back_track_test$D_fulltime, predictor = p_xgb_ft$fulltime)

p_xgb_li$obs <- X_back_track_test$D_lowinc
p_xgb_li$pred <- predict(xgb_li, newdata = X_back_track_test)
p_xgb_hi$obs <- X_back_track_test$D_highinc
p_xgb_hi$pred <- predict(xgb_hi, newdata = X_back_track_test)
p_xgb_ua$obs <- X_back_track_test$D_u25
p_xgb_ua$pred <- predict(xgb_ua, newdata = X_back_track_test)
p_xgb_oa$obs <- X_back_track_test$D_o60
p_xgb_oa$pred <- predict(xgb_oa, newdata = X_back_track_test)
p_xgb_ma$obs <- X_back_track_test$D_male
p_xgb_ma$pred <- predict(xgb_ma, newdata = X_back_track_test)
p_xgb_ea$obs <- X_back_track_test$D_east
p_xgb_ea$pred <- predict(xgb_ea, newdata = X_back_track_test)
p_xgb_mr$obs <- X_back_track_test$D_married
p_xgb_mr$pred <- predict(xgb_mr, newdata = X_back_track_test)
p_xgb_np$obs <- X_back_track_test$D_nopartner
p_xgb_np$pred <- predict(xgb_np, newdata = X_back_track_test)
p_xgb_ch$obs <- X_back_track_test$D_nochild
p_xgb_ch$pred <- predict(xgb_ch, newdata = X_back_track_test)
p_xgb_ue$obs <- X_back_track_test$D_unemp
p_xgb_ue$pred <- predict(xgb_ue, newdata = X_back_track_test)
p_xgb_ft$obs <- X_back_track_test$D_fulltime
p_xgb_ft$pred <- predict(xgb_ft, newdata = X_back_track_test)

perf_li <- cbind(twoClassSummary(drop_na(p_xgb_li), lev = levels(p_xgb_li$obs))[[1]],
                 mnLogLoss(p_xgb_li, lev = levels(p_xgb_li$obs))[[1]])
perf_hi <- cbind(twoClassSummary(drop_na(p_xgb_hi), lev = levels(p_xgb_hi$obs))[[1]],
                 mnLogLoss(p_xgb_hi, lev = levels(p_xgb_hi$obs))[[1]])
perf_ua <- cbind(twoClassSummary(drop_na(p_xgb_ua), lev = levels(p_xgb_ua$obs))[[1]],
                 mnLogLoss(p_xgb_ua, lev = levels(p_xgb_ua$obs))[[1]])
perf_oa <- cbind(twoClassSummary(drop_na(p_xgb_oa), lev = levels(p_xgb_oa$obs))[[1]],
                 mnLogLoss(p_xgb_oa, lev = levels(p_xgb_oa$obs))[[1]])
perf_ma <- cbind(twoClassSummary(drop_na(p_xgb_ma), lev = levels(p_xgb_ma$obs))[[1]],
                 mnLogLoss(p_xgb_ma, lev = levels(p_xgb_ma$obs))[[1]])
perf_ea <- cbind(twoClassSummary(drop_na(p_xgb_ea), lev = levels(p_xgb_ea$obs))[[1]],
                 mnLogLoss(p_xgb_ea, lev = levels(p_xgb_ea$obs))[[1]])
perf_mr <- cbind(twoClassSummary(drop_na(p_xgb_mr), lev = levels(p_xgb_mr$obs))[[1]],
                 mnLogLoss(p_xgb_mr, lev = levels(p_xgb_mr$obs))[[1]])
perf_np <- cbind(twoClassSummary(drop_na(p_xgb_np), lev = levels(p_xgb_np$obs))[[1]],
                 mnLogLoss(p_xgb_np, lev = levels(p_xgb_np$obs))[[1]])
perf_ch <- cbind(twoClassSummary(drop_na(p_xgb_ch), lev = levels(p_xgb_ch$obs))[[1]],
                 mnLogLoss(p_xgb_ch, lev = levels(p_xgb_ch$obs))[[1]])
perf_ue <- cbind(twoClassSummary(drop_na(p_xgb_ue), lev = levels(p_xgb_ue$obs))[[1]],
                 mnLogLoss(p_xgb_ue, lev = levels(p_xgb_ue$obs))[[1]])
perf_ft <- cbind(twoClassSummary(drop_na(p_xgb_ft), lev = levels(p_xgb_ft$obs))[[1]],
                 mnLogLoss(p_xgb_ft, lev = levels(p_xgb_ft$obs))[[1]])

tab <- rbind(perf_li, perf_hi, perf_ua, perf_oa, perf_ma, perf_ea, perf_mr, perf_np, perf_ch, perf_ue, perf_ft)
tab

rtffile <- RTF("perf_d.doc")
addTable(rtffile, round(tab, digits = 3))
done(rtffile)
