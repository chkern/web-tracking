##################################################################################

library(plyr)
library(tidyverse)
library(foreign)
library(haven)
library(data.table)
library(caret)
library(magrittr)
library(glmnet)
library(randomForest)
library(doParallel)
library(snow)
library(devtools)

setwd("Z:\\swnsswml\\Respondi\\")

survey_w1 <- read_dta(file = ".\\survey_daten\\survey_data_w1.dta")

tracking_data <- readRDS(file = ".\\data\\data_prep_final_small.rds")

tracking_full <- readRDS(file = ".\\data\\data_prep_final.rds")

load(".\\background_info\\background.RData")

##################################################################################
# Prepare data (y = sinus, x = (1) background, (2) tracking, (3) tracking full)
##################################################################################

# y

survey_w1$sinus_mil <- as_factor(survey_w1$sinus_mil, labels = "values")
survey_w1$sinus_mil <- droplevels(survey_w1$sinus_mil)
summary(survey_w1$sinus_mil)

survey_w1$sinus_mil <- revalue(survey_w1$sinus_mil, c("SozialÃ¶kologisches Milieu (SÃ–K)" = "Sozialoekologisches Milieu (SOEK)",
                                                      "BÃ¼rgerliche Mitte (BÃœM)" = "Buergerliche Mitte (BUEM)",
                                                      "PrekÃ¤res Milieu (PRE)" = "Prekaeres Milieu (PRE)"))

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

zero_var <- nearZeroVar(tracking_data, freqCut = 99.9/0.1, names = TRUE, allowParallel = T)

ncol(tracking_data)
tracking_data <- tracking_data[ , -which(names(tracking_data) %in% zero_var)]
ncol(tracking_data)

# Tracking data full

tracking_full <- tracking_full[ , -which(names(tracking_full) %in% names0)]

zero_var <- nearZeroVar(tracking_full, freqCut = 99.9/0.1, names = TRUE, allowParallel = T)

ncol(tracking_full)
tracking_full <- tracking_full[ , -which(names(tracking_full) %in% zero_var)]
ncol(tracking_full)

tracking_full$data <- "Mobile or Web views"
tracking_full$data <- factor(tracking_full$data, levels = c("Mobile or Web views", "Mobile views Missing", "Web views/pages Missing"))
tracking_full$data[is.na(tracking_full$mv_CNT_accuweather.com)] <- "Mobile views Missing"
tracking_full$data[is.na(tracking_full$wv_CNT_1und1.de)] <- "Web views/pages Missing"

tracking_full[is.na(tracking_full)] <- 0 

# Merge

training <- left_join(back, y)
names1 <- names(training[2:ncol(training)])
training <- left_join(training, tracking_data)
names2 <- names(training[2:ncol(training)])
training <- left_join(training, tracking_full)

sum(is.na(training))
training$panelist_id <- NULL

# Deal with Missings

# sum(is.na(training_split))
# training_split1 <- na.omit(training_split)
# sapply(training_split, function(x) sum(is.na(x)))

nrow(training)
training <- na.omit(training)
nrow(training)

##################################################################################
# Train-test split
##################################################################################

set.seed(82582)
index <- createDataPartition(training$sinus_mil, p = 0.75, list = F)

train <- training[index, ]
test <- training[-index, ]

train1 <- train[,names1]
train2 <- train[,names2]

cvIndex <- createFolds(train$sinus_mil, 5, returnTrain = T)

# Setup 

ctrl  <- trainControl(method = "cv",
                      number = 5,
                      summaryFunction = multiClassSummary,
                      index = cvIndex)

cl <- makeCluster(detectCores())
registerDoParallel(cl)

##################################################################################
# Models
##################################################################################

# Random Forests

X1 <- model.matrix(sinus_mil ~ ., train1)[,-1]
sqrt(ncol(X1))

grid <- expand.grid(mtry = 5*1:10)

set.seed(03764)
rf1 <- train(sinus_mil ~ ., 
             data = train1,
             method = "rf",
             trControl = ctrl,
             tuneGrid = grid)

rf1
plot(rf1)
confusionMatrix(rf1)
varImp(rf1, top = 20)

X2 <- model.matrix(sinus_mil ~ ., train2)[,-1]
sqrt(ncol(X2))

grid <- expand.grid(mtry = 5*1:15)

set.seed(03764)
rf2 <- train(sinus_mil ~ ., 
             data = train2,
             method = "rf",
             trControl = ctrl,
             tuneGrid = grid)

rf2
plot(rf2)
confusionMatrix(rf2)
varImp(rf2, top = 20)

X3 <- model.matrix(sinus_mil ~ ., train)[,-1]
sqrt(ncol(X3))

grid <- expand.grid(mtry = 5*1:20)

set.seed(03764)
rf3 <- train(sinus_mil ~ ., 
             data = train,
             method = "rf",
             trControl = ctrl,
             tuneGrid = grid)

rf3
plot(rf3)
confusionMatrix(rf3)

png("varImp_rf3.png", width = 10, height = 7, units = 'in', res = 300)
rf3_imp <- varImp(rf3)
plot(rf3_imp, top = 20)
dev.off()

# Lasso regression

grid <- expand.grid(alpha = c(0, 0.5, 1),
                    lambda = seq(0.3, 0, length = 30))

set.seed(03764)
lasso1 <- train(sinus_mil ~ ., 
                data = train1,
                method = "glmnet",
                family = "multinomial",
                #type.multinomial = "grouped",
                trControl = ctrl,
                tuneGrid = grid)

lasso1
plot(lasso1)
confusionMatrix(lasso1)
varImp(lasso1, top = 20)

# coefs1 <- coef(lasso1$finalModel, lasso1$bestTune$lambda)

set.seed(03764)
lasso2 <- train(sinus_mil ~ ., 
                data = train2,
                method = "glmnet",
                family = "multinomial",
                #type.multinomial = "grouped",
                trControl = ctrl,
                tuneGrid = grid)

lasso2
plot(lasso2)
confusionMatrix(lasso2)
varImp(lasso2, top = 20)

set.seed(03764)
lasso3 <- train(sinus_mil ~ ., 
                data = train,
                method = "glmnet",
                family = "multinomial",
                #type.multinomial = "grouped",
                trControl = ctrl,
                tuneGrid = grid)

lasso3

png("glmnet3_tune.png", width = 10, height = 7, units = 'in', res = 300)
plot(lasso3)
dev.off()

confusionMatrix(lasso3)
varImp(lasso3, top = 20)

png("glmnet3_varImp.png", width = 17, height = 7, units = 'in', res = 300)
lasso3_imp <- varImp(lasso3)
plot(lasso3_imp, top = 20, scales = list(y = list(labels = c("wv_CNT_ergoquest3.eu", "mv_CNT_computerbase.de","m_1001[11] over 5.000 EUR","mv_v_10.3.1","md_1000[13] Aerospace, Defence industry","mv_https_rel_dur","wv_DN_netflix.com","wv_CNT_elster.de ","md_1000[14] Education, Schools, ...","md_1181[7] Currently unemployed","mv_dev_TCL","m_1000[9] 4.000 to 4.500 EUR","md_1223[18] Arzt / Doctor","mv_v_10.2.1","md_1000[43] Travel agency or operator","mv_DN_VpnDialogs","mv_CNT_kerala.me","mv_DN_Farm Heroes Saga","mv_DN_Market","wv_DN_youtube.com"))))
dev.off()

stopCluster(cl)

##################################################################################
# Compare CV performance and plot trees
##################################################################################

resamps1 <- resamples(list(glmnet_base = lasso1,
                           glmnet_track1 = lasso2,
                           glmnet_track2 = lasso3))

summary(resamps1)
png("CV_glmnet.png", width = 10, height = 7, units = 'in', res = 300)
bwplot(resamps1, metric = c("Accuracy", "Kappa"))
dev.off()

resamps2 <- resamples(list(RF_base = rf1,
                          RF_track1 = rf2,
                          RF_track2 = rf3))

summary(resamps2)
png("CV_rf.png", width = 10, height = 7, units = 'in', res = 300)
bwplot(resamps2, metric = c("Accuracy", "Kappa"))
dev.off()

# install_github("araastat/reprtree")

# reptree1 <- ReprTree(rf1, train, metric = 'd2')
# plot(reptree1)
# reptree2 <- ReprTree(rf2, train, metric = 'd2')
# plot(reptree2)
# reptree3 <- ReprTree(rf3, train, metric = 'd2')
# plot(reptree3)

##################################################################################
# Predict in test data
##################################################################################

c_lasso1 <- predict(lasso1, newdata = test)
c_rf1 <- predict(rf1, newdata = test)
c_lasso2 <- predict(lasso2, newdata = test)
c_rf2 <- predict(rf2, newdata = test)
c_lasso3 <- predict(lasso3, newdata = test)
c_rf3 <- predict(rf3, newdata = test)

confusionMatrix(c_lasso1, test$sinus_mil)
postResample(c_lasso1, test$sinus_mil)
confusionMatrix(c_rf1, test$sinus_mil)
postResample(c_rf1, test$sinus_mil)

confusionMatrix(c_lasso2, test$sinus_mil)
postResample(c_lasso2, test$sinus_mil)
confusionMatrix(c_rf2, test$sinus_mil)
postResample(c_rf2, test$sinus_mil)

confusionMatrix(c_lasso3, test$sinus_mil)
postResample(c_lasso3, test$sinus_mil)
confusionMatrix(c_rf3, test$sinus_mil)
postResample(c_rf3, test$sinus_mil)
