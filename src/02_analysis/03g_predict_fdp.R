##################################################################################

library(tidyverse)
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
load("prep_pol.Rdata")

##################################################################################
# Caret Setup
##################################################################################

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

# FDP

model_f1 <- paste("FDP ~", paste(survey_demo, collapse="+"))
model_f2 <- paste(model_f1, paste("+"), paste(track_general, collapse="+"))
model_f3 <- paste(model_f1, paste("+"), paste(track_news_media, collapse="+"))
model_f4 <- paste(model_f1, paste("+"), paste(track_apps_domains, collapse="+"))
model_f5 <- paste("FDP ~", paste(track_general, collapse="+"))
model_f5 <- paste(model_f5, paste("+"), paste(track_news_media, collapse="+"))
model_f5 <- paste(model_f5, paste("+"), paste(track_apps_domains, collapse="+"))
model_f6 <- paste(model_f5, paste("+"), paste(survey_demo, collapse="+"))

# RF Grid

small <- ncol(model.matrix(eval(parse(text=model_f1)), X_back_track_train))
med <- ncol(model.matrix(eval(parse(text=model_f2)), X_back_track_train))
full <-  small + length(track_apps_domains)

rf_grid <- expand.grid(mtry = c(round(sqrt(small)), round(log2(small)), round(sqrt(med)), round(log2(med)), round(sqrt(full)), round(log2(full))),
                       splitrule = c("gini", "extratrees"),
                       min.node.size = c(1, 5))

# XGBoost Grid I

xgb_grid0 <- expand.grid(max_depth = c(1, 3, 5, 7, 9, 11),
                         nrounds = 100,
                         eta = 0.05,
                         min_child_weight = 0:5,
                         subsample = 1,
                         gamma = c(0, 0.5, 1),
                         colsample_bytree = 1)
tune_alpha <- c(0, 0.5)

# FDP - survey_demo

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_f1a <- train(",model_f1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid0,
                      alpha = alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))
  part <- xgb_f1a$results
  part$alpha <- alpha
  results <- rbind(results, part)
}

best_depth <- results$max_depth[which.min(results[,"logLoss"])]
best_child <- results$min_child_weight[which.min(results[,"logLoss"])]
best_gamma <- results$gamma[which.min(results[,"logLoss"])]
best_alpha <- results$alpha[which.min(results[,"logLoss"])]

xgb_grid <- expand.grid(max_depth = c(best_depth-1, best_depth, best_depth+1),
                        nrounds = c(250, 500, 750, 1000),
                        eta = c(0.025, 0.01),
                        min_child_weight = best_child,
                        subsample = c(0.7, 1),
                        gamma = best_gamma,
                        colsample_bytree = c(0.7, 1))
xgb_grid <- filter(xgb_grid, max_depth >= 1)

set.seed(303493)
eval(parse(text=paste("xgb_f1 <- train(",model_f1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_f1
plot(xgb_f1)

set.seed(303493)
eval(parse(text=paste("rf_f1 <- train(",model_f1,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_f1
plot(rf_f1)

# FDP - survey_demo + track_general

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_f2a <- train(",model_f2,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid0,
                      alpha = alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))
  part <- xgb_f2a$results
  part$alpha <- alpha
  results <- rbind(results, part)
}

best_depth <- results$max_depth[which.min(results[,"logLoss"])]
best_child <- results$min_child_weight[which.min(results[,"logLoss"])]
best_gamma <- results$gamma[which.min(results[,"logLoss"])]
best_alpha <- results$alpha[which.min(results[,"logLoss"])]

xgb_grid <- expand.grid(max_depth = c(best_depth-1, best_depth, best_depth+1),
                        nrounds = c(250, 500, 750, 1000),
                        eta = c(0.025, 0.01),
                        min_child_weight = best_child,
                        subsample = c(0.7, 1),
                        gamma = best_gamma,
                        colsample_bytree = c(0.7, 1))
xgb_grid <- filter(xgb_grid, max_depth >= 1)

set.seed(303493)
eval(parse(text=paste("xgb_f2 <- train(",model_f2,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_f2
plot(xgb_f2)

set.seed(303493)
eval(parse(text=paste("rf_f2 <- train(",model_f2,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_f2
plot(rf_f2)

# FDP - survey_demo + track_news_media

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_f3a <- train(",model_f3,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid0,
                      alpha = alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))
  part <- xgb_f3a$results
  part$alpha <- alpha
  results <- rbind(results, part)
}

best_depth <- results$max_depth[which.min(results[,"logLoss"])]
best_child <- results$min_child_weight[which.min(results[,"logLoss"])]
best_gamma <- results$gamma[which.min(results[,"logLoss"])]
best_alpha <- results$alpha[which.min(results[,"logLoss"])]

xgb_grid <- expand.grid(max_depth = c(best_depth-1, best_depth, best_depth+1),
                        nrounds = c(250, 500, 750, 1000),
                        eta = c(0.025, 0.01),
                        min_child_weight = best_child,
                        subsample = c(0.7, 1),
                        gamma = best_gamma,
                        colsample_bytree = c(0.7, 1))
xgb_grid <- filter(xgb_grid, max_depth >= 1)

set.seed(303493)
eval(parse(text=paste("xgb_f3 <- train(",model_f3,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_f3
plot(xgb_f3)

set.seed(303493)
eval(parse(text=paste("rf_f3 <- train(",model_f3,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_f3
plot(rf_f3)

# FDP - survey_demo + track_apps_domains

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_f4a <- train(",model_f4,",
                        data = X_back_track_train,
                        method = 'xgbTree',
                        trControl = ctrl,
                        tuneGrid = xgb_grid0,
                        alpha = alpha,
                        metric = 'logLoss',
                        na.action = na.omit)")))
  part <- xgb_f4a$results
  part$alpha <- alpha
  results <- rbind(results, part)
}

best_depth <- results$max_depth[which.min(results[,"logLoss"])]
best_child <- results$min_child_weight[which.min(results[,"logLoss"])]
best_gamma <- results$gamma[which.min(results[,"logLoss"])]
best_alpha <- results$alpha[which.min(results[,"logLoss"])]

xgb_grid <- expand.grid(max_depth = c(best_depth-1, best_depth, best_depth+1),
                        nrounds = c(250, 500, 750, 1000),
                        eta = c(0.025, 0.01),
                        min_child_weight = best_child,
                        subsample = c(0.7, 1),
                        gamma = best_gamma,
                        colsample_bytree = c(0.7, 1))
xgb_grid <- filter(xgb_grid, max_depth >= 1)

set.seed(303493)
eval(parse(text=paste("xgb_f4 <- train(",model_f4,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_f4
plot(xgb_f4)

set.seed(303493)
eval(parse(text=paste("rf_f4 <- train(",model_f4,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_f4
plot(rf_f4)

# FDP - only tracking data

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_f5a <- train(",model_f5,",
                        data = X_back_track_train,
                        method = 'xgbTree',
                        trControl = ctrl,
                        tuneGrid = xgb_grid0,
                        alpha = alpha,
                        metric = 'logLoss',
                        na.action = na.omit)")))
  part <- xgb_f5a$results
  part$alpha <- alpha
  results <- rbind(results, part)
}

best_depth <- results$max_depth[which.min(results[,"logLoss"])]
best_child <- results$min_child_weight[which.min(results[,"logLoss"])]
best_gamma <- results$gamma[which.min(results[,"logLoss"])]
best_alpha <- results$alpha[which.min(results[,"logLoss"])]

xgb_grid <- expand.grid(max_depth = c(best_depth-1, best_depth, best_depth+1),
                        nrounds = c(250, 500, 750, 1000),
                        eta = c(0.025, 0.01),
                        min_child_weight = best_child,
                        subsample = c(0.7, 1),
                        gamma = best_gamma,
                        colsample_bytree = c(0.7, 1))
xgb_grid <- filter(xgb_grid, max_depth >= 1)

set.seed(303493)
eval(parse(text=paste("xgb_f5 <- train(",model_f5,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_f5
plot(xgb_f5)

set.seed(303493)
eval(parse(text=paste("rf_f5 <- train(",model_f5,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_f5
plot(rf_f5)

# FDP - tracking data + survey_demo

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_f6a <- train(",model_f6,",
                        data = X_back_track_train,
                        method = 'xgbTree',
                        trControl = ctrl,
                        tuneGrid = xgb_grid0,
                        alpha = alpha,
                        metric = 'logLoss',
                        na.action = na.omit)")))
  part <- xgb_f6a$results
  part$alpha <- alpha
  results <- rbind(results, part)
}

best_depth <- results$max_depth[which.min(results[,"logLoss"])]
best_child <- results$min_child_weight[which.min(results[,"logLoss"])]
best_gamma <- results$gamma[which.min(results[,"logLoss"])]
best_alpha <- results$alpha[which.min(results[,"logLoss"])]

xgb_grid <- expand.grid(max_depth = c(best_depth-1, best_depth, best_depth+1),
                        nrounds = c(250, 500, 750, 1000),
                        eta = c(0.025, 0.01),
                        min_child_weight = best_child,
                        subsample = c(0.7, 1),
                        gamma = best_gamma,
                        colsample_bytree = c(0.7, 1))
xgb_grid <- filter(xgb_grid, max_depth >= 1)

set.seed(303493)
eval(parse(text=paste("xgb_f6 <- train(",model_f6,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_f6
plot(xgb_f6)

set.seed(303493)
eval(parse(text=paste("rf_f6 <- train(",model_f6,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_f6
plot(rf_f6)

##################################################################################
# Variable Importance
##################################################################################

plot(varImp(xgb_f6), top = 10)

imp_xgb_f6 <- varImp(xgb_f6)$importance
imp_xgb_f6 <- rownames_to_column(imp_xgb_f6, "varname")

imp_xgb_f6 <-
  imp_xgb_f6 %>%
  top_n(10, Overall) %>%
  mutate(order = 11 - row_number())

match(imp_xgb_f6$varname, names(X_back_track_train))
# imp_xgb_f6$varname <- c("High school degree","Why unemployed","News rel. n","Age","Manual Worker","amazon.de","In own house","youtube.com","Google Search","wahlomat.de")

ggplot(imp_xgb_f6) +
  geom_point(aes(x = Overall, y = order)) + 
  geom_segment(aes(y = order, yend = order, x = 1, xend = Overall)) +
  labs(x = "Importance", y = "") +
  xlim(0, 100) +
  scale_y_continuous(
    breaks = imp_xgb_f6$order,
    labels = imp_xgb_f6$varname)
ggsave("p_imp_f.png", width = 6, height = 6)

##################################################################################
# Compare CV performance
##################################################################################

# CV plot - FDP

resamps <- resamples(list(xgb_f1, xgb_f2, xgb_f3, xgb_f4, xgb_f5, xgb_f6))
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
                            "Demo" = "1",
                            "Demo+Track_general" = "2",
                            "Demo+Track_news" = "3",
                            "Demo+Track_domains_apps" = "4",
                            "Tracking" = "5",
                            "Demo+Tracking" = "6")) %>%
  mutate(model = fct_relevel(model, "Tracking", after = 1))

ggplot(resamp) +
  geom_boxplot(aes(y = ROC, x = fct_rev(model), fill = model)) +
  ylim(0, 1) +
  labs(x = "") +
  labs(y = "ROC-AUC") +
  coord_flip() + 
  scale_fill_manual(values = c("#E6E6E6", "#AEAEAE", "#4D4D4D", "#4D4D4D", "#4D4D4D", "#4D4D4D")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 18))
ggsave("p_resamp_f1.png", width = 7.5, height = 7)

ggplot(resamp) +
  geom_boxplot(aes(y = logLoss, x = fct_rev(model), fill = model)) +
  ylim(0.2, 0.5) +
  labs(x = "") +
  labs(y = "logLoss") +
  coord_flip() + 
  scale_fill_manual(values = c("#E6E6E6", "#AEAEAE", "#4D4D4D", "#4D4D4D", "#4D4D4D", "#4D4D4D")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 18))
ggsave("p_resamp_f2.png", width = 7.5, height = 7)

difresamps <- diff(resamps)
summary(difresamps)$table$ROC
summary(difresamps)$table$logLoss

##################################################################################
# Predict in test data
##################################################################################

# X_back_track_test_f <- X_back_track_test[!is.na(X_back_track_test$FDP),]
p_xgb_f1 <- predict(xgb_f1, newdata = X_back_track_test, type = "prob")
p_xgb_f2 <- predict(xgb_f2, newdata = X_back_track_test, type = "prob")
p_xgb_f3 <- predict(xgb_f3, newdata = X_back_track_test, type = "prob")
p_xgb_f4 <- predict(xgb_f4, newdata = X_back_track_test, type = "prob")
p_xgb_f5 <- predict(xgb_f5, newdata = X_back_track_test, type = "prob")
p_xgb_f6 <- predict(xgb_f6, newdata = X_back_track_test, type = "prob")

# ROC and LogLoss - FDP

roc_xgb_f1 <- roc(response = X_back_track_test$FDP, predictor = p_xgb_f1$FDP)
roc_xgb_f2 <- roc(response = X_back_track_test$FDP, predictor = p_xgb_f2$FDP)
roc_xgb_f3 <- roc(response = X_back_track_test$FDP, predictor = p_xgb_f3$FDP)
roc_xgb_f4 <- roc(response = X_back_track_test$FDP, predictor = p_xgb_f4$FDP)
roc_xgb_f5 <- roc(response = X_back_track_test$FDP, predictor = p_xgb_f5$FDP)
roc_xgb_f6 <- roc(response = X_back_track_test$FDP, predictor = p_xgb_f6$FDP)

ggroc(list("Demo" = roc_xgb_f1, "Tracking" = roc_xgb_f5, "Demo+Tracking" = roc_xgb_f6)) +
  geom_abline(aes(intercept = 1, slope = 1)) +
  scale_colour_manual(name = "", values = c("#F8766D", "#00BA38", "#619CFF"),
                      breaks = c("Demo", "Tracking", "Demo+Tracking")) +
  theme(text = element_text(size = 13))

ggsave("p_roc_f.png", width = 7.5, height = 6)

p_xgb_f1$obs <- X_back_track_test$FDP
p_xgb_f1$pred <- predict(xgb_f1, newdata = X_back_track_test)
p_xgb_f2$obs <- X_back_track_test$FDP
p_xgb_f2$pred <- predict(xgb_f2, newdata = X_back_track_test)
p_xgb_f3$obs <- X_back_track_test$FDP
p_xgb_f3$pred <- predict(xgb_f3, newdata = X_back_track_test)
p_xgb_f4$obs <- X_back_track_test$FDP
p_xgb_f4$pred <- predict(xgb_f4, newdata = X_back_track_test)
p_xgb_f5$obs <- X_back_track_test$FDP
p_xgb_f5$pred <- predict(xgb_f5, newdata = X_back_track_test)
p_xgb_f6$obs <- X_back_track_test$FDP
p_xgb_f6$pred <- predict(xgb_f6, newdata = X_back_track_test)

perf_f1 <- cbind(twoClassSummary(drop_na(p_xgb_f1), lev = levels(p_xgb_f1$obs))[[1]],
                 mnLogLoss(p_xgb_f1, lev = levels(p_xgb_f1$obs))[[1]])
perf_f2 <- cbind(twoClassSummary(drop_na(p_xgb_f2), lev = levels(p_xgb_f2$obs))[[1]],
                 mnLogLoss(p_xgb_f2, lev = levels(p_xgb_f2$obs))[[1]])
perf_f3 <- cbind(twoClassSummary(drop_na(p_xgb_f3), lev = levels(p_xgb_f3$obs))[[1]],
                 mnLogLoss(p_xgb_f3, lev = levels(p_xgb_f3$obs))[[1]])
perf_f4 <- cbind(twoClassSummary(drop_na(p_xgb_f4), lev = levels(p_xgb_f4$obs))[[1]],
                 mnLogLoss(p_xgb_f4, lev = levels(p_xgb_f4$obs))[[1]])
perf_f5 <- cbind(twoClassSummary(drop_na(p_xgb_f5), lev = levels(p_xgb_f5$obs))[[1]],
                 mnLogLoss(p_xgb_f5, lev = levels(p_xgb_f5$obs))[[1]])
perf_f6 <- cbind(twoClassSummary(drop_na(p_xgb_f6), lev = levels(p_xgb_f6$obs))[[1]],
                 mnLogLoss(p_xgb_f6, lev = levels(p_xgb_f6$obs))[[1]])

tab <- rbind(perf_f1, perf_f2, perf_f3, perf_f4, perf_f5, perf_f6)
tab

rtffile <- RTF("perf_f.doc")
addTable(rtffile, round(tab, digits = 3))
done(rtffile)

# Performance at "optimal" threshold - FDP

prop.table(table(X_back_track_train$FDP))
roc_xgb_f1_t <- coords(roc_xgb_f1, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.9))

c_xgb_f1 <- as.factor(ifelse(p_xgb_f1$FDP > roc_xgb_f1_t[[1]], "FDP", "not_FDP"))
c_xgb_f2 <- as.factor(ifelse(p_xgb_f2$FDP > roc_xgb_f1_t[[1]], "FDP", "not_FDP"))
c_xgb_f3 <- as.factor(ifelse(p_xgb_f3$FDP > roc_xgb_f1_t[[1]], "FDP", "not_FDP"))
c_xgb_f4 <- as.factor(ifelse(p_xgb_f4$FDP > roc_xgb_f1_t[[1]], "FDP", "not_FDP"))
c_xgb_f5 <- as.factor(ifelse(p_xgb_f5$FDP > roc_xgb_f1_t[[1]], "FDP", "not_FDP"))
c_xgb_f6 <- as.factor(ifelse(p_xgb_f6$FDP > roc_xgb_f1_t[[1]], "FDP", "not_FDP"))

cm1 <- confusionMatrix(c_xgb_f1, X_back_track_test$FDP, positive = "FDP", mode = "everything")
cm2 <- confusionMatrix(c_xgb_f2, X_back_track_test$FDP, positive = "FDP", mode = "everything")
cm3 <- confusionMatrix(c_xgb_f3, X_back_track_test$FDP, positive = "FDP", mode = "everything")
cm4 <- confusionMatrix(c_xgb_f4, X_back_track_test$FDP, positive = "FDP", mode = "everything")
cm5 <- confusionMatrix(c_xgb_f5, X_back_track_test$FDP, positive = "FDP", mode = "everything")
cm6 <- confusionMatrix(c_xgb_f6, X_back_track_test$FDP, positive = "FDP", mode = "everything")

Demo <- c(cm1$overall[1], cm1$byClass[c(1:2,5,7)], cm1$overall[2])
Demo_Tracking_general <- c(cm2$overall[1], cm2$byClass[c(1:2,5,7)], cm2$overall[2])
Demo_Tracking_news <- c(cm3$overall[1], cm3$byClass[c(1:2,5,7)], cm3$overall[2])
Demo_Tracking_apps <- c(cm4$overall[1], cm4$byClass[c(1:2,5,7)], cm4$overall[2])
Tracking <- c(cm5$overall[1], cm5$byClass[c(1:2,5,7)], cm5$overall[2])
Demo_Tracking <- c(cm6$overall[1], cm6$byClass[c(1:2,5,7)], cm6$overall[2])

tab <- rbind(Demo, Tracking, Demo_Tracking_general, Demo_Tracking_news, Demo_Tracking_apps, Demo_Tracking)
tab

rtffile <- RTF("t_perf_f.doc")
addTable(rtffile, cbind(rownames(tab),round(tab, digits = 3)))
done(rtffile)
