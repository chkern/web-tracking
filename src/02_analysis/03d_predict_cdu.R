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

# CDU

model_c1 <- paste("CDU ~", paste(survey_demo, collapse="+"))
model_c2 <- paste(model_c1, paste("+"), paste(track_general, collapse="+"))
model_c3 <- paste(model_c1, paste("+"), paste(track_news_media, collapse="+"))
model_c4 <- paste(model_c1, paste("+"), paste(track_apps_domains, collapse="+"))
model_c5 <- paste("CDU ~", paste(track_general, collapse="+"))
model_c5 <- paste(model_c5, paste("+"), paste(track_news_media, collapse="+"))
model_c5 <- paste(model_c5, paste("+"), paste(track_apps_domains, collapse="+"))
model_c6 <- paste(model_c5, paste("+"), paste(survey_demo, collapse="+"))

# RF Grid

small <- ncol(model.matrix(eval(parse(text=model_c1)), X_back_track_train))
med <- ncol(model.matrix(eval(parse(text=model_c2)), X_back_track_train))
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

# CDU - survey_demo

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_c1a <- train(",model_c1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid0,
                      alpha = alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))
  part <- xgb_c1a$results
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
eval(parse(text=paste("xgb_c1 <- train(",model_c1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_c1
plot(xgb_c1)

set.seed(303493)
eval(parse(text=paste("rf_c1 <- train(",model_c1,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_c1
plot(rf_c1)

# CDU - survey_demo + track_general

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_c2a <- train(",model_c2,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid0,
                      alpha = alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))
  part <- xgb_c2a$results
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
eval(parse(text=paste("xgb_c2 <- train(",model_c2,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_c2
plot(xgb_c2)

set.seed(303493)
eval(parse(text=paste("rf_c2 <- train(",model_c2,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_c2
plot(rf_c2)

# CDU - survey_demo + track_news_media

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_c3a <- train(",model_c3,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid0,
                      alpha = alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))
  part <- xgb_c3a$results
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
eval(parse(text=paste("xgb_c3 <- train(",model_c3,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_c3
plot(xgb_c3)

set.seed(303493)
eval(parse(text=paste("rf_c3 <- train(",model_c3,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_c3
plot(rf_c3)

# CDU - survey_demo + track_apps_domains

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_c4a <- train(",model_c4,",
                        data = X_back_track_train,
                        method = 'xgbTree',
                        trControl = ctrl,
                        tuneGrid = xgb_grid0,
                        alpha = alpha,
                        metric = 'logLoss',
                        na.action = na.omit)")))
  part <- xgb_c4a$results
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
eval(parse(text=paste("xgb_c4 <- train(",model_c4,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_c4
plot(xgb_c4)

set.seed(303493)
eval(parse(text=paste("rf_c4 <- train(",model_c4,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_c4
plot(rf_c4)

# CDU - only tracking data

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_c5a <- train(",model_c5,",
                        data = X_back_track_train,
                        method = 'xgbTree',
                        trControl = ctrl,
                        tuneGrid = xgb_grid0,
                        alpha = alpha,
                        metric = 'logLoss',
                        na.action = na.omit)")))
  part <- xgb_c5a$results
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
eval(parse(text=paste("xgb_c5 <- train(",model_c5,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_c5
plot(xgb_c5)

set.seed(303493)
eval(parse(text=paste("rf_c5 <- train(",model_c5,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_c5
plot(rf_c5)

# CDU - tracking data + survey_demo

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_c6a <- train(",model_c6,",
                        data = X_back_track_train,
                        method = 'xgbTree',
                        trControl = ctrl,
                        tuneGrid = xgb_grid0,
                        alpha = alpha,
                        metric = 'logLoss',
                        na.action = na.omit)")))
  part <- xgb_c6a$results
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
eval(parse(text=paste("xgb_c6 <- train(",model_c6,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_c6
plot(xgb_c6)

set.seed(303493)
eval(parse(text=paste("rf_c6 <- train(",model_c6,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_c6
plot(rf_c6)

##################################################################################
# Variable Importance
##################################################################################

plot(varImp(xgb_c6), top = 10)

imp_xgb_c6 <- varImp(xgb_c6)$importance
imp_xgb_c6 <- rownames_to_column(imp_xgb_c6, "varname")

imp_xgb_c6 <-
  imp_xgb_c6 %>%
  top_n(10, Overall) %>%
  mutate(order = 11 - row_number())

match(imp_xgb_c6$varname, names(X_back_track_train))
# imp_xgb_c6$varname <- c("High school degree","Why unemployed","News rel. n","Age","Manual Worker","amazon.de","In own house","youtube.com","Google Search","wahlomat.de")

ggplot(imp_xgb_c6) +
  geom_point(aes(x = Overall, y = order)) + 
  geom_segment(aes(y = order, yend = order, x = 1, xend = Overall)) +
  labs(x = "Importance", y = "") +
  xlim(0, 100) +
  scale_y_continuous(
    breaks = imp_xgb_c6$order,
    labels = imp_xgb_c6$varname)
ggsave("p_imp_c.png", width = 6, height = 6)

plot(varImp(xgb_a6), top = 10)

##################################################################################
# Compare CV performance
##################################################################################

# CV plot - CDU

resamps <- resamples(list(xgb_c1, xgb_c2, xgb_c3, xgb_c4, xgb_c5, xgb_c6))
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
  theme(text = element_text(size = 15))
ggsave("p_resamp_c1.png", width = 7.5, height = 7)

ggplot(resamp) +
  geom_boxplot(aes(y = logLoss, x = fct_rev(model), fill = model)) +
  ylim(0.3, 0.6) +
  labs(x = "") +
  labs(y = "logLoss") +
  coord_flip() + 
  scale_fill_manual(values = c("#E6E6E6", "#AEAEAE", "#4D4D4D", "#4D4D4D", "#4D4D4D", "#4D4D4D")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
ggsave("p_resamp_c2.png", width = 7.5, height = 7)

difresamps <- diff(resamps)
summary(difresamps)$table$ROC
summary(difresamps)$table$logLoss

##################################################################################
# Predict in test data
##################################################################################

# X_back_track_test_c <- X_back_track_test[!is.na(X_back_track_test$CDU),]
p_xgb_c1 <- predict(xgb_c1, newdata = X_back_track_test, type = "prob")
p_xgb_c2 <- predict(xgb_c2, newdata = X_back_track_test, type = "prob")
p_xgb_c3 <- predict(xgb_c3, newdata = X_back_track_test, type = "prob")
p_xgb_c4 <- predict(xgb_c4, newdata = X_back_track_test, type = "prob")
p_xgb_c5 <- predict(xgb_c5, newdata = X_back_track_test, type = "prob")
p_xgb_c6 <- predict(xgb_c6, newdata = X_back_track_test, type = "prob")

# ROC and LogLoss - CDU

roc_xgb_c1 <- roc(response = X_back_track_test$CDU, predictor = p_xgb_c1$CDU)
roc_xgb_c2 <- roc(response = X_back_track_test$CDU, predictor = p_xgb_c2$CDU)
roc_xgb_c3 <- roc(response = X_back_track_test$CDU, predictor = p_xgb_c3$CDU)
roc_xgb_c4 <- roc(response = X_back_track_test$CDU, predictor = p_xgb_c4$CDU)
roc_xgb_c5 <- roc(response = X_back_track_test$CDU, predictor = p_xgb_c5$CDU)
roc_xgb_c6 <- roc(response = X_back_track_test$CDU, predictor = p_xgb_c6$CDU)

ggroc(list("Demo" = roc_xgb_c1, "Tracking" = roc_xgb_c5, "Demo+Tracking" = roc_xgb_c6)) +
  geom_abline(aes(intercept = 1, slope = 1)) +
  scale_colour_manual(name = "", values = c("#F8766D", "#00BA38", "#619CFF"),
                      breaks = c("Demo", "Tracking", "Demo+Tracking")) +
  theme(text = element_text(size = 13))

ggsave("p_roc_c.png", width = 7.5, height = 6)

p_xgb_c1$obs <- X_back_track_test$CDU
p_xgb_c1$pred <- predict(xgb_c1, newdata = X_back_track_test)
p_xgb_c2$obs <- X_back_track_test$CDU
p_xgb_c2$pred <- predict(xgb_c2, newdata = X_back_track_test)
p_xgb_c3$obs <- X_back_track_test$CDU
p_xgb_c3$pred <- predict(xgb_c3, newdata = X_back_track_test)
p_xgb_c4$obs <- X_back_track_test$CDU
p_xgb_c4$pred <- predict(xgb_c4, newdata = X_back_track_test)
p_xgb_c5$obs <- X_back_track_test$CDU
p_xgb_c5$pred <- predict(xgb_c5, newdata = X_back_track_test)
p_xgb_c6$obs <- X_back_track_test$CDU
p_xgb_c6$pred <- predict(xgb_c6, newdata = X_back_track_test)

perf_c1 <- cbind(twoClassSummary(drop_na(p_xgb_c1), lev = levels(p_xgb_c1$obs))[[1]],
                 mnLogLoss(p_xgb_c1, lev = levels(p_xgb_c1$obs))[[1]])
perf_c2 <- cbind(twoClassSummary(drop_na(p_xgb_c2), lev = levels(p_xgb_c2$obs))[[1]],
                 mnLogLoss(p_xgb_c2, lev = levels(p_xgb_c2$obs))[[1]])
perf_c3 <- cbind(twoClassSummary(drop_na(p_xgb_c3), lev = levels(p_xgb_c3$obs))[[1]],
                 mnLogLoss(p_xgb_c3, lev = levels(p_xgb_c3$obs))[[1]])
perf_c4 <- cbind(twoClassSummary(drop_na(p_xgb_c4), lev = levels(p_xgb_c4$obs))[[1]],
                 mnLogLoss(p_xgb_c4, lev = levels(p_xgb_c4$obs))[[1]])
perf_c5 <- cbind(twoClassSummary(drop_na(p_xgb_c5), lev = levels(p_xgb_c5$obs))[[1]],
                 mnLogLoss(p_xgb_c5, lev = levels(p_xgb_c5$obs))[[1]])
perf_c6 <- cbind(twoClassSummary(drop_na(p_xgb_c6), lev = levels(p_xgb_c6$obs))[[1]],
                 mnLogLoss(p_xgb_c6, lev = levels(p_xgb_c6$obs))[[1]])

tab <- rbind(perf_c1, perf_c2, perf_c3, perf_c4, perf_c5, perf_c6)
tab

rtffile <- RTF("perf_c.doc")
addTable(rtffile, round(tab, digits = 3))
done(rtffile)

# Performance at "optimal" threshold - CDU

prop.table(table(X_back_track_train$CDU))
roc_xgb_c1_t <- coords(roc_xgb_c1, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.25))

c_xgb_c1 <- as.factor(ifelse(p_xgb_c1$CDU > roc_xgb_c1_t[[1]], "CDU", "not_CDU"))
c_xgb_c2 <- as.factor(ifelse(p_xgb_c2$CDU > roc_xgb_c1_t[[1]], "CDU", "not_CDU"))
c_xgb_c3 <- as.factor(ifelse(p_xgb_c3$CDU > roc_xgb_c1_t[[1]], "CDU", "not_CDU"))
c_xgb_c4 <- as.factor(ifelse(p_xgb_c4$CDU > roc_xgb_c1_t[[1]], "CDU", "not_CDU"))
c_xgb_c5 <- as.factor(ifelse(p_xgb_c5$CDU > roc_xgb_c1_t[[1]], "CDU", "not_CDU"))
c_xgb_c6 <- as.factor(ifelse(p_xgb_c6$CDU > roc_xgb_c1_t[[1]], "CDU", "not_CDU"))

cm1 <- confusionMatrix(c_xgb_c1, X_back_track_test$CDU, positive = "CDU", mode = "everything")
cm2 <- confusionMatrix(c_xgb_c2, X_back_track_test$CDU, positive = "CDU", mode = "everything")
cm3 <- confusionMatrix(c_xgb_c3, X_back_track_test$CDU, positive = "CDU", mode = "everything")
cm4 <- confusionMatrix(c_xgb_c4, X_back_track_test$CDU, positive = "CDU", mode = "everything")
cm5 <- confusionMatrix(c_xgb_c5, X_back_track_test$CDU, positive = "CDU", mode = "everything")
cm6 <- confusionMatrix(c_xgb_c6, X_back_track_test$CDU, positive = "CDU", mode = "everything")

Demo <- c(cm1$overall[1], cm1$byClass[c(1:2,5,7)], cm1$overall[2])
Demo_Tracking_general <- c(cm2$overall[1], cm2$byClass[c(1:2,5,7)], cm2$overall[2])
Demo_Tracking_news <- c(cm3$overall[1], cm3$byClass[c(1:2,5,7)], cm3$overall[2])
Demo_Tracking_apps <- c(cm4$overall[1], cm4$byClass[c(1:2,5,7)], cm4$overall[2])
Tracking <- c(cm5$overall[1], cm5$byClass[c(1:2,5,7)], cm5$overall[2])
Demo_Tracking <- c(cm6$overall[1], cm6$byClass[c(1:2,5,7)], cm6$overall[2])

tab <- rbind(Demo, Tracking, Demo_Tracking_general, Demo_Tracking_news, Demo_Tracking_apps, Demo_Tracking)
tab

rtffile <- RTF("t_perf_c.doc")
addTable(rtffile, cbind(rownames(tab),round(tab, digits = 3)))
done(rtffile)
