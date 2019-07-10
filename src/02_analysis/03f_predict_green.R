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

# GREEN

model_g1 <- paste("GREEN ~", paste(survey_demo, collapse="+"))
model_g2 <- paste(model_g1, paste("+"), paste(track_general, collapse="+"))
model_g3 <- paste(model_g1, paste("+"), paste(track_news_media, collapse="+"))
model_g4 <- paste(model_g1, paste("+"), paste(track_apps_domains, collapse="+"))
model_g5 <- paste("GREEN ~", paste(track_general, collapse="+"))
model_g5 <- paste(model_g5, paste("+"), paste(track_news_media, collapse="+"))
model_g5 <- paste(model_g5, paste("+"), paste(track_apps_domains, collapse="+"))
model_g6 <- paste(model_g5, paste("+"), paste(survey_demo, collapse="+"))

# RF Grid

small <- ncol(model.matrix(eval(parse(text=model_g1)), X_back_track_train))
med <- ncol(model.matrix(eval(parse(text=model_g2)), X_back_track_train))
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

# GREEN - survey_demo

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_g1a <- train(",model_g1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid0,
                      alpha = alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))
  part <- xgb_g1a$results
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
eval(parse(text=paste("xgb_g1 <- train(",model_g1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_g1
plot(xgb_g1)

set.seed(303493)
eval(parse(text=paste("rf_g1 <- train(",model_g1,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_g1
plot(rf_g1)

# GREEN - survey_demo + track_general

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_g2a <- train(",model_g2,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid0,
                      alpha = alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))
  part <- xgb_g2a$results
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
eval(parse(text=paste("xgb_g2 <- train(",model_g2,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_g2
plot(xgb_g2)

set.seed(303493)
eval(parse(text=paste("rf_g2 <- train(",model_g2,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_g2
plot(rf_g2)

# GREEN - survey_demo + track_news_media

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_g3a <- train(",model_g3,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid0,
                      alpha = alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))
  part <- xgb_g3a$results
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
eval(parse(text=paste("xgb_g3 <- train(",model_g3,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_g3
plot(xgb_g3)

set.seed(303493)
eval(parse(text=paste("rf_g3 <- train(",model_g3,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_g3
plot(rf_g3)

# GREEN - survey_demo + track_apps_domains

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_g4a <- train(",model_g4,",
                        data = X_back_track_train,
                        method = 'xgbTree',
                        trControl = ctrl,
                        tuneGrid = xgb_grid0,
                        alpha = alpha,
                        metric = 'logLoss',
                        na.action = na.omit)")))
  part <- xgb_g4a$results
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
eval(parse(text=paste("xgb_g4 <- train(",model_g4,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_g4
plot(xgb_g4)

set.seed(303493)
eval(parse(text=paste("rf_g4 <- train(",model_g4,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_g4
plot(rf_g4)

# GREEN - only tracking data

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_g5a <- train(",model_g5,",
                        data = X_back_track_train,
                        method = 'xgbTree',
                        trControl = ctrl,
                        tuneGrid = xgb_grid0,
                        alpha = alpha,
                        metric = 'logLoss',
                        na.action = na.omit)")))
  part <- xgb_g5a$results
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
eval(parse(text=paste("xgb_g5 <- train(",model_g5,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_g5
plot(xgb_g5)

set.seed(303493)
eval(parse(text=paste("rf_g5 <- train(",model_g5,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_g5
plot(rf_g5)

# GREEN - tracking data + survey_demo

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_g6a <- train(",model_g6,",
                        data = X_back_track_train,
                        method = 'xgbTree',
                        trControl = ctrl,
                        tuneGrid = xgb_grid0,
                        alpha = alpha,
                        metric = 'logLoss',
                        na.action = na.omit)")))
  part <- xgb_g6a$results
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
eval(parse(text=paste("xgb_g6 <- train(",model_g6,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_g6
plot(xgb_g6)

set.seed(303493)
eval(parse(text=paste("rf_g6 <- train(",model_g6,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_g6
plot(rf_g6)

##################################################################################
# Variable Importance
##################################################################################

plot(varImp(xgb_g6), top = 10)

imp_xgb_g6 <- varImp(xgb_g6)$importance
imp_xgb_g6 <- rownames_to_column(imp_xgb_g6, "varname")

imp_xgb_g6 <-
  imp_xgb_g6 %>%
  top_n(10, Overall) %>%
  mutate(order = 11 - row_number())

match(imp_xgb_g6$varname, names(X_back_track_train))
# imp_xgb_g6$varname <- c("High school degree","Why unemployed","News rel. n","Age","Manual Worker","amazon.de","In own house","youtube.com","Google Search","wahlomat.de")

ggplot(imp_xgb_g6) +
  geom_point(aes(x = Overall, y = order)) + 
  geom_segment(aes(y = order, yend = order, x = 1, xend = Overall)) +
  labs(x = "Importance", y = "") +
  xlim(0, 100) +
  scale_y_continuous(
    breaks = imp_xgb_g6$order,
    labels = imp_xgb_g6$varname)
ggsave("p_imp_g.png", width = 6, height = 6)

##################################################################################
# Compare CV performance
##################################################################################

# CV plot - GREEN

resamps <- resamples(list(xgb_g1, xgb_g2, xgb_g3, xgb_g4, xgb_g5, xgb_g6))
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
ggsave("p_resamp_g1.png", width = 7.5, height = 7)

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
ggsave("p_resamp_g2.png", width = 7.5, height = 7)

difresamps <- diff(resamps)
summary(difresamps)$table$ROC
summary(difresamps)$table$logLoss

##################################################################################
# Predict in test data
##################################################################################

# X_back_track_test_g <- X_back_track_test[!is.na(X_back_track_test$GREEN),]
p_xgb_g1 <- predict(xgb_g1, newdata = X_back_track_test, type = "prob")
p_xgb_g2 <- predict(xgb_g2, newdata = X_back_track_test, type = "prob")
p_xgb_g3 <- predict(xgb_g3, newdata = X_back_track_test, type = "prob")
p_xgb_g4 <- predict(xgb_g4, newdata = X_back_track_test, type = "prob")
p_xgb_g5 <- predict(xgb_g5, newdata = X_back_track_test, type = "prob")
p_xgb_g6 <- predict(xgb_g6, newdata = X_back_track_test, type = "prob")

# ROC and LogLoss - GREEN

roc_xgb_g1 <- roc(response = X_back_track_test$GREEN, predictor = p_xgb_g1$GREEN)
roc_xgb_g2 <- roc(response = X_back_track_test$GREEN, predictor = p_xgb_g2$GREEN)
roc_xgb_g3 <- roc(response = X_back_track_test$GREEN, predictor = p_xgb_g3$GREEN)
roc_xgb_g4 <- roc(response = X_back_track_test$GREEN, predictor = p_xgb_g4$GREEN)
roc_xgb_g5 <- roc(response = X_back_track_test$GREEN, predictor = p_xgb_g5$GREEN)
roc_xgb_g6 <- roc(response = X_back_track_test$GREEN, predictor = p_xgb_g6$GREEN)

ggroc(list("Demo" = roc_xgb_g1, "Tracking" = roc_xgb_g5, "Demo+Tracking" = roc_xgb_g6)) +
  geom_abline(aes(intercept = 1, slope = 1)) +
  scale_colour_manual(name = "", values = c("#F8766D", "#00BA38", "#619CFF"),
                      breaks = c("Demo", "Tracking", "Demo+Tracking")) +
  theme(text = element_text(size = 13))

ggsave("p_roc_g.png", width = 7.5, height = 6)

p_xgb_g1$obs <- X_back_track_test$GREEN
p_xgb_g1$pred <- predict(xgb_g1, newdata = X_back_track_test)
p_xgb_g2$obs <- X_back_track_test$GREEN
p_xgb_g2$pred <- predict(xgb_g2, newdata = X_back_track_test)
p_xgb_g3$obs <- X_back_track_test$GREEN
p_xgb_g3$pred <- predict(xgb_g3, newdata = X_back_track_test)
p_xgb_g4$obs <- X_back_track_test$GREEN
p_xgb_g4$pred <- predict(xgb_g4, newdata = X_back_track_test)
p_xgb_g5$obs <- X_back_track_test$GREEN
p_xgb_g5$pred <- predict(xgb_g5, newdata = X_back_track_test)
p_xgb_g6$obs <- X_back_track_test$GREEN
p_xgb_g6$pred <- predict(xgb_g6, newdata = X_back_track_test)

perf_g1 <- cbind(twoClassSummary(drop_na(p_xgb_g1), lev = levels(p_xgb_g1$obs))[[1]],
                 mnLogLoss(p_xgb_g1, lev = levels(p_xgb_g1$obs))[[1]])
perf_g2 <- cbind(twoClassSummary(drop_na(p_xgb_g2), lev = levels(p_xgb_g2$obs))[[1]],
                 mnLogLoss(p_xgb_g2, lev = levels(p_xgb_g2$obs))[[1]])
perf_g3 <- cbind(twoClassSummary(drop_na(p_xgb_g3), lev = levels(p_xgb_g3$obs))[[1]],
                 mnLogLoss(p_xgb_g3, lev = levels(p_xgb_g3$obs))[[1]])
perf_g4 <- cbind(twoClassSummary(drop_na(p_xgb_g4), lev = levels(p_xgb_g4$obs))[[1]],
                 mnLogLoss(p_xgb_g4, lev = levels(p_xgb_g4$obs))[[1]])
perf_g5 <- cbind(twoClassSummary(drop_na(p_xgb_g5), lev = levels(p_xgb_g5$obs))[[1]],
                 mnLogLoss(p_xgb_g5, lev = levels(p_xgb_g5$obs))[[1]])
perf_g6 <- cbind(twoClassSummary(drop_na(p_xgb_g6), lev = levels(p_xgb_g6$obs))[[1]],
                 mnLogLoss(p_xgb_g6, lev = levels(p_xgb_g6$obs))[[1]])

tab <- rbind(perf_g1, perf_g2, perf_g3, perf_g4, perf_g5, perf_g6)
tab

rtffile <- RTF("perf_g.doc")
addTable(rtffile, round(tab, digits = 3))
done(rtffile)

# Performance at "optimal" threshold - GREEN

prop.table(table(X_back_track_train$GREEN))
roc_xgb_g1_t <- coords(roc_xgb_g1, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.1))

c_xgb_g1 <- as.factor(ifelse(p_xgb_g1$GREEN > roc_xgb_g1_t[[1]], "GREEN", "not_GREEN"))
c_xgb_g2 <- as.factor(ifelse(p_xgb_g2$GREEN > roc_xgb_g1_t[[1]], "GREEN", "not_GREEN"))
c_xgb_g3 <- as.factor(ifelse(p_xgb_g3$GREEN > roc_xgb_g1_t[[1]], "GREEN", "not_GREEN"))
c_xgb_g4 <- as.factor(ifelse(p_xgb_g4$GREEN > roc_xgb_g1_t[[1]], "GREEN", "not_GREEN"))
c_xgb_g5 <- as.factor(ifelse(p_xgb_g5$GREEN > roc_xgb_g1_t[[1]], "GREEN", "not_GREEN"))
c_xgb_g6 <- as.factor(ifelse(p_xgb_g6$GREEN > roc_xgb_g1_t[[1]], "GREEN", "not_GREEN"))

cm1 <- confusionMatrix(c_xgb_g1, X_back_track_test$GREEN, positive = "GREEN", mode = "everything")
cm2 <- confusionMatrix(c_xgb_g2, X_back_track_test$GREEN, positive = "GREEN", mode = "everything")
cm3 <- confusionMatrix(c_xgb_g3, X_back_track_test$GREEN, positive = "GREEN", mode = "everything")
cm4 <- confusionMatrix(c_xgb_g4, X_back_track_test$GREEN, positive = "GREEN", mode = "everything")
cm5 <- confusionMatrix(c_xgb_g5, X_back_track_test$GREEN, positive = "GREEN", mode = "everything")
cm6 <- confusionMatrix(c_xgb_g6, X_back_track_test$GREEN, positive = "GREEN", mode = "everything")

Demo <- c(cm1$overall[1], cm1$byClass[c(1:2,5,7)], cm1$overall[2])
Demo_Tracking_general <- c(cm2$overall[1], cm2$byClass[c(1:2,5,7)], cm2$overall[2])
Demo_Tracking_news <- c(cm3$overall[1], cm3$byClass[c(1:2,5,7)], cm3$overall[2])
Demo_Tracking_apps <- c(cm4$overall[1], cm4$byClass[c(1:2,5,7)], cm4$overall[2])
Tracking <- c(cm5$overall[1], cm5$byClass[c(1:2,5,7)], cm5$overall[2])
Demo_Tracking <- c(cm6$overall[1], cm6$byClass[c(1:2,5,7)], cm6$overall[2])

tab <- rbind(Demo, Tracking, Demo_Tracking_general, Demo_Tracking_news, Demo_Tracking_apps, Demo_Tracking)
tab

rtffile <- RTF("t_perf_g.doc")
addTable(rtffile, cbind(rownames(tab),round(tab, digits = 3)))
done(rtffile)
