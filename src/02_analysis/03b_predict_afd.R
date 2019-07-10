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
library(pdp)

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

# AFD

model_a1 <- paste("AFD ~", paste(survey_demo, collapse="+"))
model_a2 <- paste(model_a1, paste("+"), paste(track_general, collapse="+"))
model_a3 <- paste(model_a1, paste("+"), paste(track_news_media, collapse="+"))
model_a4 <- paste(model_a1, paste("+"), paste(track_apps_domains, collapse="+"))
model_a5 <- paste("AFD ~", paste(track_general, collapse="+"))
model_a5 <- paste(model_a5, paste("+"), paste(track_news_media, collapse="+"))
model_a5 <- paste(model_a5, paste("+"), paste(track_apps_domains, collapse="+"))
model_a6 <- paste(model_a5, paste("+"), paste(survey_demo, collapse="+"))

# RF Grid

small <- ncol(model.matrix(eval(parse(text=model_a1)), X_back_track_train))
med <- ncol(model.matrix(eval(parse(text=model_a2)), X_back_track_train))
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

# AFD - survey_demo

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_a1a <- train(",model_a1,",
                        data = X_back_track_train,
                        method = 'xgbTree',
                        trControl = ctrl,
                        tuneGrid = xgb_grid0,
                        alpha = alpha,
                        metric = 'logLoss',
                        na.action = na.omit)")))
  part <- xgb_a1a$results
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
eval(parse(text=paste("xgb_a1 <- train(",model_a1,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_a1
plot(xgb_a1)

set.seed(303493)
eval(parse(text=paste("rf_a1 <- train(",model_a1,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_a1
plot(rf_a1)

# AFD - survey_demo + track_general

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_a2a <- train(",model_a2,",
                        data = X_back_track_train,
                        method = 'xgbTree',
                        trControl = ctrl,
                        tuneGrid = xgb_grid0,
                        alpha = alpha,
                        metric = 'logLoss',
                        na.action = na.omit)")))
  part <- xgb_a2a$results
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
eval(parse(text=paste("xgb_a2 <- train(",model_a2,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_a2
plot(xgb_va)

set.seed(303493)
eval(parse(text=paste("rf_a2 <- train(",model_a2,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_a2
plot(rf_a2)

# AFD - survey_demo + track_news_media

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_a3a <- train(",model_a3,",
                        data = X_back_track_train,
                        method = 'xgbTree',
                        trControl = ctrl,
                        tuneGrid = xgb_grid0,
                        alpha = alpha,
                        metric = 'logLoss',
                        na.action = na.omit)")))
  part <- xgb_a3a$results
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
eval(parse(text=paste("xgb_a3 <- train(",model_a3,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_a3
plot(xgb_a3)

set.seed(303493)
eval(parse(text=paste("rf_a3 <- train(",model_a3,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_a3
plot(rf_a3)

# AFD - survey_demo + track_apps_domains

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_a4a <- train(",model_a4,",
                        data = X_back_track_train,
                        method = 'xgbTree',
                        trControl = ctrl,
                        tuneGrid = xgb_grid0,
                        alpha = alpha,
                        metric = 'logLoss',
                        na.action = na.omit)")))
  part <- xgb_a4a$results
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
eval(parse(text=paste("xgb_a4 <- train(",model_a4,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_a4
plot(xgb_a4)

set.seed(303493)
eval(parse(text=paste("rf_a4 <- train(",model_a4,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_a4
plot(rf_a4)

# AFD - only tracking data

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_a5a <- train(",model_a5,",
                        data = X_back_track_train,
                        method = 'xgbTree',
                        trControl = ctrl,
                        tuneGrid = xgb_grid0,
                        alpha = alpha,
                        metric = 'logLoss',
                        na.action = na.omit)")))
  part <- xgb_a5a$results
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
eval(parse(text=paste("xgb_a5 <- train(",model_a5,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_a5
plot(xgb_a5)

set.seed(303493)
eval(parse(text=paste("rf_a5 <- train(",model_a5,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_a5
plot(rf_a5)

# AFD - tracking data + survey_demo

results <- data.frame()
for (i in seq_along(tune_alpha)) {
  alpha <- tune_alpha[i]
  set.seed(303493)
  eval(parse(text=paste("xgb_a6a <- train(",model_a6,",
                        data = X_back_track_train,
                        method = 'xgbTree',
                        trControl = ctrl,
                        tuneGrid = xgb_grid0,
                        alpha = alpha,
                        metric = 'logLoss',
                        na.action = na.omit)")))
  part <- xgb_a6a$results
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
eval(parse(text=paste("xgb_a6 <- train(",model_a6,",
                      data = X_back_track_train,
                      method = 'xgbTree',
                      trControl = ctrl,
                      tuneGrid = xgb_grid,
                      alpha = best_alpha,
                      metric = 'logLoss',
                      na.action = na.omit)")))

xgb_a6
plot(xgb_a6)

set.seed(303493)
eval(parse(text=paste("rf_a6 <- train(",model_a6,",
                      data = X_back_track_train,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = 'logLoss',
                      importance = 'impurity',
                      na.action = na.omit)")))

rf_a6
plot(rf_a6)

##################################################################################
# Variable Importance
##################################################################################

plot(varImp(xgb_a6), top = 10)

imp_xgb_a6 <- varImp(xgb_a6)$importance
imp_xgb_a6 <- rownames_to_column(imp_xgb_a6, "varname")

imp_xgb_a6 <-
  imp_xgb_a6 %>%
  top_n(10, Overall) %>%
  mutate(order = 11 - row_number())

match(imp_xgb_a6$varname, names(X_back_track_train))
# imp_xgb_a6$varname <- c("Fake rel. d", "High school degree", "deref-web-02.de", "ebay.de", "Age", "hclips.com", "dropbox.com", "ikea.com", "eventim.de", "Gender")

ggplot(imp_xgb_a6) +
  geom_point(aes(x = Overall, y = order)) + 
  geom_segment(aes(y = order, yend = order, x = 1, xend = Overall)) +
  labs(x = "Importance", y = "") +
  xlim(0, 100) +
  scale_y_continuous(
    breaks = imp_xgb_a6$order,
    labels = imp_xgb_a6$varname)
ggsave("p_imp_a.png", width = 6, height = 6)

##################################################################################
# PDPs
##################################################################################
 
pdp1 <- partial(xgb_a6, pred.var = "wpfakereld", type = "classification", which.class = 2, prob = T, rug = T,
                plot = T, plot.engine = "ggplot2", progress = "text")

pdp1
ggsave("p_pdp1_a.png", width = 9, height = 9)

pdp2 <- partial(xgb_a6, pred.var = "eduschool", type = "classification", which.class = 2, prob = T, rug = T,
                plot = T, plot.engine = "ggplot2", progress = "text")

pdp2
ggsave("p_pdp2_a.png", width = 9, height = 9)

pdp3 <- partial(xgb_a6, pred.var = "wpderefweb02de", type = "classification", which.class = 2, prob = T, rug = T,
                plot = T, plot.engine = "ggplot2", progress = "text")

pdp3
ggsave("p_pdp3_a.png", width = 9, height = 9)

pdp4 <- partial(xgb_a6, pred.var = "wpebayde", type = "classification", which.class = 2, prob = T, rug = T, 
                plot = T, plot.engine = "ggplot2", progress = "text")

pdp4
ggsave("p_pdp4_a.png", width = 9, height = 9)

pdp5 <- partial(xgb_a6, pred.var = "age", type = "classification", which.class = 2, prob = T, rug = T, 
                plot = T, plot.engine = "ggplot2", progress = "text")

pdp5
ggsave("p_pdp5_a.png", width = 9, height = 9)

pdp6 <- partial(xgb_a6, pred.var = "wphclipscom", type = "classification", which.class = 2, prob = T, rug = T, 
                plot = T, plot.engine = "ggplot2", progress = "text")

pdp6
ggsave("p_pdp6_a.png", width = 9, height = 9)

pdp7 <- partial(xgb_a6, pred.var = "wpdropboxcom", type = "classification", which.class = 2, prob = T, rug = T, 
                plot = T, plot.engine = "ggplot2", progress = "text")

pdp7
ggsave("p_pdp7_a.png", width = 9, height = 9)

pdp8 <- partial(xgb_a6, pred.var = "wpikeacom", type = "classification", which.class = 2, prob = T, rug = T, 
                plot = T, plot.engine = "ggplot2", progress = "text")

pdp8
ggsave("p_pdp8_a.png", width = 9, height = 9)

pdp9 <- partial(xgb_a6, pred.var = "wpeventimde", type = "classification", which.class = 2, prob = T, rug = T, 
                plot = T, plot.engine = "ggplot2", progress = "text")

pdp9
ggsave("p_pdp9_a.png", width = 9, height = 9)

pdp10 <- partial(xgb_a6, pred.var = "gender", type = "classification", which.class = 2, prob = T, rug = T, 
                plot = T, plot.engine = "ggplot2", progress = "text")

pdp10
ggsave("p_pdp10_a.png", width = 9, height = 9)

##################################################################################
# Compare CV performance
##################################################################################

# CV plot - AFD

resamps <- resamples(list(xgb_a1, xgb_a2, xgb_a3, xgb_a4, xgb_a5, xgb_a6))
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
ggsave("p_resamp_a1.png", width = 7.5, height = 7)

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
ggsave("p_resamp_a2.png", width = 7.5, height = 7)

difresamps <- diff(resamps)
summary(difresamps)$table$ROC
summary(difresamps)$table$logLoss

##################################################################################
# Predict in test data
##################################################################################

p_xgb_a1 <- predict(xgb_a1, newdata = X_back_track_test, type = "prob")
p_xgb_a2 <- predict(xgb_a2, newdata = X_back_track_test, type = "prob")
p_xgb_a3 <- predict(xgb_a3, newdata = X_back_track_test, type = "prob")
p_xgb_a4 <- predict(xgb_a4, newdata = X_back_track_test, type = "prob")
p_xgb_a5 <- predict(xgb_a5, newdata = X_back_track_test, type = "prob")
p_xgb_a6 <- predict(xgb_a6, newdata = X_back_track_test, type = "prob")

# ROC and LogLoss - AFD

roc_xgb_a1 <- roc(response = X_back_track_test$AFD, predictor = p_xgb_a1$AFD)
roc_xgb_a2 <- roc(response = X_back_track_test$AFD, predictor = p_xgb_a2$AFD)
roc_xgb_a3 <- roc(response = X_back_track_test$AFD, predictor = p_xgb_a3$AFD)
roc_xgb_a4 <- roc(response = X_back_track_test$AFD, predictor = p_xgb_a4$AFD)
roc_xgb_a5 <- roc(response = X_back_track_test$AFD, predictor = p_xgb_a5$AFD)
roc_xgb_a6 <- roc(response = X_back_track_test$AFD, predictor = p_xgb_a6$AFD)

ggroc(list("Demo" = roc_xgb_a1, "Tracking" = roc_xgb_a5, "Demo+Tracking" = roc_xgb_a6)) +
  geom_abline(aes(intercept = 1, slope = 1)) +
  scale_colour_manual(name = "", values = c("#F8766D", "#00BA38", "#619CFF"),
                      breaks = c("Demo", "Tracking", "Demo+Tracking")) +
  theme(text = element_text(size = 13))

ggsave("p_roc_a.png", width = 7.5, height = 6)

p_xgb_a1$obs <- X_back_track_test$AFD
p_xgb_a1$pred <- predict(xgb_a1, newdata = X_back_track_test)
p_xgb_a2$obs <- X_back_track_test$AFD
p_xgb_a2$pred <- predict(xgb_a2, newdata = X_back_track_test)
p_xgb_a3$obs <- X_back_track_test$AFD
p_xgb_a3$pred <- predict(xgb_a3, newdata = X_back_track_test)
p_xgb_a4$obs <- X_back_track_test$AFD
p_xgb_a4$pred <- predict(xgb_a4, newdata = X_back_track_test)
p_xgb_a5$obs <- X_back_track_test$AFD
p_xgb_a5$pred <- predict(xgb_a5, newdata = X_back_track_test)
p_xgb_a6$obs <- X_back_track_test$AFD
p_xgb_a6$pred <- predict(xgb_a6, newdata = X_back_track_test)

perf_a1 <- cbind(twoClassSummary(drop_na(p_xgb_a1), lev = levels(p_xgb_a1$obs))[[1]],
                 mnLogLoss(p_xgb_a1, lev = levels(p_xgb_a1$obs))[[1]])
perf_a2 <- cbind(twoClassSummary(drop_na(p_xgb_a2), lev = levels(p_xgb_a2$obs))[[1]],
                 mnLogLoss(p_xgb_a2, lev = levels(p_xgb_a2$obs))[[1]])
perf_a3 <- cbind(twoClassSummary(drop_na(p_xgb_a3), lev = levels(p_xgb_a3$obs))[[1]],
                 mnLogLoss(p_xgb_a3, lev = levels(p_xgb_a3$obs))[[1]])
perf_a4 <- cbind(twoClassSummary(drop_na(p_xgb_a4), lev = levels(p_xgb_a4$obs))[[1]],
                 mnLogLoss(p_xgb_a4, lev = levels(p_xgb_a4$obs))[[1]])
perf_a5 <- cbind(twoClassSummary(drop_na(p_xgb_a5), lev = levels(p_xgb_a5$obs))[[1]],
                 mnLogLoss(p_xgb_a5, lev = levels(p_xgb_a5$obs))[[1]])
perf_a6 <- cbind(twoClassSummary(drop_na(p_xgb_a6), lev = levels(p_xgb_a6$obs))[[1]],
                 mnLogLoss(p_xgb_a6, lev = levels(p_xgb_a6$obs))[[1]])

tab <- rbind(perf_a1, perf_a2, perf_a3, perf_a4, perf_a5, perf_a6)
tab

rtffile <- RTF("perf_a.doc")
addTable(rtffile, round(tab, digits = 3))
done(rtffile)

# Performance at "optimal" threshold - AFD

prop.table(table(X_back_track_train$AFD))
roc_xgb_a1_t <- coords(roc_xgb_a1, x = "best", best.method = "closest.topleft", best.weights = c(1, 0.15))

c_xgb_a1 <- as.factor(ifelse(p_xgb_a1$AFD > roc_xgb_a1_t[[1]], "AFD", "not_AFD"))
c_xgb_a2 <- as.factor(ifelse(p_xgb_a2$AFD > roc_xgb_a1_t[[1]], "AFD", "not_AFD"))
c_xgb_a3 <- as.factor(ifelse(p_xgb_a3$AFD > roc_xgb_a1_t[[1]], "AFD", "not_AFD"))
c_xgb_a4 <- as.factor(ifelse(p_xgb_a4$AFD > roc_xgb_a1_t[[1]], "AFD", "not_AFD"))
c_xgb_a5 <- as.factor(ifelse(p_xgb_a5$AFD > roc_xgb_a1_t[[1]], "AFD", "not_AFD"))
c_xgb_a6 <- as.factor(ifelse(p_xgb_a6$AFD > roc_xgb_a1_t[[1]], "AFD", "not_AFD"))

cm1 <- confusionMatrix(c_xgb_a1, X_back_track_test$AFD, positive = "AFD", mode = "everything")
cm2 <- confusionMatrix(c_xgb_a2, X_back_track_test$AFD, positive = "AFD", mode = "everything")
cm3 <- confusionMatrix(c_xgb_a3, X_back_track_test$AFD, positive = "AFD", mode = "everything")
cm4 <- confusionMatrix(c_xgb_a4, X_back_track_test$AFD, positive = "AFD", mode = "everything")
cm5 <- confusionMatrix(c_xgb_a5, X_back_track_test$AFD, positive = "AFD", mode = "everything")
cm6 <- confusionMatrix(c_xgb_a6, X_back_track_test$AFD, positive = "AFD", mode = "everything")

Demo <- c(cm1$overall[1], cm1$byClass[c(1:2,5,7)], cm1$overall[2])
Demo_Tracking_general <- c(cm2$overall[1], cm2$byClass[c(1:2,5,7)], cm2$overall[2])
Demo_Tracking_news <- c(cm3$overall[1], cm3$byClass[c(1:2,5,7)], cm3$overall[2])
Demo_Tracking_apps <- c(cm4$overall[1], cm4$byClass[c(1:2,5,7)], cm4$overall[2])
Tracking <- c(cm5$overall[1], cm5$byClass[c(1:2,5,7)], cm5$overall[2])
Demo_Tracking <- c(cm6$overall[1], cm6$byClass[c(1:2,5,7)], cm6$overall[2])

tab <- rbind(Demo, Tracking, Demo_Tracking_general, Demo_Tracking_news, Demo_Tracking_apps, Demo_Tracking)
tab

rtffile <- RTF("t_perf_a.doc")
addTable(rtffile, cbind(rownames(tab),round(tab, digits = 3)))
done(rtffile)

##################################################################################
# Predict in test data by subgroups
##################################################################################

# add party affiliation
Yadd3 <- select(survey_w3, party_affiliation, panelist_id)
Yadd3$party_affiliation <- as.factor(Yadd3$party_affiliation)
levels(Yadd3$party_affiliation) <- c("CDU","SPD","GREEN","FDP","LEFT","AFD","Other")

X_back_track_test <- left_join(X_back_track_test, Yadd3, by = c("panelistid" = "panelist_id"))

# add undecided from wave 2
setwd("Y:\\Respondi\\RESPONDI_w2\\")
survey_w2 <- read_dta(file = "./survey_daten/survey_data_w2.dta")
Yadd2 <- select(survey_w2, undecided, panelist_id)
Yadd2$undecided <- as.factor(Yadd2$undecided)
levels(Yadd2$undecided) <- c("decided", "undecided")

X_back_track_test <- left_join(X_back_track_test, Yadd2, by = c("panelistid" = "panelist_id"))

# AFD by undecided
confusionMatrix(c_xgb_a1[X_back_track_test$undecided == "decided"], X_back_track_test$AFD[X_back_track_test$undecided == "decided"], positive = "AFD", mode = "everything")
confusionMatrix(c_xgb_a1[X_back_track_test$undecided == "undecided"], X_back_track_test$AFD[X_back_track_test$undecided == "undecided"], positive = "AFD", mode = "everything")
confusionMatrix(c_xgb_a6[X_back_track_test$undecided == "decided"], X_back_track_test$AFD[X_back_track_test$undecided == "decided"], positive = "AFD", mode = "everything")
confusionMatrix(c_xgb_a6[X_back_track_test$undecided == "undecided"], X_back_track_test$AFD[X_back_track_test$undecided == "undecided"], positive = "AFD", mode = "everything")

# AFD by voting
addmargins(prop.table(table(c_xgb_a1, X_back_track_test$voted), 2))
addmargins(prop.table(table(c_xgb_a5, X_back_track_test$voted), 2))
addmargins(prop.table(table(c_xgb_a6, X_back_track_test$voted), 2))

# party affiliation by AFD
tab1 <- rbind(table(c_xgb_a1, X_back_track_test$party_affiliation), prop.table(table(c_xgb_a1, X_back_track_test$party_affiliation), 1))
tab2 <- rbind(table(c_xgb_a5, X_back_track_test$party_affiliation), prop.table(table(c_xgb_a5, X_back_track_test$party_affiliation), 1))
tab3 <- rbind(table(c_xgb_a6, X_back_track_test$party_affiliation), prop.table(table(c_xgb_a6, X_back_track_test$party_affiliation), 1))

tab <- rbind(tab1, tab2, tab3)
tab

rownames(tab) <- c()
rtffile <- RTF("parties_a.doc")
addTable(rtffile, round(tab, digits = 3))
done(rtffile)
