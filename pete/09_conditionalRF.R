# ============================================================================
# File Name: 09_conditionalRF.R
#
# Author: Evan P. Walsh
# Contact: epwalsh@iastate.edu
#
# Creation Date: 14-05-2015
# Last Modified: Tue May 19 04:40:21 2015
#
# Purpose: Create predictions using conditional random forests for 
# individual coupon predictions, basket value, and basket value using coupon 
# predictions. Conditional RF's are implemented through the "party" package.
#
# Output an RDS with the model saved, predictions sorted by order ID, and 
# validation error. Save this RDS in predictions folder with a name like
# 'CRF_FMatVers4.0_couponOnly.rds'
#
# ============================================================================


library(party)
source("~/GitHub/dmc2015/pete/10_roc.R")
source("~/GitHub/dmc2015/pete/loss_function.R")


# Classification
# ============================================================================

ntrees = 1000
nvars = 150

# Historical set 1
# ----------------------------------------------------------------------------
# Sort variables by importance in set 1
# imp <- imp[order(imp$h1_imp, decreasing = T),]
imp <- readRDS("~/GitHub/dmc2015/pete/predictions/importance_H1_0.6.rds")

# Grep out naive vars
imp <- imp[-grep("naive", imp$var),]
# Grep out est vars 
imp <- imp[-grep("est", imp$var),]

h1 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.3.rds")
h1_t <- cbind(couponUsed = h1$train$y$couponUsed, 
              h1$train$X[names(h1$train$X) %in% as.character(imp$var[1:nvars])])
h1_cf <- cforest(couponUsed~., data = h1_t,
                 control = cforest_unbiased(mtry = 10, ntree = ntrees))
# Validation set 
h1_v <- cbind(couponUsed = h1$validation$y$couponUsed, 
              h1$validation$X[names(h1$validation$X) %in% as.character(imp$var[1:nvars])])
h1_v_p <- predict(h1_cf, newdata = h1_v)
# Validation error
lossFun(h1_v$couponUsed, h1_v_p)
error = lossFun(h1_v$couponUsed, h1_v_p)
# Classification set predictions
h1_c <- cbind(couponUsed = h1$class$y$couponUsed,
              h1$class$X[names(h1$class$X) %in% as.character(imp$var[1:nvars])])
h1_c_p <- predict(h1_cf, newdata = h1_c)
h1_c_p <- cbind(orderID = h1$class$y$orderID, couponUsed = h1_c_p)
# Importance 
# h1_imp <- varimp(h1_cf)
# h1_imp <- data.frame(var = names(h1_imp), imp = as.numeric(h1_imp))
# h1_imp <- h1_imp[order(h1_imp$imp, decreasing = T),]
# h1_imp$imp <- h1_imp$imp * 10000
# rownames(h1_imp) <- 1:nrow(h1_imp)
# Save model and predictions
h1_mod <- list(val_predictions = h1_v_p,
               class_predictions = h1_c_p,
               error = error,
               details = list(nvars = nvars,
                              ntrees = ntrees,
                              mtry = 10))

saveRDS(h1_mod, "~/GitHub/dmc2015/predictions/set1/crf_crf_set1_0.8.rds")

# Plot ROC
# jpeg("~/GitHub/dmc2015/pete/figures/cforest_h1_0.3.jpg", width = 480, height = 480)
# roc(h1_v_p, h1_v$couponUsed)
# dev.off()


# Historical set 3
# ----------------------------------------------------------------------------
# imp <- imp[order(imp$h3_imp, decreasing = T),]
imp <- readRDS("~/GitHub/dmc2015/pete/predictions/importance_H3_0.5.rds")

h3 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset3_LONG_ver0.5.rds")
h3_t <- cbind(couponUsed = h3$train$y$couponUsed, 
              h3$train$X[as.character(imp$var[1:nvars])])
h3_cf <- cforest(couponUsed~., data = h3_t,
                 control = cforest_unbiased(mtry = 10, ntree = ntrees))
# Validation set
h3_v <- cbind(couponUsed = h3$validation$y$couponUsed, 
              h3$validation$X[as.character(imp$var[1:nvars])])
h3_v_p <- predict(h3_cf, newdata = h3_v)
# Validation error
lossFun(h3_v$couponUsed, h3_v_p)
error = lossFun(h3_v$couponUsed, h3_v_p)
# Classification set predictions
h3_c <- cbind(couponUsed = h3$class$y$couponUsed,
              h3$class$X[as.character(imp$var[1:nvars])])
h3_c_p <- predict(h3_cf, newdata = h3_c)
h3_c_p <- cbind(orderID = h3$class$y$orderID, couponUsed = h3_c_p)
# Importance 
# h3_imp <- varimp(h3_cf)
# h3_imp <- data.frame(var = names(h3_imp), imp = as.numeric(h3_imp))
# h3_imp <- h3_imp[order(h3_imp$imp, decreasing = T),]
# h3_imp$imp <- h3_imp$imp * 10000
# Save model and predictions
h3_mod <- list(val_predictions = h3_v_p,
              class_predictions = h3_c_p,
              error = error,
              details = list(nvars = nvars,
                             ntrees = ntrees,
                             mtry = 10))

saveRDS(h3_mod, "~/GitHub/dmc2015/predictions/cforest_H3_0.5_coup.rds")

# Save ROC curve
# jpeg("~/GitHub/dmc2015/pete/figures/cforest_h3_0.3.jpg", width = 480, height = 480)
# roc(h3_v_p, h3_v$couponUsed)
# dev.off()

# Compare ROC curves of each set 
# ============================================================================
set1 <- roc_dat(h1_v_p, h1_v$couponUsed)
set1$set <- "HTV1"
set3 <- roc_dat(h3_v_p, h3_v$couponUsed)
set3$set <- "HTV3"
jpeg("~/GitHub/dmc2015/pete/figures/cforest_all_0.3.jpg", width = 480, height = 480) 
rbind(set1, set3) %>% ggplot(aes(x = fpr, y = tpr)) + 
  geom_step(aes(colour = set)) + theme_bw() + 
  xlab("False Positive Rate") + ylab("True Positive Rate")
dev.off()

# Fit CRF on variables selected by other methods
# ----------------------------------------------------------------------------
imp <- readRDS("~/GitHub/dmc2015/penglh/imp_set1/imp_gbm_col_name.rds")
h1 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.8.rds")
h1_t <- cbind(couponUsed = h1$train$y$couponUsed, 
              h1$train$X[imp])
h1_cf <- cforest(couponUsed~., data = h1_t,
                 control = cforest_unbiased(mtry = 10, ntree = ntrees))
# Validation set 
h1_v <- cbind(couponUsed = h1$validation$y$couponUsed, 
              h1$validation$X[imp])
h1_v_p <- predict(h1_cf, newdata = h1_v)
# Validation error
error = lossFun(h1_v$couponUsed, h1_v_p)
# Classification set predictions
h1_c <- cbind(couponUsed = h1$class$y$couponUsed,
              h1$class$X[imp])
h1_c_p <- predict(h1_cf, newdata = h1_c)
h1_c_p <- cbind(orderID = h1$class$y$orderID, couponUsed = h1_c_p)
# Save model and predictions
h1_mod <- list(val_predictions = h1_v_p,
               class_predictions = h1_c_p,
               error = error,
               details = list(vars = "GBM selection",
                              nvars = 150,
                              ntrees = ntrees,
                              mtry = 10))
saveRDS(h1_mod, "~/GitHub/dmc2015/predictions/set1/crf_gbm_set1_0.8.rds")

# Regression
# ============================================================================
imp <- readRDS("~/GitHub/dmc2015/pete/predictions/importance_H1_0.5_BV.rds")
# imp <- imp[order(imp$h1_imp, decreasing = T),]

ntrees = 1000
nvars = 150

# Historical set 1
# ----------------------------------------------------------------------------

h1 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_WIDE_ver0.5.rds")
h1_t <- cbind(basketValue = h1$train$y$basketValue, 
              h1$train$X[as.character(imp$var[1:nvars])])
h1_cf <- cforest(basketValue~., data = h1_t,
                 control = cforest_unbiased(mtry = 50, ntree = ntrees))
# OOB
h1_cf_oob <- predict(h1_cf, OOB=T)
mean(h1_cf_oob)
# Validation set
h1_v <- cbind(basketValue = h1$validation$y$basketValue, 
              h1$validation$X[as.character(imp$var[1:nvars])])
h1_v_p <- predict(h1_cf, newdata = h1_v)
# Validation set error
basketValue_mean <- mean(h1_v$basketValue)
h1_cf_error = sum(((h1_v$basketValue - h1_v_p) / basketValue_mean)^2)
sum(((h1_v$basketValue - h1_v_p) / basketValue_mean)^2)
# Classification set predictions
h1_c <- cbind(basketValue = h1$class$y$basketValue,
              h1$class$X[as.character(imp$var[1:nvars])])
h1_c_p <- predict(h1_cf, newdata = h1_c)
h1_c_p <- cbind(orderID = h1$class$y$orderID, basketValue = h1_c_p)
# Save model and predictions
h1_mod <- list(val_predictions = h1_v_p,
               class_predictions = h1_c_p, 
               error = h1_cf_error)
saveRDS(h1_mod, "~/GitHub/dmc2015/predictions/cforest_H1_0.5_BV.rds")


