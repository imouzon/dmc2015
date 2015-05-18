# ============================================================================
# File Name: 09_conditionalRF.R
#
# Author: Evan P. Walsh
# Contact: epwalsh@iastate.edu
#
# Creation Date: 14-05-2015
# Last Modified: Sun May 17 18:15:12 2015
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

# Note: We still need to agree on a common loss function to use to compare 
# models.

library(party)
source("~/GitHub/dmc2015/pete/10_roc.R")
imp <- readRDS("~/GitHub/dmc2015/pete/predictions/importance.rds")

# Classification
# ============================================================================

ntrees = 300
nvars = 150

# Historical set 1
# ----------------------------------------------------------------------------
# Sort variables by importance in set 1
imp <- imp[order(imp$h1_imp, decreasing = T),]

h1 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.3.rds")
h1_t <- cbind(couponUsed = h1$train$y$couponUsed, 
              h1$train$X[names(h1$train$X) %in% as.character(imp$var[1:nvars])])
h1_cf <- cforest(couponUsed~., data = h1_t,
                 control = cforest_unbiased(mtry = 10, ntree = ntrees))
# Validation set 
h1_v <- cbind(couponUsed = h1$validation$y$couponUsed, 
              h1$validation$X[as.character(imp$var[1:nvars])])
h1_v_p <- predict(h1_cf, newdata = h1_v)
# Validation error
coupon_mean <- mean(h1_v$couponUsed)
h1_cf_error = sum(((h1_v$couponUsed - h1_v_p) / coupon_mean)^2)
h1_cf_mean_err = h1_cf_error / length(h1_v_p)
# Classification set predictions
h1_c <- cbind(couponUsed = h1$class$y$couponUsed,
              h1$class$X[as.character(imp$var[1:nvars])])
h1_c_p <- predict(h1_cf, newdata = h1_c)
h1_c_p <- cbind(orderID = h1$class$y$orderID, couponUsed = h1_c_p)
# Save model and predictions
h1_mod <- list(predictions = h1_c_p, 
               error = list(error = h1_cf_error, mean_error = h1_cf_mean_err))
saveRDS(h1_mod, "~/GitHub/dmc2015/predictions/cforest_H1_0.3_coup.rds")

# Plot ROC
jpeg("~/GitHub/dmc2015/pete/figures/cforest_h1_0.3.jpg", width = 480, height = 480)
roc(h1_v_p, h1_v$couponUsed)
dev.off()


# Historical set 2
# ----------------------------------------------------------------------------
imp <- imp[order(imp$h2_imp, decreasing = T),]

h2 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset2_LONG_ver0.3.rds")
h2_t <- cbind(couponUsed = h2$train$y$couponUsed, 
              h2$train$X[as.character(imp$var[1:nvars])])
h2_cf <- cforest(couponUsed~., data = h2_t,
                 control = cforest_unbiased(mtry = 10, ntree = ntrees))
# Validation set 
h2_v <- cbind(couponUsed = h2$validation$y$couponUsed, 
              h2$validation$X[as.character(imp$var[1:nvars])])
h2_v_p <- predict(h2_cf, newdata = h2_v)
# Validation error
coupon_mean <- mean(h2_v$couponUsed)
h2_cf_error = sum(((h2_v$couponUsed - h2_v_p) / coupon_mean)^2)
h2_cf_mean_err = h2_cf_error / length(h2_v_p)
# Classification set predictions
h2_c <- cbind(couponUsed = h2$class$y$couponUsed,
              h2$class$X[as.character(imp$var[1:nvars])])
h2_c_p <- predict(h2_cf, newdata = h2_c)
h2_c_p <- cbind(orderID = h2$class$y$orderID, couponUsed = h2_c_p)
# Save model and predictions
h2_mod <- list(predictions = h2_c_p, 
               error = list(error = h2_cf_error, mean_error = h2_cf_mean_err))
saveRDS(h2_mod, "~/GitHub/dmc2015/predictions/cforest_H2_0.3_coup.rds")

jpeg("~/GitHub/dmc2015/pete/figures/cforest_h2_0.3.jpg", width = 480, height = 480)
roc(h2_v_p, h2_v$couponUsed)
dev.off()

# Historical set 3
# ----------------------------------------------------------------------------
imp <- imp[order(imp$h2_imp, decreasing = T),]

h3 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset3_LONG_ver0.3.rds")
h3_t <- cbind(couponUsed = h3$train$y$couponUsed, 
              h3$train$X[as.character(imp$var[1:nvars])])
h3_cf <- cforest(couponUsed~., data = h3_t,
                 control = cforest_unbiased(mtry = 10, ntree = ntrees))
# Validation set
h3_v <- cbind(couponUsed = h3$validation$y$couponUsed, 
              h3$validation$X[as.character(imp$var[1:nvars])])
h3_v_p <- predict(h3_cf, newdata = h3_v)
# Valdation error
coupon_mean <- mean(h3_v$couponUsed)
h3_cf_error = sum(((h3_v$couponUsed - h3_v_p) / coupon_mean)^2)
h3_cf_mean_err = h3_cf_error / length(h3_v_p)
# Classification set predictions
h3_c <- cbind(couponUsed = h3$class$y$couponUsed,
              h3$class$X[as.character(imp$var[1:nvars])])
h3_c_p <- predict(h3_cf, newdata = h3_c)
h3_c_p <- cbind(orderID = h3$class$y$orderID, couponUsed = h3_c_p)
# Save model and predictions
h3_mod <- list(predictions = h3_c_p, 
               error = list(error = h3_cf_error, mean_error = h3_cf_mean_err))
saveRDS(h3_mod, "~/GitHub/dmc2015/predictions/cforest_H3_0.3_coup.rds")

# Save ROC curve
jpeg("~/GitHub/dmc2015/pete/figures/cforest_h3_0.3.jpg", width = 480, height = 480)
roc(h3_v_p, h3_v$couponUsed)
dev.off()

# Compare ROC curves of each set 
# ============================================================================
set1 <- roc_dat(h1_v_p, h1_v$couponUsed)
set1$set <- "HTV1"
set2 <- roc_dat(h2_v_p, h2_v$couponUsed)
set2$set <- "HTV2"
set3 <- roc_dat(h3_v_p, h3_v$couponUsed)
set3$set <- "HTV3"
jpeg("~/GitHub/dmc2015/pete/figures/cforest_all_0.3.jpg", width = 480, height = 480) 
rbind(set1, set2, set3) %>% ggplot(aes(x = fpr, y = tpr)) + 
  geom_step(aes(colour = set)) + theme_bw() + 
  xlab("False Positive Rate") + ylab("True Positive Rate")
dev.off()


# Regression
# ============================================================================
imp <- readRDS("~/GitHub/dmc2015/pete/predictions/importance_BV.rds")

ntrees = 300
nvars = 150

# Historical set 1
# ----------------------------------------------------------------------------
imp <- imp[order(imp$h1_imp, decreasing = T),]

h1 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_WIDE_ver0.3.rds")
h1_t <- cbind(basketValue = h1$train$y$basketValue, 
              h1$train$X[as.character(imp$var[1:nvars])])
h1_cf <- cforest(basketValue~., data = h1_t,
                 control = cforest_unbiased(mtry = 10, ntree = ntrees))
# Validation set
h1_v <- cbind(basketValue = h1$validation$y$basketValue, 
              h1$validation$X[as.character(imp$var[1:nvars])])
h1_v_p <- predict(h1_cf, newdata = h1_v)
basketValue_mean <- mean(h1_v$basketValue)
# Validation set error
h1_cf_error = sum(((h1_v$basketValue - h1_v_p) / basketValue_mean)^2)
h1_cf_mean_err = h1_cf_error / length(h1_v_p)
# Classification set predictions
h1_c <- cbind(basketValue = h1$class$y$basketValue,
              h1$class$X[as.character(imp$var[1:nvars])])
h1_c_p <- predict(h1_cf, newdata = h1_c)
h1_c_p <- cbind(orderID = h1$class$y$orderID, basketValue = h1_c_p)
# Save model and predictions
h1_mod <- list(predictions = h1_c_p, 
               error = list(error = h1_cf_error, mean_error = h1_cf_mean_err))
saveRDS(h1_mod, "~/GitHub/dmc2015/predictions/cforest_H1_0.3_BV.rds")

