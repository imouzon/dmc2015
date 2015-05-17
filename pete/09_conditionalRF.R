# ============================================================================
# File Name: 09_conditionalRF.R
#
# Author: Evan P. Walsh
# Contact: epwalsh@iastate.edu
#
# Creation Date: 14-05-2015
# Last Modified: Sat May 16 21:22:11 2015
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
imp <- readRDS("~/GitHub/dmc2015/pete/predictions/importance.rds")

# Historical set 1
# ----------------------------------------------------------------------------
h1 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.3.rds")
h1_t <- cbind(couponUsed = h1$train$y$couponUsed, 
              h1$train$X[names(h1$train$X) %in% as.character(imp$var[1:100])])
h1_cf <- cforest(couponUsed~., data = h1_t,
                control = cforest_unbiased(mtry = 10, ntree = 200))
h1_v <- cbind(couponUsed = h1$validation$y$couponUsed, 
              h1$validation$X[as.character(imp$var[1:100])])
h1_v_p <- predict(h1_cf, newdata = h1_v)
jpeg("~/GitHub/dmc2015/pete/figures/cforest_h1_0.3.jpg")
roc(h1_v_p, h1_v$couponUsed)
dev.off()

saveRDS(h1_cf, "~/GitHub/dmc2015/predictions/cforest_H1_0.3.rds")

# Historical set 2
# ----------------------------------------------------------------------------
h2 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset2_LONG_ver0.3.rds")
h2_t <- cbind(couponUsed = h2$train$y$couponUsed, 
              h2$train$X[names(h2$train$X) %in% as.character(imp$var[1:100])])
h2_cf <- cforest(couponUsed~., data = h2_t,
                control = cforest_unbiased(mtry = 10, ntree = 200))
h2_v <- cbind(couponUsed = h2$validation$y$couponUsed, 
              h2$validation$X[names(h2$train$X) %in% as.character(imp$var[1:100])])
h2_v_p <- predict(h2_cf, newdata = h2_v)
jpeg("~/GitHub/dmc2015/pete/figures/cforest_h2_0.3.jpg")
roc(h2_v_p, h2_v$couponUsed)
dev.off()

# Historical set 3
# ----------------------------------------------------------------------------
h3 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset3_LONG_ver0.3.rds")
h3_t <- cbind(couponUsed = h3$train$y$couponUsed, 
              h3$train$X[names(h3$train$X) %in% as.character(imp$var[1:100])])
h3_cf <- cforest(couponUsed~., data = h3_t,
                control = cforest_unbiased(mtry = 10, ntree = 200))
h3_v <- cbind(couponUsed = h3$validation$y$couponUsed, 
              h3$validation$X[names(h3$train$X) %in% as.character(imp$var[1:100])])
h3_v_p <- predict(h3_cf, newdata = h3_v)
jpeg("~/GitHub/dmc2015/pete/figures/cforest_h3_0.3.jpg")
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
jpeg("~/GitHub/dmc2015/pete/figures/cforest_all_0.3.jpg")
rbind(set1, set2, set3) %>% ggplot(aes(x = fpr, y = tpr)) + 
  geom_step(aes(colour = set)) + theme_bw()
dev.off()
