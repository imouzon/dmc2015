# ============================================================================
# File Name: 09_conditionalRF.R
#
# Author: Evan P. Walsh
# Contact: epwalsh@iastate.edu
#
# Creation Date: 14-05-2015
# Last Modified: Fri May 15 11:46:26 2015
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


# Note: We still need to combine the historical features with universal 
# features.

install.packages("party")
library(party)
source("~/GitHub/dmc2015/pete/10_roc.R")

# Historical set 1
# ----------------------------------------------------------------------------
h1 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.rds")
h1_t <- cbind(couponUsed = h1$train$y$couponUsed, h1$train$X)

vars <- names(h1_t)
vars1 <- c("couponUsed", vars[grep("llr_[estnaive]+_.*brd.*", vars)][1:20])

h1ct <- cforest(couponUsed~., data = h1_t[vars1],
                control = cforest_unbiased(mtry = 3, ntree = 50))
imp1 <- varimp(h1ct)
imp1[order(imp1)]

h1_v <- cbind(couponUsed = h1$validation$y$couponUsed, h1$validation$X)
h1_v_p <- predict(h1ct, newdata = h1_v[vars1])

roc(h1_v_p, h1_v$couponUsed)
fpr <- d$fpr
tpr <- d$tpr



saveRDS(h1ct, "~/GitHub/dmc2015/predictions/..")

# Historical set 2
# ----------------------------------------------------------------------------
h2 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset2_LONG_ver0.rds")



# Historical set 3
# ----------------------------------------------------------------------------

