# ============================================================================
# File Name: 09_conditionalRF.R
#
# Author: Evan P. Walsh
# Contact: epwalsh@iastate.edu
#
# Creation Date: 14-05-2015
# Last Modified: Fri May 15 00:08:26 2015
#
# Purpose: Create predictions using conditional random forests for 
# individual coupon predictions, basket value, and basket value using coupon 
# predictions. Conditional RF's are implemented through the "party" package.
#
# Output an RDS with the model saved, predictions sorted by order ID, and 
# validation error. Save this RDS in predicitons folder with a name like
# 'CRF_FMatVers4.0_couponOnly.rds'
#
# ============================================================================

install.packages("party")
library(party)

# Historical set 1
# ----------------------------------------------------------------------------
h1 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.rds")
h1_t <- cbind(couponUsed = as.factor(h1$train$y$couponUsed), h1$train$X)
samp <- sample(1:nrow(h1_t), 500)
# This may take a while...
h1ct <- ctree(couponUsed~., data = h1_t, subset = samp)

h1_v <- cbind(couponUsed = as.factor(h1$validation$y$couponUsed), h1$validation$X)
h1_v_p <- predict(h1ct, newdata = h1_v)

sum(h1_v_p == h1_v$couponUsed) / length(h1_v_p)
sum(h1_v$couponUsed == 0) / length(h1_v$couponUsed)
table(h1_v_p, h1_v$couponUsed)


h1_c <- cbind(couponUsed = as.factor(h1$class$y$couponUsed), h1$class$X)

saveRDS(h1ct, "~/GitHub/dmc2015/predictions/..")

# Historical set 2
# ----------------------------------------------------------------------------
h2 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset2_LONG_ver0.rds")



# Historical set 3
# ----------------------------------------------------------------------------

