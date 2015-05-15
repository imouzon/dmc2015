# ============================================================================
# File Name: 09_conditionalRF.R
#
# Author: Evan P. Walsh
# Contact: epwalsh@iastate.edu
#
# Creation Date: 14-05-2015
# Last Modified: Thu May 14 23:27:55 2015
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
h1_t <- cbind(couponUsed = h1$train$y$couponUsed, h1$train$X)

# This may take a while...
h1ct <- ctree(couponUsed~., data = h1_t)

h1_v <- cbind(couponUsed = h1$validation$y$couponUsed, h1$validation$X)
h1_v_p <- predict(h1ct, newdata = h1_v)

h1_c <- cbind(couponUsed = h1$class$y$couponUsed, h1$class$X)

saveRDS(h1ct, "~/GitHub/dmc2015/predictions/..")

# Historical set 2
# ----------------------------------------------------------------------------
h2 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset2_LONG_ver0.rds")



# Historical set 3
# ----------------------------------------------------------------------------

