# ============================================================================
# File Name: 13_final.R
#
# Author: Evan P. Walsh
# Contact: epwalsh@iastate.edu
#
# Creation Date: 19-05-2015
# Last Modified: Tue May 19 06:46:25 2015
#
# Purpose:
#
# ============================================================================

crf <- readRDS("~/GitHub/dmc2015/predictions/set1/crf_crf_set1_0.3.rds")
lasso <- readRDS("~/GitHub/dmc2015/predictions/set1/lasso_lasso_set1.rds")

set1_mix <- 0.5*crf$class_predictions[,2] + 0.5*lasso$class$lasso_lasso.pred.TV
set1_mix <- data.frame(orderID = lasso$class$orderID, 
                       couponCol = rep(1:3, length(set1_mix) / 3),
                       couponUsed = set1_mix)

set2_mix <- 0.5*crf$class_predictions[,2] + 0.5*lasso$class$lasso_lasso.pred.T
set2_mix <- data.frame(orderID = lasso$class$orderID, 
                       couponCol = rep(1:3, length(set2_mix) / 3),
                       couponUsed = set2_mix)

out <- matrix(set1_mix$couponUsed, ncol = 3, byrow = T)
colnames(out) <- c("coupon1Used", "coupon2Used", "coupon3Used")
out <- data.frame(out)
out <- cbind(orderID = 6054:6722, out)

write.csv(out, "~/GitHub/dmc2015/pete/predictions/final.csv")
saveRDS(set1_mix, "~/GitHub/dmc2015/pete/predictions/final.rds")
