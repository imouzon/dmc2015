# ============================================================================
# Author: Evan P. Walsh
# Contact: epwalsh@iastate.edu
# Date: 08-05-2015
#
# Purpose: A function to calculate the loss function according to the DMC's
# specifications.
#
# ============================================================================

compute_loss <- function(predictions, actual) {
  coupon1_mean = mean(actual$coupon1Used)
  coupon1_error = sum((abs(actual$coupon1Used - predictions$coupon1Used) / 
                       coupon1_mean)^2)

  coupon2_mean = mean(actual$coupon2Used)
  coupon2_error = sum((abs(actual$coupon2Used - predictions$coupon2Used) / 
                       coupon2_mean)^2)

  coupon3_mean = mean(actual$coupon3Used)
  coupon3_error = sum((abs(actual$coupon3Used - predictions$coupon3Used) / 
                       coupon3_mean)^2)

  basketValue_mean = mean(basketValue)
  basketValue_error = sum((abs(actual$basketValue - predictions$basketValue) / 
                           basketValue_mean)^2)

  error = coupon1_error + coupon2_error + coupon3_error + basketValue_error
  res <- c(coupon1_error, coupon2_error, coupon3_error, 
           basketValue_error, error)
  names(res) <- c("coupon1_error", "coupon2_error", "coupon3_error", 
                  "basketValue_error", "total error")
  return(res)
}

lossFun = function(couponTrue, couponPredict) {
    n = length(couponTrue)
    idx1 = seq(1,n, by=3)
    idx2 = seq(2,n, by=3)
    idx3 = seq(3,n, by=3)
    coupon1pred = couponPredict[idx1]
    coupon2pred = couponPredict[idx2]
    coupon3pred = couponPredict[idx3]
    coupon1true = couponTrue[idx1]
    coupon2true = couponTrue[idx2]
    coupon3true = couponTrue[idx3]
    loss1 <- sum((coupon1pred-coupon1true)^2)/(mean(coupon1true))^2
    loss2 <- sum((coupon2pred-coupon2true)^2)/(mean(coupon2true))^2
    loss3 <- sum((coupon3pred-coupon3true)^2)/(mean(coupon3true))^2
    return(c(loss1, loss2, loss3, sum(loss1+loss2+loss3)))
}
