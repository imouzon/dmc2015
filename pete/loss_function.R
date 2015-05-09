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


