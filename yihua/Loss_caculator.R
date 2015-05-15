Loss_calculator <- function(coupon1pred, coupon1true,
                 coupon2pred, coupon2true,
                 coupon3pred, coupon3true, 
                 basketpred=NULL, baskettrue=NULL) {
  loss1 <- sum((coupon1pred-coupon1true)^2)/(mean(coupon1true))^2
  loss2 <- sum((coupon2pred-coupon2true)^2)/(mean(coupon2true))^2
  loss3 <- sum((coupon3pred-coupon3true)^2)/(mean(coupon3true))^2
  loss4 <- 0
  if (!is.null(basketpred)) {
    loss4 <- sum((basketpred-baskettrue)^2)/(mean(baskettrue))^2
  }
  return(c(loss1,loss2,loss3,loss4))
}