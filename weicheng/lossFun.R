lossFun = function(couponTrue, couponPredict){
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
