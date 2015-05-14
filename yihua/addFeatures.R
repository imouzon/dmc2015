addFeatures <- function(trt,tst,prior_prob){
  # tansfer TimeBtwnRecOrder to discrete data
  trt$TimeBtwnRecOrder_disc <- as.numeric(as.character(cut(trt$TimeBtwnRecOrder,breaks=c(0,quantile(trt$TimeBtwnRecOrder,seq(0,0.95,by=0.05)),1000000),
                                                           labels=quantile(trt$TimeBtwnRecOrder,seq(0,1,by=0.05)))))
  tst$TimeBtwnRecOrder_disc <- as.numeric(as.character(cut(tst$TimeBtwnRecOrder,breaks=c(0,quantile(trt$TimeBtwnRecOrder,seq(0,0.95,by=0.05)),1000000),
                                                           labels=quantile(trt$TimeBtwnRecOrder,seq(0,1,by=0.05)))))
  
  # add basePrice/price
  trt$ratio_bp_p = trt$basePrice/trt$price
  tst$ratio_bp_p = tst$basePrice/trt$price
  trt$ratio_bp_p_round = round(trt$ratio_bp_p,1)
  tst$ratio_bp_p_round = round(tst$ratio_bp_p,1)
  
  # oneway interaction
  oneway = c("userID","couponID","couponsReceivedTime","couponsReceivedDoW",
             "orderTimeTime","orderTimeDoW","TimeBtwnRecOrder_disc",
             "price","basePrice","productGroup","categoryIDs","ratio_bp_p_round")
  temp = addKey(trt,tst,key=c("userID"),prior_prob)
  for(i in 2:length(oneway)){
    temp = addKey(temp$trt,temp$tst,key=oneway[i],prior_prob)
  }

  # twoway interaction
  key_temp3 <- c(oneway[-2],"brand","premiumProduct")
  reward_twoway = lapply(key_temp3,function(x) t(combn(c("reward",x),2)))
  for(i in 1:length(reward_twoway)){
    temp = addKey(temp$trt,temp$tst,key=reward_twoway[[i]],prior_prob)
  }
  
  keys_temp1 <- c(oneway[-1],"brand","premiumProduct")
  userID_twoway <- lapply(keys_temp1,function(x) t(combn(c("userID",x),2)))
  for(i in 1:length(userID_twoway)){
    temp = addKey(temp$trt,temp$tst,key=userID_twoway[[i]],prior_prob)
  }
  
  keys_temp2 <- oneway[3:7]
  couponID_twoway <- lapply(keys_temp2,function(x) t(combn(c("couponID",x),2)))
  for(i in 1:length(couponID_twoway)){
    temp = addKey(temp$trt,temp$tst,key=couponID_twoway[[i]],prior_prob)
  }
  
  keys_temp <- c(oneway[-c(1,2)],"brand","premiumProduct")
  twoway <- combn(keys_temp,2)
  for(i in 1:ncol(twoway)){
    temp = addKey(temp$trt,temp$tst,key=twoway[,i],prior_prob)
  }
  
  #   trt = temp$trt
  #   tst = temp$tst
  
  return(temp)
}

