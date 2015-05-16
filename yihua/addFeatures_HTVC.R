addFeatures_HTVC <- function(H_melt, T_melt, V_melt, C_melt){
  # tansfer TimeBtwnRecOrder to discrete data
  H_melt$TimeBtwnRecOrder_disc <- as.numeric(as.character(cut(H_melt$TimeBtwnRecOrder,
                                                              breaks=c(0,quantile(H_melt$TimeBtwnRecOrder,seq(0,0.95,by=0.05)),1000000),
                                                              labels=quantile(H_melt$TimeBtwnRecOrder,seq(0,1,by=0.05)))))
  T_melt$TimeBtwnRecOrder_disc <- as.numeric(as.character(cut(T_melt$TimeBtwnRecOrder,
                                                           breaks=c(0,quantile(H_melt$TimeBtwnRecOrder,seq(0,0.95,by=0.05)),1000000),
                                                           labels=quantile(H_melt$TimeBtwnRecOrder,seq(0,1,by=0.05)))))
  V_melt$TimeBtwnRecOrder_disc <- as.numeric(as.character(cut(V_melt$TimeBtwnRecOrder,
                                                              breaks=c(0,quantile(H_melt$TimeBtwnRecOrder,seq(0,0.95,by=0.05)),1000000),
                                                              labels=quantile(H_melt$TimeBtwnRecOrder,seq(0,1,by=0.05)))))
  C_melt$TimeBtwnRecOrder_disc <- as.numeric(as.character(cut(C_melt$TimeBtwnRecOrder,
                                                              breaks=c(0,quantile(H_melt$TimeBtwnRecOrder,seq(0,0.95,by=0.05)),1000000),
                                                              labels=quantile(H_melt$TimeBtwnRecOrder,seq(0,1,by=0.05)))))
  
  # add basePrice/price
  H_melt$ratio_bp_p = H_melt$basePrice/H_melt$price
  T_melt$ratio_bp_p = T_melt$basePrice/T_melt$price
  V_melt$ratio_bp_p = V_melt$basePrice/V_melt$price
  C_melt$ratio_bp_p = C_melt$basePrice/C_melt$price
  H_melt$ratio_bp_p_round = round(H_melt$ratio_bp_p,1)
  T_melt$ratio_bp_p_round = round(T_melt$ratio_bp_p,1)
  V_melt$ratio_bp_p_round = round(V_melt$ratio_bp_p,1)
  C_melt$ratio_bp_p_round = round(C_melt$ratio_bp_p,1)
  
  # oneway interaction
  oneway = c("couponID", "couponsReceivedTime", "couponsReceivedDoW",
             "orderTimeTime", "orderTimeDoW", "TimeBtwnRecOrder_disc",
             "price", "basePrice", "productGroup", "categoryIDs", 
             "ratio_bp_p_round")
  temp = addKey_UniqueUser(H_melt, T_melt, V_melt, C_melt, key=oneway[1])
  for(i in 2:length(oneway)){
    temp = addKey_UniqueUser(temp$H_melt, temp$T_melt, temp$V_melt, temp$C_melt,
                             key=oneway[i])
  }
  
  # twoway interaction
  key_temp3 <- c(oneway[-1], "brand", "premiumProduct")
  reward_twoway = lapply(key_temp3, function(x) t(combn(c("reward",x),2)))
  for(i in 1:length(reward_twoway)){
    temp = addKey_UniqueUser(temp$H_melt, temp$T_melt, temp$V_melt, temp$C_melt,
                             key=reward_twoway[[i]])
  }
    
  keys_temp2 <- oneway[2:6]
  couponID_twoway <- lapply(keys_temp2, function(x) t(combn(c("couponID",x),2)))
  for(i in 1:length(couponID_twoway)){
    temp = addKey_UniqueUser(temp$H_melt,temp$T_melt, temp$V_melt, temp$C_melt, 
                             key=couponID_twoway[[i]])
  }
  
  keys_temp <- c(oneway[-1], "brand", "premiumProduct")
  twoway <- combn(keys_temp, 2)
  for(i in 1:ncol(twoway)){
    temp = addKey_UniqueUser(temp$H_melt, temp$T_melt, temp$V_melt, temp$C_melt, 
                             key=twoway[,i])
  }
  
  return(temp)
}