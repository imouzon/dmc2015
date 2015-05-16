addKey_UniqueUser = function(H_melt, T_melt, V_melt, C_melt, key){
  n = length(key)
  key_paste = key[1]
  if(n >= 2){
    for(i in 2:n){
      key_paste = paste(key_paste,key[i])
    }
  }
  key_cols = which(names(H_melt)%in%key)
  if(!(key_paste%in%names(H_melt))){
    H_melt$key_paste = H_melt[,key_cols[1]]
    T_melt$key_paste = T_melt[,key_cols[1]]
    V_melt$key_paste = V_melt[,key_cols[1]]    
    C_melt$key_paste = C_melt[,key_cols[1]]    
    if(n >= 2){
      for(i in 2:n){
        H_melt$key_paste = paste(H_melt$key_paste,H_melt[,key_cols[i]])
        T_melt$key_paste = paste(T_melt$key_paste,T_melt[,key_cols[i]])
        V_melt$key_paste = paste(V_melt$key_paste,V_melt[,key_cols[i]])
        C_melt$key_paste = paste(C_melt$key_paste,C_melt[,key_cols[i]])
      }
    }
  }else{
    H_melt$key_paste = H_melt[,key_cols[1]]
    T_melt$key_paste = T_melt[,key_cols[1]]
    V_melt$key_paste = V_melt[,key_cols[1]]
    C_melt$key_paste = C_melt[,key_cols[1]]
  }

  key_ID = unique(H_melt$key_paste)
  key_num = length(key_ID)
  
  H.u <- H_melt %>% group_by(key_paste, userID) %>% 
    summarize(nUser=n(), nUsed=sum(couponUsed))
  column1 <- H_melt %>% group_by(key_paste) %>% 
    summarize(key_nUser=length(unique(userID)))
  column2 <- H.u %>% group_by(key_paste) %>% 
    summarize(key_nUserUsed=sum(nUsed>=1), 
              key_nUserTwiceOrMore=sum(nUsed>=2))
  column2$key_TwiceOrMore <- as.numeric(column2$key_nUserTwiceOrMore>=1)
  
  key_summ <- merge(column1, column2, by='key_paste')
  key_summ$SimpleProb <- key_summ$key_nUserUsed/key_summ$key_nUser
  
  alpha <- mmedist(key_summ$SimpleProb, "beta")$estimate[1]
  beta <- mmedist(key_summ$SimpleProb, "beta")$estimate[2]
  if(beta==0) {alpha <- 0.2; beta <- 0.8}
  
  key_summ$key_prob <- (key_summ$key_nUserUsed+alpha) / (key_summ$key_nUser+alpha+beta)
  key_summ <- key_summ[,-c(4,6)]
  
  H_melt = H_melt %>% left_join(key_summ)
  T_melt = T_melt %>% left_join(key_summ)
  V_melt = V_melt %>% left_join(key_summ)
  C_melt = C_melt %>% left_join(key_summ)
  T_melt$key_nUser[is.na(T_melt$key_nUser)] = 0
  V_melt$key_nUser[is.na(V_melt$key_nUser)] = 0
  C_melt$key_nUser[is.na(C_melt$key_nUser)] = 0
  T_melt$key_nUserUsed[is.na(T_melt$key_nUserUsed)] = 0
  V_melt$key_nUserUsed[is.na(V_melt$key_nUserUsed)] = 0
  C_melt$key_nUserUsed[is.na(C_melt$key_nUserUsed)] = 0
  T_melt$key_TwiceOrMore[is.na(T_melt$key_TwiceOrMore)] = 0
  V_melt$key_TwiceOrMore[is.na(V_melt$key_TwiceOrMore)] = 0
  C_melt$key_TwiceOrMore[is.na(C_melt$key_TwiceOrMore)] = 0
  
  T_melt$key_prob[is.na(T_melt$key_prob)] = alpha/(alpha+beta)
  V_melt$key_prob[is.na(V_melt$key_prob)] = alpha/(alpha+beta)
  C_melt$key_prob[is.na(C_melt$key_prob)] = alpha/(alpha+beta)
  
  if(key_paste %in% names(H_melt)){
    H_melt = H_melt %>% select(-key_paste)
    T_melt = T_melt %>% select(-key_paste)
    V_melt = V_melt %>% select(-key_paste)
    C_melt = C_melt %>% select(-key_paste)
  }
  
  if(!(key_paste %in% names(H_melt))){
    key_paste1 = key[1]
    if(n >= 2){
      for(i in 2:n){
        key_paste1 = paste0(key_paste1,"_",key[i])
      }
    }
    names(H_melt)[names(H_melt)=="key_nUser"] <- paste0(key_paste1,"_nUser")
    names(H_melt)[names(H_melt)=="key_nUserUsed"] <- paste0(key_paste1,"_nUserUsed")
    names(H_melt)[names(H_melt)=="key_TwiceOrMore"] <- paste0(key_paste1,"_Twice")
    names(H_melt)[names(H_melt)=="key_prob"] <- paste0(key_paste1,"_prob")
    names(T_melt)[names(T_melt)=="key_nUser"] <- paste0(key_paste1,"_nUser")
    names(T_melt)[names(T_melt)=="key_nUserUsed"] <- paste0(key_paste1,"_nUserUsed")
    names(T_melt)[names(T_melt)=="key_TwiceOrMore"] <- paste0(key_paste1,"_Twice")
    names(T_melt)[names(T_melt)=="key_prob"] <- paste0(key_paste1,"_prob")
    names(V_melt)[names(V_melt)=="key_nUser"] <- paste0(key_paste1,"_nUser")
    names(V_melt)[names(V_melt)=="key_nUserUsed"] <- paste0(key_paste1,"_nUserUsed")
    names(V_melt)[names(V_melt)=="key_TwiceOrMore"] <- paste0(key_paste1,"_Twice")
    names(V_melt)[names(V_melt)=="key_prob"] <- paste0(key_paste1,"_prob")
    names(C_melt)[names(C_melt)=="key_nUser"] <- paste0(key_paste1,"_nUser")
    names(C_melt)[names(C_melt)=="key_nUserUsed"] <- paste0(key_paste1,"_nUserUsed")
    names(C_melt)[names(C_melt)=="key_TwiceOrMore"] <- paste0(key_paste1,"_Twice")
    names(C_melt)[names(C_melt)=="key_prob"] <- paste0(key_paste1,"_prob")
  }else{
    names(H_melt)[names(H_melt)=="key_nUser"] <- paste0(key_paste,"_nUser")
    names(H_melt)[names(H_melt)=="key_nUserUsed"] <- paste0(key_paste,"_nUserUsed")
    names(H_melt)[names(H_melt)=="key_TwiceOrMore"] <- paste0(key_paste,"_Twice")
    names(H_melt)[names(H_melt)=="key_prob"] <- paste0(key_paste,"_prob")
    names(T_melt)[names(T_melt)=="key_nUser"] <- paste0(key_paste,"_nUser")
    names(T_melt)[names(T_melt)=="key_nUserUsed"] <- paste0(key_paste,"_nUserUsed")
    names(T_melt)[names(T_melt)=="key_TwiceOrMore"] <- paste0(key_paste,"_Twice")
    names(T_melt)[names(T_melt)=="key_prob"] <- paste0(key_paste,"_prob")
    names(V_melt)[names(V_melt)=="key_nUser"] <- paste0(key_paste,"_nUser")
    names(V_melt)[names(V_melt)=="key_nUserUsed"] <- paste0(key_paste,"_nUserUsed")
    names(V_melt)[names(V_melt)=="key_TwiceOrMore"] <- paste0(key_paste,"_Twice")
    names(V_melt)[names(V_melt)=="key_prob"] <- paste0(key_paste,"_prob")
    names(C_melt)[names(C_melt)=="key_nUser"] <- paste0(key_paste,"_nUser")
    names(C_melt)[names(C_melt)=="key_nUserUsed"] <- paste0(key_paste,"_nUserUsed")
    names(C_melt)[names(C_melt)=="key_TwiceOrMore"] <- paste0(key_paste,"_Twice")
    names(C_melt)[names(C_melt)=="key_prob"] <- paste0(key_paste,"_prob")
  }
  H_melt = H_melt %>% arrange(orderID, couponCol)
  T_melt = T_melt %>% arrange(orderID, couponCol)
  V_melt = V_melt %>% arrange(orderID, couponCol)
  C_melt = C_melt %>% arrange(orderID, couponCol)
  
  return(list("H_melt"=H_melt, "T_melt"=T_melt, 
              "V_melt"=V_melt, "C_melt"=C_melt))
}