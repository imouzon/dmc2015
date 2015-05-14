addKey = function(trt,tst,key,prior_prob){
  n = length(key)
  key_paste = key[1]
  if(n >= 2){
    for(i in 2:n){
      key_paste = paste(key_paste,key[i])
    }
  }
  key_cols = which(names(trt)%in%key)
  if(!(key_paste%in%names(trt))){
    trt$key_paste = trt[,key_cols[1]]
    tst$key_paste = tst[,key_cols[1]]
    if(n >= 2){
      for(i in 2:n){
        trt$key_paste = paste(trt$key_paste,trt[,key_cols[i]])
        tst$key_paste = paste(tst$key_paste,tst[,key_cols[i]])
      }
    }
  }else{
    trt$key_paste = trt[,key_cols[1]]
    tst$key_paste = tst[,key_cols[1]]
  }
  key_ID = unique(trt$key_paste)
  key_num = length(key_ID)
  key_summ = data.frame("key_paste"=key_ID,"key_freq"=rep(0,key_num)
                              ,"key_prob"=rep(0,key_num))
  for(i in 1:key_num){
    dat_temp = trt$couponUsed[trt$key_paste==key_ID[i]]
    key_summ[i,2] = length(dat_temp)
    key_summ[i,3] = (sum(dat_temp)+prior_prob)/(key_summ[i,2]+1)
  }
  
  trt = trt %>% left_join(key_summ)
  tst = tst %>% left_join(key_summ)
  tst$key_freq[is.na(tst$key_freq)] = 0
  tst$key_prob[is.na(tst$key_prob)] = prior_prob
  
  if(key_paste%in%names(trt)){
    trt = trt %>% select(-key_paste)
    tst = tst %>% select(-key_paste)
  }
  
  if(!(key_paste%in%names(trt))){
    key_paste1 = key[1]
    if(n >= 2){
      for(i in 2:n){
        key_paste1 = paste0(key_paste1,"_",key[i])
      }
    }
    names(trt)[names(trt)=="key_paste"] <- paste0(key_paste1,"_key")
    names(trt)[names(trt)=="key_freq"] <- paste0(key_paste1,"_freq")
    names(trt)[names(trt)=="key_prob"] <- paste0(key_paste1,"_prob")
    names(tst)[names(tst)=="key_paste"] <- paste0(key_paste1,"_key")
    names(tst)[names(tst)=="key_freq"] <- paste0(key_paste1,"_freq")
    names(tst)[names(tst)=="key_prob"] <- paste0(key_paste1,"_prob")
  }else{
    names(trt)[names(trt)=="key_freq"] <- paste0(key_paste,"_freq")
    names(trt)[names(trt)=="key_prob"] <- paste0(key_paste,"_prob")
    names(tst)[names(tst)=="key_freq"] <- paste0(key_paste,"_freq")
    names(tst)[names(tst)=="key_prob"] <- paste0(key_paste,"_prob")
  }
  trt = trt %>% arrange(orderID, couponCol)
  tst = tst %>% arrange(orderID, couponCol)
  return(list("trt"=trt,"tst"=tst))
}