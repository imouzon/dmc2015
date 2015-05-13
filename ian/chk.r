#--------------------------------------**--------------------------------------#
#  File Name: chk.r
#  Purpose:
#
#  Creation Date: 12-05-2015
#  Last Modified: Tue May 12 15:32:51 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#

llr_multiway = function(dsn,hst,trn,cls,val,comparecols){
   #we need the data to be in long form: see 
   hst.ids = hst$orderID
   trn.ids = trn$orderID
   val.ids = val$orderID
   cls.ids = cls$orderID

   dsn.llr = dsn
   for(i in comparecols) dsn.llr[,i] = as.character(dsn.llr[,i])
   dsn.llr = dsn.llr[,c("orderID","couponCol","couponUsed",names(dsn)[comparecols])]

   dsn.llr$compID = sapply(1:nrow(dsn.llr),function(i) paste(dsn.llr[i,4:ncol(dsn.llr)],collapse="|"))

   #estimate priors
   prior.est.llr = dsn.llr %>% select(orderID,couponCol,couponUsed,compID) %>% 
      group_by(compID) %>%
      summarize(ntimes = n(), timesUsed = sum(couponUsed), timesNotUsed = ntimes - timesUsed, propUsed = timesUsed/ntimes) %>%
      filter(ntimes != 1) 

      M = mean(prior.est.llr$propUsed,na.rm=T)
      M2 = mean(prior.est.llr$propUsed^2,na.rm=T)

      alpha.est = M * (M - M2) / ( M2 - M*M ) 
      beta.est = ( 1 - M) * ( M - M2 ) / (M2 - M*M)

   #get posterior information
   post.est.llr = dsn.llr %>% select(orderID,couponCol,couponUsed,compID) %>% 
      filter(orderID %in% hst.ids) %>%
      group_by(compID) %>%
      mutate(ntimes = n(), 
                timesUsed = sum(couponUsed), 
                timesNotUsed = ntimes - timesUsed, 
                llr_est = log((timesUsed + alpha.est)/(timesNotUsed + beta.est)),
                llr_naive = log((timesUsed + 1)/(timesNotUsed + 1))) %>% 
      select(orderID,couponCol,ntimes,timesUsed,timesNotUsed,llr_est,llr_naive)

      M = with(post.est.llr %>% group_by(compID) %>% summarize(propest = mean(timesUsed/ntimes)),mean(propest))
      M2 = with(post.est.llr %>% group_by(compID) %>% summarize(propest = mean(timesUsed/ntimes)),mean(propest^2))

      alpha.hist = M * (M - M2) / ( M2 - M*M ) 
      beta.hist = ( 1 - M) * ( M - M2 ) / (M2 - M*M)


   dsn.res = dsn.llr %>% left_join(post.est.llr,by=c("orderID","couponCol")) %>% 
      select(orderID,couponCol,ntimes,timesUsed,timesNotUsed,llr_est,llr_naive)


   dsn.res$ntimes[is.na(dsn.res$ntimes)] = -1  
   dsn.res$timesUsed[is.na(dsn.res$timesUsed)] = -1 
   dsn.res$timesNotUsed[is.na(dsn.res$timesNotUsed)] = -1  

   dsn.res$llr_est[is.na(dsn.res$llr_est)] = alpha.est/beta.est
   dsn.res$llr_naive[is.na(dsn.res$llr_naive)] = 0

   abbrv = paste(names(dsn)[comparecols],collapse="X")
   abbrv = gsub("productGroup","prod",abbrv)
   abbrv = gsub("brand","brd",abbrv)
   abbrv = gsub("reward","rwd",abbrv)
   abbrv = gsub("couponseReceived","cpnrec",abbrv)
   abbrv = gsub("orderTime","ort",abbrv)
   abbrv = gsub("premiumProduct","lux",abbrv)
   abbrv = gsub("categoryIDs","catids",abbrv)
   abbrv = gsub("categoryIDs1","catid1",abbrv)
   abbrv = gsub("categoryIDs2","catid2",abbrv)
   abbrv = gsub("categoryIDs3","catid3",abbrv)
   abbrv = gsub("categoryIDs4","catid4",abbrv)
   abbrv = gsub("categoryIDs5","catid5",abbrv)
   abbrv = gsub("couponID","cpn",abbrv)

   dsn.res_long = dsn.res
   names(dsn.res_long)[3:ncol(dsn.res_long)] = paste(names(dsn.res_long)[3:ncol(dsn.res_long)],abbrv,sep="_")

   dsn.res_wide1 = dsn.res %>% select(orderID,couponCol,ntimes) %>% spread(couponCol,ntimes) 
   names(dsn.res_wide1)[2:4] = paste0(paste("ntimes",abbrv,"col",sep="_"), names(dsn.res_wide1)[2:4])

   dsn.res_wide2 = dsn.res %>% select(orderID,couponCol,timesUsed) %>% spread(couponCol,timesUsed)
   names(dsn.res_wide2)[2:4] = paste0(paste("timesUsed",abbrv,"col",sep="_"), names(dsn.res_wide1)[2:4]) 

   dsn.res_wide3 = dsn.res %>% select(orderID,couponCol,timesNotUsed) %>% spread(couponCol,timesNotUsed)
   names(dsn.res_wide3)[2:4] = paste0(paste("timesNotUsed",abbrv,"col",sep="_"), names(dsn.res_wide1)[2:4])
   
   dsn.res_wide4 = dsn.res %>% select(orderID,couponCol,llr_est) %>% spread(couponCol,llr_est)
   names(dsn.res_wide4)[2:4] = paste0(paste("llr_est",abbrv,"col",sep="_"), names(dsn.res_wide1)[2:4])

   dsn.res_wide5 = dsn.res %>% select(orderID,couponCol,llr_naive) %>% spread(couponCol,llr_naive)
   names(dsn.res_wide5)[2:4] = paste0(paste("llr_naive",abbrv,"col",sep="_"), names(dsn.res_wide1)[2:4])

   dsn.res_wide = dsn.res_wide1 %>% 
      full_join(dsn.res_wide2,by="orderID") 
      full_join(dsn.res_wide3,by="orderID") 
      full_join(dsn.res_wide4,by="orderID") 
      full_join(dsn.res_wide5,by="orderID") 

   ret = list("long" = dsn.res_long,"wide" = dsn.res_wide)
   return(ret)
}

dsn = dmc
comparecols = 28:29

HTVset1 = readRDS("~/dmc2015/data/featureMatrix/HTVset1.rds")
hst = HTVset1$H
trn = HTVset1$T
val = HTVset1$V
cls = HTVset1$C

names(HTVset1)
result = llr_multiway(dsn,hst,trn,cls,val,comparecols)









   hst.llr = dsn.llr %>% filter(orderID %in% hst.ids)

   #table of usage:

   
   #


   #get table based on 




checkfile = readRDS("~/dmc2015/features/feature_files/set1/llr_cpnXordDoW.rds")
   names(checkfile)
   nrow(checkfile$T)
   head(checkfile$T)

##  way llr

llr = compute_ll_2w(as.character(H$couponID), as.character(H$orderTimeDoW), H$couponUsed, as.character(TVC$couponID), as.character(TVC$orderTimeDoW))
llr_cpnXordDoW = data.frame("couponID" = TVC$couponID, "orderTimeDoW" = TVC$orderTimeDoW, "llr_cpnXordDoW" = llr) %>% arrange(couponID,orderTimeDoW) %>% unique

ggplot(data=llr_cpnXordDoW,aes(x=couponID, y=orderTimeDoW, size = llr_cpnXordDoW),alpha=.1) + geom_point()
qplot(couponID,llr_cpnXordDoW,data=llr_cpnXordDoW, geom='line', group=orderTimeDoW, facets = orderTimeDoW ~ .)

TVC %>% left_join(llr_cpnXordDoW, by=c("couponID","orderTimeDoW")) %>%
  select(orderID,couponID,dsn,orderTimeDoW,llr_cpnXordDoW,couponUsed) %>%
  ggplot(aes(x=orderTimeDoW,y=llr_cpnXordDoW,fill=factor(couponUsed),color=dsn))+geom_boxplot()+facet_grid(dsn~.)

llr_cpnXordDoW = TVC %>% 
  left_join(llr_cpnXordDoW, by=c("couponID","orderTimeDoW")) %>%
  select(orderID,couponCol,dsn,llr_cpnXordDoW)

trn = llr_cpnXordDoW %>% filter(dsn == "train") %>% select(-dsn)
val = llr_cpnXordDoW %>% filter(dsn == "validation") %>% select(-dsn)
cls = llr_cpnXordDoW %>% filter(dsn == "class") %>% select(-dsn)

llr_cpnXordDoW = list("T" = trn, "V" = val, "C" = cls) 

