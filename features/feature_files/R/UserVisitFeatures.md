---
title: 
author: 
course: 
date: 
---

[//]: # (R code (No Results in Document))
```{r set-parent, echo=FALSE,warning=FALSE,error=FALSE,message=FALSE,cache=TRUE,warning=FALSE,include=FALSE,comment=NA}
   #set up knitr
   #devtools::install_github('imouzon/usefulR')
   library(usefulR)

   working.dir = "/Users/user/dmc2015/ian"
   setwd(working.dir)
   knitrSetup(rootDir=FALSE,use.tikz=TRUE)

   #set up file locations
   parent.file = makeParent(parentDir = getwd(),type="markdown",overwrite=FALSE)
   set_parent(parent.file)
```
I am using the following packages:
[//]: # (R code (No Results in Document))
```{r paks, echo=TRUE, cache=FALSE, message=FALSE, tidy=FALSE, include=TRUE}
   library(ggplot2)
   library(lubridate)
   library(xtable)
   library(foreach)
   library(rCharts)
   library(magrittr)
   library(tidyr)
   library(dplyr)
   library(reshape2)
   library(gtools)
   library(sqldf)
```
and my working directory is set to \verb!dmc2015/ian!.


[//]: # (: R code (No Results in Document))
```{r echo=FALSE,warning=FALSE,error=FALSE,message=FALSE,cache=TRUE,warning=FALSE,include=FALSE,comment=NA}
   dat = readRDS("../data/clean_data/clean_data_v1.rds")
   tr = dat$train
   te = dat$class

   #combine:
   tr$dataset = "train"
   te$dataset = "test"
   dataset = rbind(tr,te)
   dataset$orderTime = ymd_hms(dataset$orderTime)
   dataset$couponsReceived = ymd_hms(dataset$couponsReceived)
   #dataset_new = dataset[,c(1:4,33)]
   dataset_new = dataset

#this function will be helpful later
#aka I can't get anonymouse functions to work inside mutate
time_between_visits = function(times) {
  c(NA, as.duration(diff(times, lag = 1, differences = 1)))/360
}

#1: user number of times visited total
#2: enumerate the total visits
#3: time between visits (hours)
#4: average time between visits (hours)  -- NaN if they only visit once
#5: variance of time between visits (hours) -- NA if they only visit once
#6: median time between visits (hours) -- NA if they only visit once
#7: max time between visits (hours) -- NA if they only visit once
#8: min time between visits (hours) -- NA if they only visit once
#9: Average time between receiving coupon and ordering
#10: Average time of day (in terms of hour) of order
dataset_new = dataset_new %>% 
  group_by(userID) %>% 
  mutate(tot_visits = n(),
         tot_visitOrder = rank(orderTime),
         time_lastVisit = time_between_visits(orderTime),
         avgTime_visit = mean(time_lastVisit, na.rm=T),
         var_visit = var(time_lastVisit, na.rm=T),
         medianTime_visit = median(time_lastVisit, na.rm=T),
         maxTime_visit = max(time_lastVisit, na.rm=T),
         minTime_visit = min(time_lastVisit, na.rm=T),
         avgTime_recd_order = mean(as.duration(orderTime - couponsReceived)/360),
         avgTime_order = mean(hour(orderTime))
         )

dataset_new = dataset_new %>% group_by(userID,batchID) %>%
   mutate(batchTimesReceived = length(unique(couponsReceived)), 
          batchCountReceived = rank(couponsReceived),
          couponSwitch = (length(unique(couponID1)) > 1 | length(unique(couponID2)) > 1 | length(unique(couponID3)) > 1))
@

These are some wonderful features. Lets write them to a dataset:

[//]: # (chunk-label: R code (No Results in Document))
```{r echo=FALSE,warning=FALSE,eval=FALSE,error=FALSE,message=FALSE,cache=TRUE,warning=FALSE,include=FALSE,comment=NA}
   features = dataset_new[,c(1,50:63)]

   features.trn = features[which(features$dataset == "train"),-2]
   features.cls = features[which(features$dataset == "test"),-2]

   #save it as a RDS file
   saveRDS(list(train=features.train,class = features.cls),"~/dmc2015/features/feature_files/UserVisitFeatures.rds")

   #save it as a CSV
   write.csv(features.trn,file="~/dmc2015/features/feature_files/UserVisitFeatures_train.csv",na="",quote=FALSE,row.names=FALSE)
   write.csv(features.cls,file="~/dmc2015/features/feature_files/UserVisitFeatures_class.csv",na="",quote=FALSE,row.names=FALSE)
```

[//]: # (: R code (No Results in Document))
```{r echo=FALSE,warning=FALSE,error=FALSE,message=FALSE,cache=TRUE,warning=FALSE,include=FALSE,comment=NA}


dataset_new %>% filter(couponSwitch) %>% select(batchID,couponsReceived,couponID1,coupon1Used,couponID2,coupon2Used,couponID3,coupon3Used) %>% arrange(userID,batchID) %>% data.frame
writeset = dataset_new %>% data.frame


userd = dataset_new
userd = userd %>% mutate(coupUsed = paste0(coupon1Used,coupon2Used,coupon3Used))

plotUser = function(user){
   usertmp = userd %>% filter(userID == user)
   p = ggplot() + geom_rect(data=couponBatches, aes(xmin=couponsSent, xmax=couponsSent+weeks(1), ymin=0, ymax=1, fill = batch),alpha=I(.4)) 
   print(p + geom_point(data=usertmp,aes(x=orderTime,y=.6,shape=coupUsed)) + geom_point(data=usertmp,aes(x = couponsReceived,y=.5),color='white'))
   return(usertmp %>% select(batchID,couponsReceived,couponID1,coupon1Used,couponID2,coupon2Used,couponID3,coupon3Used)) %>% data.frame
}

userd %>% group_by(userID, batchID) %>% summarize(timesSent = length(unique(couponsReceived))) %>% ungroup %>% arrange(-timesSent)
userd %>% filter(userID == "user297") %>% select(batchID,couponsReceived,couponID1,coupon1Used,basketValue,coupUsed) %>% data.frame
userd %>% filter(couponID3 == "cpn158" | couponID2 == "cpn158" | couponID1 == "cpn158") %>% 
   select(batchID,couponsReceived,couponID1,coupon1Used,couponID2,coupon2Used,couponID3,coupon3Used) %>% 
   arrange(userID,couponsReceived,batchID) %>%
   data.frame
plotUser("user921")

head(userd)

#break it back into training/test again
tr_new = dataset_new %>% filter(dataset == "train") %>% select(-dataset)
te_new = dataset_new %>% filter(dataset == "test") %>% select(-dataset)
```


