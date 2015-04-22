#--------------------------------------**--------------------------------------#
#  File Name: interactive.r
#  Purpose:
#
#  Creation Date: 15-04-2015
#  Last Modified: Wed Apr 15 16:20:00 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)

#my personal github library
#devtools::install_github('imouzon/usefulR')
library(plyr)
library(dplyr)
library(usefulR)
library(rCharts)

#add training set
source("./load_data_from_raw.R")
difftime(
         head(trn$couponsReceivedTime)
         head(as.duration(trn$couponsReceivedTime))
         head(interval(hms("00:00:00"),trn$couponsReceived))

         head(as.numeric(trn$couponsReceivedTime))
         head(as.numeric(as.duration(trn$couponsReceivedTime)))
         ,hms("00:00:00",tz='CET'),units="hours")
names(trn)

#stack coupons
source("./R/stackCoupons.R")

#detach("package:plyr", unload=TRUE)


stack.ls = stackCoupons(trn,tst)

stack.trn = stack.ls$train
stack.tst = stack.ls$test
stack.d = stack.ls$combined

names(stack.d)

head(stack.d$productGroup)
head(stack.d$categoryIrs)
unique(stack.d$brand)
brandremap = data.frame(brand = unique(stack.d$productGroup), remap = 1:length(unique(stack.d$brand)))
tbl = data.frame(with(stack.d, table(brand,productGroup)))
names(tbl)
tbl[which(tbl$Freq != 0),]



firstRec = ddply(stack.d, .(couponID,batchID), summarize, 
                 timesRec = length(couponID),
                 firstRec = min(couponsReceived),
                 lastRec = max(couponsReceived),
                 timesUsed = sum(couponUsed),
                 nUsers = length(unique(userID)))

ggplot() + geom_line(data=firstRec,aes(x=batchID,y=timesRec,group=couponID))
   geom_point(data=firstRec,aes(x=firstRec,couponID,size=timesRec,group=couponID)) + 
   geom_line(data=firstRec,aes(x=firstRec,couponID,group=couponID),size=.04) + 
   theme(axis.text.y = element_blank())

+ geom_point() + geom_line()
ggplot(data=firstRec,aes(x=firstRec,couponID,size=timesRec,group=couponID)) + geom_point() + geom_line()
qplot(firstRec,couponID,size=timesRec,data=firstRec) 

stack.d %>% group_by(couponID) %>% mean(couponsReceived)



