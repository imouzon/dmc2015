#------------------------------**------------------------------# 
#
# DMC_2015: coupon feature
# dmc2015_v_1.R
# 
#------------------------------**------------------------------# 

library(ggplot2)
library(plyr)
library(lsr)
unlibrary(plyr)
library(lubridate)
library(dplyr)
library(fitdistrplus)

setwd('/Users/yihuali/Documents/dmc2015')

dat <- read.csv('./data/clean_data/melted_train_simple_name.csv', header=T)
names(dat)

dat.uc <- dat %>% group_by(userID, couponID) %>% 
  summarise(nUser=n(), nUsed=sum(couponUsed))

column1 <- dat %>% group_by(couponID) %>% 
  summarize(nUser=length(unique(userID)))
column2 <- dat.uc %>% group_by(couponID) %>% 
  summarize(nUserUsed=sum(nUsed>=1), nUserTwiceorMore=sum(nUsed>=2))
  
dat.coupon <- merge(column1, column2, by="couponID")
dat.coupon$nUserNotUsed <- dat.coupon$nUser - dat.coupon$nUserUsed
dat.coupon$SimpleProb <- dat.coupon$nUserUsed/dat.coupon$nUser
hist(dat.coupon$SimpleProb,30)
sum(dat.coupon$nUserUsed) # 3499
sum(dat.coupon$nUser) # 17079

alpha <- mmedist(dat.coupon$SimpleProb, "beta")$estimate[1]
beta <- mmedist(dat.coupon$SimpleProb, "beta")$estimate[2]

dat.coupon$Prob <- (dat.coupon$nUserUsed+alpha) / (dat.coupon$nUser+alpha+beta)
