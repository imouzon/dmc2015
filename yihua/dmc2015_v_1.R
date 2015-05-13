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

dat.org <- read.csv('./data/clean_data/melted_train_simple_name.csv', header=T)
names(dat)
batch <- read.csv('./features/feature_files/csv/batchinfo_train.csv', header=T)
names(batch)
batch <- batch[,c('orderID','batchID')]
dat.full <- merge(dat.org, batch, by='orderID')
dat <- subset(dat.full, batchID<=7)

dat.set3 <- readRDS('./data/featureMatrix/HTVset3.rds')
dat.H <- dat.set3$H
dat.H1 <- dat.H[,c(1,3,5:12,29)]
dat.H2 <- dat.H[,c(1,3,13:20,30)]
dat.H3 <- dat.H[,c(1,3,21:28,31)]
names(dat.H1) <- names(dat.H2) <- names(dat.H3) <-
  c('orderID', 'userID', 'couponID', 'price', 'basePrice',
    'reward', 'premiumProduct', 'brand', 'productGroup', 
    'categoryIDs', 'couponUsed')
dat <- rbind(dat.H1, dat.H2, dat.H3)

dat.uc <- dat %>% group_by(userID, couponID) %>% 
  summarise(nUser=n(), nUsed=sum(couponUsed))

column1 <- dat %>% group_by(couponID) %>% 
  summarize(nUser=length(unique(userID)))
column2 <- dat.uc %>% group_by(couponID) %>% 
  summarize(nUserUsed=sum(nUsed>=1), nUserTwiceorMore=sum(nUsed>=2))
  
dat.coupon <- merge(column1, column2, by="couponID")
dat.coupon <- dat.coupon[order(dat.coupon$couponID),]
dat.coupon$nUserNotUsed <- dat.coupon$nUser - dat.coupon$nUserUsed
dat.coupon$SimpleProb <- dat.coupon$nUserUsed/dat.coupon$nUser
hist(dat.coupon$SimpleProb,30)
sum(dat.coupon$nUserUsed) # 2591 (batch1-7) 3499 (all)
sum(dat.coupon$nUser) # 12878 (batch1-7) 17079 (all)

alpha <- mmedist(dat.coupon$SimpleProb, "beta")$estimate[1]
beta <- mmedist(dat.coupon$SimpleProb, "beta")$estimate[2]

dat.coupon$Prob <- (dat.coupon$nUserUsed+alpha) / (dat.coupon$nUser+alpha+beta)

head(dat.coupon)

# merge to training, validation, classification sets
train <- dat.set3$T # 945
valid <- dat.set3$V # 568
class <- dat.set3$C # 669

train <- merge()

good <- subset(dat.coupon, SimpleProb==1)
dim(good[good$nUser>1,])
dim(good)
