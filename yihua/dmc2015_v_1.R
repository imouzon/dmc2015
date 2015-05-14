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

dat.set3 <- readRDS('./data/featureMatrix/HTVset2.rds')
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
column2$TwiceOrMore <- as.numeric(column2$nUserTwiceorMore>=1)

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
dat.coupon <- dat.coupon[,c(-4,-7)]

# merge to training, validation, classification sets
histo <- dat.set3$H # 4540
train <- dat.set3$T # 945
valid <- dat.set3$V # 568
class <- dat.set3$C # 669

histo <- merge(histo, dat.coupon, by.x='couponID1', by.y='couponID', all.x=T)
train <- merge(train, dat.coupon, by.x='couponID1', by.y='couponID', all.x=T)
valid <- merge(valid, dat.coupon, by.x='couponID1', by.y='couponID', all.x=T)
class <- merge(class, dat.coupon, by.x='couponID1', by.y='couponID', all.x=T)
names(histo)[50:54] <- c('nUser1', 'nUserUsed1', 'nUserTwiceOrMore1',
                         'nUserNotUsed1', 'Prob1')
names(train)[50:54] <- c('nUser1', 'nUserUsed1', 'nUserTwiceOrMore1',
                         'nUserNotUsed1', 'Prob1')
names(valid)[50:54] <- c('nUser1', 'nUserUsed1', 'nUserTwiceOrMore1',
                         'nUserNotUsed1', 'Prob1')
names(class)[50:54] <- c('nUser1', 'nUserUsed1', 'nUserTwiceOrMore1',
                         'nUserNotUsed1', 'Prob1')

histo <- merge(histo, dat.coupon, by.x='couponID2', by.y='couponID', all.x=T)
train <- merge(train, dat.coupon, by.x='couponID2', by.y='couponID', all.x=T)
valid <- merge(valid, dat.coupon, by.x='couponID2', by.y='couponID', all.x=T)
class <- merge(class, dat.coupon, by.x='couponID2', by.y='couponID', all.x=T)
names(histo)[55:59] <- c('nUser2', 'nUserUsed2', 'nUserTwiceOrMore2',
                         'nUserNotUsed2', 'Prob2')
names(train)[55:59] <- c('nUser2', 'nUserUsed2', 'nUserTwiceOrMore2',
                         'nUserNotUsed2', 'Prob2')
names(valid)[55:59] <- c('nUser2', 'nUserUsed2', 'nUserTwiceOrMore2',
                         'nUserNotUsed2', 'Prob2')
names(class)[55:59] <- c('nUser2', 'nUserUsed2', 'nUserTwiceOrMore2',
                         'nUserNotUsed2', 'Prob2')

histo <- merge(histo, dat.coupon, by.x='couponID3', by.y='couponID', all.x=T)
train <- merge(train, dat.coupon, by.x='couponID3', by.y='couponID', all.x=T)
valid <- merge(valid, dat.coupon, by.x='couponID3', by.y='couponID', all.x=T)
class <- merge(class, dat.coupon, by.x='couponID3', by.y='couponID', all.x=T)
names(histo)[60:64] <- c('nUser3', 'nUserUsed3', 'nUserTwiceOrMore3',
                         'nUserNotUsed3', 'Prob3')
names(train)[60:64] <- c('nUser3', 'nUserUsed3', 'nUserTwiceOrMore3',
                         'nUserNotUsed3', 'Prob3')
names(valid)[60:64] <- c('nUser3', 'nUserUsed3', 'nUserTwiceOrMore3',
                         'nUserNotUsed3', 'Prob3')
names(class)[60:64] <- c('nUser3', 'nUserUsed3', 'nUserTwiceOrMore3',
                         'nUserNotUsed3', 'Prob3')

histo[is.na(histo)] <- 0
train[is.na(train)] <- 0
valid[is.na(valid)] <- 0
class[is.na(class)] <- 0
histo$Prob1[histo$Prob1==0] <- alpha/(alpha+beta)
histo$Prob2[histo$Prob2==0] <- alpha/(alpha+beta)
histo$Prob3[histo$Prob3==0] <- alpha/(alpha+beta)
train$Prob1[train$Prob1==0] <- alpha/(alpha+beta)
train$Prob2[train$Prob2==0] <- alpha/(alpha+beta)
train$Prob3[train$Prob3==0] <- alpha/(alpha+beta)
valid$Prob1[valid$Prob1==0] <- alpha/(alpha+beta)
valid$Prob2[valid$Prob2==0] <- alpha/(alpha+beta)
valid$Prob3[valid$Prob3==0] <- alpha/(alpha+beta)
class$Prob1[class$Prob1==0] <- alpha/(alpha+beta)
class$Prob2[class$Prob2==0] <- alpha/(alpha+beta)
class$Prob3[class$Prob3==0] <- alpha/(alpha+beta)

a <- vector('list',4)
a[[1]] <- histo
a[[2]] <- train
a[[3]] <- valid
a[[4]] <- class
names(a) <- c('H','T','V','C')
saveRDS(a, file='./yihua/HTVset2_Coupon_UniqueUser.rds')

tmp <- readRDS(file='./yihua/HTVset1_Coupon_UniqueUser.rds')
training <- tmp$T
test <- tmp$V

library(randomForest)
rf1 <- randomForest(as.factor(coupon1Used)~Prob1, 
                    data=training, ntree=500)
result <- predict(rf1, test)
result <- as.numeric(result>0.5)
table(test$coupon1Used, result)
