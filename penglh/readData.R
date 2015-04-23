setwd("C:/Users/Liuhua/Dropbox/DMC2015")
dataTrain <- read.table("DMC_2015_orders_train.txt",sep="|",header=TRUE)
dataTest <- read.table("DMC_2015_orders_class.txt",sep="|",header=TRUE)
library(dplyr)
library(lubridate)
library(reshape)
dataTrain$orderTime <- ymd_hms(dataTrain$orderTime)
dataTrain$couponsReceived <- ymd_hms(dataTrain$couponsReceived)
dataTrain$timePeriod <- as.numeric(difftime(dataTrain$orderTime,dataTrain$couponsReceived))

######################################################################
################# look at outliers in BasketValue ####################
dataTrain$couponUsedTime <- rowSums(dataTrain[,29:31])
table(dataTrain$couponUsedTime)
usedTime_order0 <- which(dataTrain$couponUsedTime==0)
usedTime_order1 <- which(dataTrain$couponUsedTime==1)
usedTime_order2 <- which(dataTrain$couponUsedTime==2)
usedTime_order3 <- which(dataTrain$couponUsedTime==3)

quan <- seq(0.05,0.95,by=0.05)
plot(quan,quantile(dataTrain$basketValue[usedTime_order0],quan),col=1,pch=15,type="b",ylab="basketValue",xlab="quantile")
lines(quan,quantile(dataTrain$basketValue[usedTime_order1],quan),col=2,type="b",pch=16)
lines(quan,quantile(dataTrain$basketValue[usedTime_order2],quan),col=3,type="b",pch=17)
lines(quan,quantile(dataTrain$basketValue[usedTime_order3],quan),col=4,type="b",pch=18)
legend("topleft",legend=c("0","1","2","3"),col=c(1:4),lwd=1,pch=c(15:18))

plot(quan,quantile(dataTrain$timePeriod[usedTime_order0],quan),col=1,pch=15,type="b",ylab="Time Period(in sec)",xlab="quantile")
lines(quan,quantile(dataTrain$timePeriod[usedTime_order1],quan),col=2,type="b",pch=16)
lines(quan,quantile(dataTrain$timePeriod[usedTime_order2],quan),col=3,type="b",pch=17)
lines(quan,quantile(dataTrain$timePeriod[usedTime_order3],quan),col=4,type="b",pch=18)
legend("topleft",legend=c("0","1","2","3"),col=c(1:4),lwd=1,pch=c(15:18))

############  add a cloumn indicate how many times each user did purchases
dataTrain <- transform(dataTrain,userFreq=ave(seq(nrow(dataTrain)),userID,FUN=length))
table(dataTrain$userFreq)
tapply(dataTrain$basketValue,dataTrain$userFreq,mean)

plot(tapply(dataTrain$basketValue,dataTrain$userFreq,mean))

tapply(dataTrain$couponUsedTime,dataTrain$userFreq,mean)


library(descr)
dataUsedTime3 <- dataTrain[usedTime_order3,]
dataUsedTime3$userID <- factor(dataUsedTime3$userID)
freq(dataUsedTime3$userID)

dataUsedTime2 <- dataTrain[usedTime_order2,]
dataUsedTime2$userID <- factor(dataUsedTime2$userID)
freq(dataUsedTime2$userID)

dataUsedTime1 <- dataTrain[usedTime_order1,]
dataUsedTime1$userID <- factor(dataUsedTime1$userID)
freq(dataUsedTime1$userID)

dataUsedTime0 <- dataTrain[usedTime_order0,]
dataUsedTime0$userID <- factor(dataUsedTime0$userID)
freq(dataUsedTime0$userID)[1,]


############ largest basketValue where coupon is used
max(dataTrain$basketValue[couponUsedTime>0]) # 7799.02
which(dataTrain$basketValue==max(dataTrain$basketValue[couponUsedTime>0])) # 4477
dataTrain[4477,]

outlier_BV <- which(dataTrain$basketValue>1000)
hist(dataTrain$basketValue[-outlier_BV])
dataTrain$coupon1Used[outlier_BV]
couponUsedTime[outlier_BV]
plot(dataTrain$basketValue[outlier_BV],couponUsedTime[outlier_BV])

############ subset containing these outliers
dataTrainOut <- dataTrain[outlier_BV,]

dataOutUser1 <- dataTrain[which(dataTrain$userID==dataTrainOut$userID[1]),]
######################################################################


tableUser <- table(dataTrain$userID)
freUser <- tableUser[which(tableUser>5)]
dataFreUser <- vector("list",length(freUser))
for(i in 1:length(freUser)){
  dataFreUser[[i]] <- dataTrain[dataTrain$userID==names(freUser[i]),]
}

data1 <- dataFreUser[[2]]





tmp <- dataTrain$price1*dataTrain$coupon1Used+dataTrain$price2*dataTrain$coupon2Used+dataTrain$price3*dataTrain$coupon3Used
plot(tmp[dataTrain$basketValue<1000],dataTrain$basketValue[dataTrain$basketValue<1000])

tmp1 <- dataTrain$basePrice1*dataTrain$coupon1Used+dataTrain$basePrice2*dataTrain$coupon2Used+dataTrain$basePrice3*dataTrain$coupon3Used
plot(tmp1[dataTrain$basketValue<1000],dataTrain$basketValue[dataTrain$basketValue<1000])

plot(dataTrain$price1[dataTrain$basePrice1<100],dataTrain$basePrice1[dataTrain$basePrice1<100])
abline(1,1)
plot(dataTrain$price2[dataTrain$basePrice2<100],dataTrain$basePrice2[dataTrain$basePrice2<100])
abline(1,1)
plot(dataTrain$price3[dataTrain$basePrice3<100],dataTrain$basePrice3[dataTrain$basePrice3<100])
abline(1,1)

sum(as.character(dataTrain$brand1)==as.character(dataTrain$brand2))
sum(as.character(dataTrain$productGroup1)==as.character(dataTrain$productGroup2))

data1 <- dataTrain[,c(1:4,which(grepl("1",names(dataTrain))),32)]
names(data1) <- names(dataTrain)[1:12] 
data2 <- dataTrain[,c(1:4,which(grepl("2",names(dataTrain))),32)]
names(data2) <- names(dataTrain)[1:12] 
data3 <- dataTrain[,c(1:4,which(grepl("3",names(dataTrain))),32)]
names(data3) <- names(dataTrain)[1:12] 
dataMelt <- rbind(data1,data2,data3)
colnames(dataMelt)[13:14] <- c("coupon1Used","basketValue")
dataMelt$group <- rep(c(1,2,3),each=dim(dataTrain)[1])


library(randomForest)
library(rpart)
temp <- sample(1:dim(dataMelt)[1],size=dim(dataTrain)[1],replace=FALSE)
dataMeltTrain <- dataMelt[temp,]
dataMeltTrain$coupon1Used <- as.factor(dataMeltTrain$coupon1Used)
rf <- randomForest(coupon1Used~price1+basePrice1+reward1+premiumProduct1,data=dataMeltTrain,ntree=1)

y2 <- rpart(coupon1Used~price1+basePrice1+reward1+premiumProduct1,data=dataMeltTrain)
plot(y2)
y3 <- predict(y2,dataMeltTest)
y3.class = 1*(y3[,1] < y3[,2])
table(y3.class,dataMeltTest$coupon1Used)
dataMeltTest <- dataMelt[-temp,]
y1 <- predict(rf,dataMeltTest)
mean(y1==dataMeltTest$coupon1Used)

library(ggplot2)
ggplot()+geom_point(aes(dataTrain$couponsReceived,dataTrain$orderTime))

boxplot(timePeriod[dataTrain$coupon1Used==0],timePeriod[dataTrain$coupon1Used==1])
boxplot(timePeriod[dataTrain$coupon2Used==0],timePeriod[dataTrain$coupon2Used==1])
boxplot(timePeriod[dataTrain$coupon3Used==0],timePeriod[dataTrain$coupon3Used==1])

user1OrderID <- which(dataTrain$userID==dataTrain$userID[1])
dataUser1 <- dataTrain[which(dataTrain$userID==dataTrain$userID[1]),]


couponUsedTime <- rowSums(dataTrain[,29:31])
table(dataTrain$premiumProduct3,couponUsedTime)



length(unique(dataTrain$userID))
range(dataTrain$basketValue)
hist(dataTrain$basketValue[which(dataTrain$basketValue<1000)])
sum(dataTrain$basketValue<1000)

mean(dataTrain$coupon1Used[dataTrain$basketValue>2000])
mean(dataTrain$coupon2Used[dataTrain$basketValue>2000])
mean(dataTrain$coupon3Used[dataTrain$basketValue>2000])

mean(dataTrain$coupon1Used[dataTrain$basketValue<=2000])
mean(dataTrain$coupon2Used[dataTrain$basketValue<=2000])
mean(dataTrain$coupon3Used[dataTrain$basketValue<=2000])



cp1mean <- mean(dataTrain$coupon1Used)
cp2mean <- mean(dataTrain$coupon2Used)
cp3mean <- mean(dataTrain$coupon3Used)
N <- nrow(dataTrain)

BVmax <- max(dataTrain$basketValue)
BVmean <- mean(dataTrain$basketValue)
BVpred <- seq(0,BVmax,length.out=10000)
error1 <- function(pred,p){
  N*(1-p)*(1/(cp1mean)^2+1/(cp2mean)^2+1/(cp3mean)^2)+(pred-BVmax)^2/(BVmean)^2
}

p8 <- error1(BVpred,0.8)
p85 <- error1(BVpred,0.85)
p9 <- error1(BVpred,0.9)
p95 <- error1(BVpred,0.95)
plot(p8~BVpred,type="l",ylim=c(20000,300000),ylab="error",xlab="prediction of one particular outlier")
lines(p85~BVpred,col=2)
lines(p9~BVpred,col=3)
lines(p95~BVpred,col=4)
abline(v=1000)
abline(h=error1(1000,0.85))
abline(v=10500)
legend(110000,220000,legend=c("0.8","0.85","0.9","0.95"),col=1:4,lty=rep(1,4))
