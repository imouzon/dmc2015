setwd("C:/Users/Liuhua/Dropbox/DMC2015")
dataTrain <- read.table("DMC_2015_orders_train.txt",sep="|",header=TRUE)
library(dplyr)
library(lubridate)
library(reshape)
dataTrain$orderTime <- ymd_hms(dataTrain$orderTime)
dataTrain$couponsReceived <- ymd_hms(dataTrain$couponsReceived)
timePeriod <- as.numeric(difftime(dataTrain$orderTime,dataTrain$couponsReceived))


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
