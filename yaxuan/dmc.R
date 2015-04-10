setwd("F:/DMC/dmc2015")
library(dplyr)
library(lubridate) #change format of the time
library(ggplot2)
# read data
dat.train <- read.table("data/raw_data/DMC_2015_orders_train.txt",sep="|",header=T)
dat.test <- read.table("data/raw_data/DMC_2015_orders_class.txt",sep="|",header=T)

################## explore some features of the training dataset ##################
names(dat.train)
dat.train$orderTime <- as.POSIXct(dat.train$orderTime)
dat.train$couponsReceived <-  as.POSIXct(dat.train$couponsReceived)


# Most of the coupons are received on Tuesdays
dat.train$date <- do.call("c",lapply(dat.train$couponsReceived,function(x) 
  as.Date(paste("2015",month(x),day(x)),format="%Y %m %d")))
ggplot(dat.train)+geom_histogram(aes(x=factor(date)))+theme(axis.text.x=element_text(angle=60,hjust=1))

### coupon received time vs order time
ggplot(dat.train)+geom_point(aes(couponsReceived,orderTime))

### How fast do people use the coupon when they receive it?
dat.train$tdiff <- difftime(dat.train$orderTime,dat.train$couponsReceived,units="mins")
ggplot(dat.train)+geom_point(aes(factor(date),as.numeric(tdiff)))+theme(axis.text.x=element_text(angle=60,hjust=1))

### correlation between time difference and how many coupons are used
dat.train$couponused <- rowSums(dat.train[,29:31])
boxplot(as.numeric(tdiff)~couponused,dat.train,xlab="#coupon used",ylab="time difference")


### some users do several orders
user <- unique(dat.train$userID)
user1 <- dat.train[dat.train$orderID[which(dat.train$userID==user[1])],]

