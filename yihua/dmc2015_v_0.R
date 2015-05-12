#------------------------------**------------------------------# 
#
# DMC_2015: data preparation
# dmc2015_v_0.R
# 
#------------------------------**------------------------------# 

library(ggplot2)
library(plyr)
library(lubridate)

setwd('/Users/yihuali/Documents/dmc2015')

train <- read.table("data/raw_data/DMC_2015_orders_train.txt", head = T, sep = "|")
class <- read.table("data/raw_data/DMC_2015_orders_class.txt", head = T, sep = "|")

names(train)
head(train)
summary(train)

tr <- train
te <- class

#################
### lubridate ###
#################

# difference in seconds
tr$diff <- as.numeric(difftime(ymd_hms(tr$orderTime), ymd_hms(tr$couponsReceived)))  
qplot(diff, data=tr, geom = "histogram", binwidth = 10000) + xlab("received_use_dif")

# 40% of orders used at least one coupon
mean(tr$coupon3Used+tr$coupon1Used+tr$coupon2Used==0) 
mean(tr$coupon3Used+tr$coupon1Used+tr$coupon2Used==1) 
mean(tr$coupon3Used+tr$coupon1Used+tr$coupon2Used==2) 
mean(tr$coupon3Used+tr$coupon1Used+tr$coupon2Used==3) 

# coupon_received_time vs. order_time 
ggplot() + 
  geom_point(data=subset(tr, coupon3Used+coupon1Used+coupon2Used>0),
             aes(x = ymd_hms(couponsReceived), y = ymd_hms(orderTime))) +
  xlab("coupon_received_time") + ylab("order_time")
ggplot() + 
  geom_point(data=subset(tr, coupon3Used+coupon1Used+coupon2Used==0),
             aes(x = ymd_hms(couponsReceived), y = ymd_hms(orderTime))) +
  xlab("coupon_received_time") + ylab("order_time")
ggplot() + 
  geom_point(data=tr,
             aes(x = ymd_hms(couponsReceived), y = ymd_hms(orderTime))) +
  xlab("coupon_received_time") + ylab("order_time")

###############################
### Which coupons are used? ###
###############################

# split into one row for each coupon
coupon1.tr <- tr[,c(1:4,5:12,29,32:33)]
coupon1.tr$coupon <- 1
coupon2.tr <- tr[,c(1:4,13:20,30,32:33)]
coupon2.tr$coupon <- 2
coupon3.tr <- tr[,c(1:4,21:28,31,32:33)]
coupon3.tr$coupon <- 3
names(coupon1.tr) <- names(coupon2.tr) <- names(coupon3.tr) <- 
  c("orderID", "orderTime", "userID", "couponsReceived", 
    "couponID", "price", "basePrice", "reward",
    "premiumProduct", "brand", "productGroup", "categoryIDs", 
    "couponUsed", "basketValue", "diff", "coupon")
coupon.tr <- rbind(coupon1.tr, coupon2.tr, coupon3.tr)
coupon.tr <- coupon.tr[order(coupon.tr$orderID),]
head(coupon.tr)

# Do people respond to the same coupon similarly?
coupon.used <- ddply(coupon.tr, .(couponID), summarise, 
                     used=mean(couponUsed), dispatch=length(couponUsed),
                     appear1=sum(coupon==1), appear2=sum(coupon==2),
                     appear3=sum(coupon==3), diff.m=mean(diff),
                     price=mean(price), basePrice=mean(basePrice),
                     brand=unique(brand), 
                     category=length(strsplit(unique(as.character(categoryIDs)),split=',')[[1]]),
                     reward=mean(reward))

head(coupon.used)
summary(as.factor(coupon.used$category))
summary(coupon.used$used)
hist(coupon.used$used, 100)
best <- subset(coupon.used, used==1)
hist(best$dispatch)
hist(best$reward)
qplot(price, basePrice, data=best, xlim=c(0,25), ylim=c(0,25), color=dispatch)
worst <- subset(coupon.used, used==0)
hist(worst$dispatch)
hist(worst$reward)
qplot(price, basePrice, data=worst, xlim=c(0,25), ylim=c(0,25), color=dispatch)
qplot(used, log(as.numeric(diff)), data=coupon.used)

# do people treat all coupons similarly?
customer <- ddply(tr, .(userID), summarise, 
                  used=mean(coupon1Used+coupon2Used+coupon3Used), 
                  used0=sum(coupon1Used+coupon2Used+coupon3Used==0),
                  used1=sum(coupon1Used+coupon2Used+coupon3Used==1),
                  used2=sum(coupon1Used+coupon2Used+coupon3Used==2),
                  used3=sum(coupon1Used+coupon2Used+coupon3Used==3),
                  appear=length(orderID),
                  use=sum(coupon1Used+coupon2Used+coupon3Used>0),
                  diff.m=mean(diff),
                  basketValue= mean(basketValue))
customer <- customer[order(customer$appear, decreasing=T),]
table(customer$appear, customer$use)
table(customer$appear, customer$used0)
table(customer$appear, customer$used1)
table(customer$appear, customer$used2)
table(customer$appear, customer$used3)

qplot(used, data=customer, geom='histogram')
qplot(used0, data=customer, geom='histogram')
qplot(used1, data=customer, geom='histogram')
qplot(used2, data=customer, geom='histogram')
qplot(used3, data=customer, geom='histogram')

# For those who received the same coupon multiple times, 
# are those responsed consistent?
cst.order <- ddply(coupon.tr, .(userID, couponID), summarise, 
                   count=length(orderID),
                   used=mean(couponUsed), 
                   use=sum(couponUsed>0),
                   diff.m=mean(diff),
                   basketValue= mean(basketValue))
cst.order <- cst.order[order(cst.order$count, decreasing=T),]

qplot(count, use, data=cst.order)
qplot(count, used, data=cst.order)
qplot(used, geom='histogram', data=cst.order)
table(cst.order$count, cst.order$use)

###########################
### coupon used or not? ###
###########################

couponUsed <- train[, c('coupon1Used','coupon2Used','coupon3Used')]
couponUsed$coupon_num <- apply(couponUsed, 1, sum)

# coupon 1
dif_coupon11 <- dif_lub[couponUsed[, 1] == 1]
dif_coupon10 <- dif_lub[couponUsed[, 1] == 0]
boxplot(dif_coupon11, dif_coupon10)
# coupon 2
dif_coupon21 <- dif_lub[couponUsed[, 2] == 1]
dif_coupon20 <- dif_lub[couponUsed[, 2] == 0]
boxplot(dif_coupon21, dif_coupon20)
# coupon 3
dif_coupon31 <- dif_lub[couponUsed[, 3] == 1]
dif_coupon30 <- dif_lub[couponUsed[, 3] == 0]
boxplot(dif_coupon31, dif_coupon30)

# time difference between order time and coupon received time
# vs. number of coupon used
boxplot(dif_lub ~ couponUsed[, 4])

# detect coupon information
# Finding: same couponID have same coupon info
x <- table(coupon.tr$couponID, coupon.tr$reward)
length(unique(coupon.tr$couponID))
sum(x>0)

plot(log(coupon.tr$price/Coupon$basePrice), coupon.tr$couponUsed)

# How much worth of products does people buy?
qplot(log(coupon1Used*price1+coupon2Used*price2+coupon3Used*price3), 
      log(basketValue), xlim=c(0,5), ylim=c(0,10), data=tr)
qplot(log(basketValue)-log(coupon1Used*price1+coupon2Used*price2+coupon3Used*price3),
      data=tr, geom='histogram')
