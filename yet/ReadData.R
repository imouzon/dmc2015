library(ggplot2)
library(plyr)
library(lubridate)
library(reshape)
library(scales)
dat <- read.table("data/raw_data/DMC_2015_orders_train.txt", sep = "|", header = T)

write.csv(dat, file = "yet/dat.csv", row.names = F)
str(dat)
colnames(dat)



dat$orderTime <- ymd_hms(dat$orderTime)
dat$wdorder <- wday(dat$orderTime, label = T)
dat$couponsReceived <- ymd_hms(dat$couponsReceived)
dat$wdcoupon <- wday(dat$couponsReceived, label = T)



##Plot of price vs basePrice
dat$coupon1Used <- as.factor(dat$coupon1Used)
ggplot(dat) + geom_point(aes(y = log(price1+1), x = log(basePrice1+1), colour = coupon1Used))

ggplot(subset(dat, basePrice1 <100)) + 
  geom_point(aes(y = log(price1+1), x = log(basePrice1+1), colour = coupon1Used))

dat$coupon2Used <- as.factor(dat$coupon2Used)
ggplot(dat) + geom_point(aes(y = log(price2+1), x = log(basePrice2+1), colour = coupon2Used))

ggplot(subset(dat, basePrice2 < 100))+ geom_point(aes(y = log(price2+1), x = log(basePrice2+1), colour = coupon2Used))

dat$coupon3Used <- as.factor(dat$coupon3Used)
ggplot(dat) + geom_point(aes(y = log(price3+1), x = log(basePrice3+1), colour = coupon3Used))
ggplot(subset(dat, basePrice3 < 100)) + geom_point(aes(y = log(price3+1), x = log(basePrice3+1), colour = coupon3Used))

## Percentage of used coupons
mean(as.numeric(dat$coupon1Used)-1)
mean(as.numeric(dat$coupon2Used)-1)
mean(as.numeric(dat$coupon3Used)-1)

## Plot orderTime vs couponsReceived
ggplot(dat) + geom_point(aes(y = orderTime, x = couponsReceived, colour = coupon1Used))
ggplot(dat) + geom_point(aes(y = orderTime, x = couponsReceived, colour = coupon2Used))
ggplot(dat) + geom_point(aes(y = orderTime, x = couponsReceived, colour = coupon3Used))


## looking at brand

ggplot(dat) + geom_bar(aes(x = brand1, fill = coupon1Used, colour = coupon1Used))
ggplot(dat) + geom_bar(aes(x = brand2, fill = coupon2Used, colour = coupon2Used))
ggplot(dat) + geom_bar(aes(x = brand3, fill = coupon3Used, colour = coupon3Used))


#wday vs coupon1Used
ggplot(dat) + geom_bar(aes(x = wdcoupon, fill = coupon1Used, colour = coupon1Used))
ggplot(dat) + geom_bar(aes(x = wdcoupon, fill = coupon2Used, colour = coupon2Used))
ggplot(dat) + geom_bar(aes(x = wdcoupon, fill = coupon3Used, colour = coupon3Used))

ggplot(dat) + geom_bar(aes(x = wdorder, fill = coupon1Used, colour = coupon1Used))
ggplot(dat) + geom_bar(aes(x = wdorder, fill = coupon2Used, colour = coupon2Used))
ggplot(dat) + geom_bar(aes(x = wdorder, fill = coupon3Used, colour = coupon3Used))

## reward1
ggplot(dat) + geom_point(aes(y = reward1, x = log(price1+1), colour = coupon1Used))

ggplot(dat) + geom_point(aes(y = reward1, x = price1, colour = coupon1Used))
