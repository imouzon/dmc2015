train <- read.csv("/Users/epwalsh/GitHub/dmc2015/data/raw_data/DMC_2015_orders_train.txt", sep="|")
library(lubridate)
library(ggplot2)
library(reshape2)
library(dplyr)
train$orderTime <- ymd_hms(train$orderTime)
train$couponsReceived <- ymd_hms(train$couponsReceived)
train$time_diff = train$orderTime - train$couponsReceived

# create variable for difference in times (minutes)
train$minute_diff = as.numeric(train$time_diff) / 60
train$day_diff = (ceiling_date(train$orderTime, "day") - 
		  ceiling_date(train$couponsReceived, "day")) / (3600*24)

# create variable for day of the week, for both recieved and order time
train$wday_orderTime <- wday(train$orderTime, label=T)
train$wday_couponsReceived <- wday(train$couponsReceived, label=T)

train$coupons_used = train$coupon1Used + train$coupon2Used + train$coupon3Used

coupon1 = train[,c("coupon1Used", "couponID1")]
coupon2 = train[,c("coupon2Used", "couponID2")]
coupon3 = train[,c("coupon3Used", "couponID3")]

names(coupon1) <- c("used", "coupon")
names(coupon2) <- c("used", "coupon")
names(coupon3) <- c("used", "coupon")

coupon1$coupon <- as.character(coupon1$coupon)
coupon1$place <- 1
coupon2$coupon <- as.character(coupon2$coupon)
coupon2$place <- 2
coupon3$coupon <- as.character(coupon3$coupon)
coupon3$place <- 3

couponData = rbind(coupon1, coupon2, coupon3)
couponData$coupon = as.factor(couponData$coupon)
levels(couponData$coupon) = 1:length(levels(couponData$coupon))

couponSummary = couponData %>% group_by(coupon, place) %>%
	summarize(count = length(used),
						used = sum(used))

# get all coupons that appeared in every order
tab = table(couponSummary$coupon)
ids = as.numeric(which(tab == 3))

index = rep(NA, 3*length(ids))
j = 1
for (i in ids) {
	index[j:(j+2)] = which(couponSummary$coupon == i)
	j = j + 3
}

couponSum2 = couponSummary[index,]
couponSum2$rate = couponSum2$used / couponSum2$count
