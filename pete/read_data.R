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
train$day_diff = as.numeric(train$day_diff)

# create variable for day of the week, for both recieved and order time
train$wday_orderTime <- wday(train$orderTime, label=T)
train$wday_couponsReceived <- wday(train$couponsReceived, label=T)

train$coupons_used = train$coupon1Used + train$coupon2Used + train$coupon3Used

all_coups = c(as.character(train$couponID1), 
							as.character(train$couponID2),
							as.character(train$couponID3))
all_coups = as.factor(all_coups)
levels(all_coups) = 1:length(levels(all_coups))
train$couponID1 = all_coups[1:nrow(train)]
train$couponID2 = all_coups[(nrow(train) + 1):(2*nrow(train))]
train$couponID3 = all_coups[(2*nrow(train) + 1):length(all_coups)]
