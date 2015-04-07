setwd("//Users/Ran/Documents/ISU/dmc2015/")
train <- read.table("data/raw_data/DMC_2015_orders_train.txt", head = T, sep = "|")
class <- read.table("data/raw_data/DMC_2015_orders_class.txt", head = T, sep = "|")

names(train)
head(train)
summary(train)
attach(train)


###########################################
### order time vs. coupon received time ###
###########################################

nlevels(orderTime)
train[orderTime %in% names(table(orderTime)[table(orderTime) == 2]),]

# time is transformed to c(date, hour, minute, second), date is from 1970-01-01
time <- function(Time){
  date <- as.numeric(as.Date(substr(as.character(Time), 1, 10)))
  hour <- as.numeric(substr(as.character(Time), 12, 13))
  minute <- as.numeric(substr(as.character(Time), 15, 16))
  second <- as.numeric(substr(as.character(Time), 18, 19))
  return(c(date, hour, minute, second))
}

# time difference between order time and coupon received time
coupon_use_time <- function(orderTime, couponsReceived){
  order <- time(orderTime)
  coupon <- time(couponsReceived)
  order_second <- sum(order * c(3600 * 24, 3600, 60, 1))
  coupon_second <- sum(coupon * c(3600 * 24, 3600, 60, 1))
  dif_second <- order_second - coupon_second
  return(c(order_second, coupon_second, dif_second))
}

order_second <- coupon_second <- dif_second <- rep(0, nrow(train))
for (i in 1:nrow(train)){
  dif <- coupon_use_time(orderTime[i], couponsReceived[i]) 
  order_second[i] <- dif[1]
  coupon_second[i] <- dif[2]
  dif_second[i] <- dif[3]
}
start_time <- min(coupon_second)
order_second0 <- order_second - start_time
coupon_second0 <- coupon_second - start_time
dif_second0 <- dif_second

library(ggplot2)
hist(dif_second0/3600, breaks = 100)
plot(coupon_second0, order_second0)
ggplot() + 
  geom_point(aes(x = coupon_second0, y = order_second0)) +
  xlab("coupon_received_time") + ylab("order_time")

#################
### lubridate ###
#################

library(lubridate)
dif_lub <- difftime(ymd_hms(orderTime), ymd_hms(couponsReceived))
hist(as.numeric(dif_lub), breaks = 100)
ggplot() + 
  geom_point(aes(x = ymd_hms(couponsReceived), y = ymd_hms(orderTime))) +
  xlab("coupon_received_time") + ylab("order_time")






nlevels(train$couponID1)

