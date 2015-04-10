setwd("//Users/Ran/Dropbox/ISU/dmc2015/")
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
qplot(dif_second0, geom = "histogram", binwidth = 10000) + xlab("received_use_dif")
ggplot() + 
  geom_point(aes(x = coupon_second0, y = order_second0)) +
  xlab("coupon_received_time") + ylab("order_time")

#################
### lubridate ###
#################

library(lubridate)
dif_lub <- as.numeric(difftime(ymd_hms(orderTime), ymd_hms(couponsReceived), units = "mins"))  # difference in seconds
qplot(dif_lub, geom = "histogram", binwidth = 200) + xlab("received_use_dif")
ggplot() + 
  geom_point(aes(x = ymd_hms(couponsReceived), y = ymd_hms(orderTime))) +
  xlab("coupon_received_time") + ylab("order_time")


###########################
### coupon used or not? ###
###########################

couponUsed <- train[, 29:31]
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
boxplot(dif_lub ~ couponUsed[, 4], xlab = "number of coupons used", 
        ylab = "order_received time diff")


#######################
### melt by coupons ###
#######################

coupon1 <- train[, c(1:12, 29)]
coupon2 <- train[, c(1:4, 13:20, 30)]
coupon3 <- train[, c(1:4, 21:28, 31)]
coupon1$coupon_num <- 1
coupon2$coupon_num <- 2
coupon3$coupon_num <- 3
names(coupon1) <- names(coupon2) <- names(coupon3) <-
  c("orderID", "orderTime", "userID", "couponsReceived", "couponID", "price", "basePrice", "reward",
    "premiumProduct", "brand", "productGroup", "categoryIDs", "couponUsed", "coupon_num")  
train_melt <- rbind(coupon1, coupon2, coupon3)
train_melt <- train_melt[order(train_melt$orderID),]



nlevels(train$couponID1)

