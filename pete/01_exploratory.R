class <- read.csv("/Users/epwalsh/GitHub/dmc2015/data/raw_data/DMC_2015_orders_class.txt", sep="|")
train <- read.csv("/Users/epwalsh/GitHub/dmc2015/data/raw_data/DMC_2015_orders_train.txt", sep="|")

library(lubridate)
library(ggplot2)
library(dplyr)
library(reshape2)

# ==================================================================
# how does order time compare to the time the coupon was recieved?
# ==================================================================

train$orderTime <- ymd_hms(train$orderTime)
train$couponsReceived <- ymd_hms(train$couponsReceived)
train$time_diff = train$orderTime - train$couponsReceived

# create variable for difference in times (minutes)
train$minute_diff = as.numeric(train$time_diff) / 60
train$day_diff = (ceiling_date(train$orderTime, "day") - ceiling_date(train$couponsReceived, "day")) / (3600*24)

# create variable for day of the week, for both recieved and order time
train$wday_orderTime <- wday(train$orderTime, label=T)
train$wday_couponsReceived <- wday(train$couponsReceived, label=T)

qplot(train$minute_diff, geom="histogram", colour=I("white"))

train %>% ggplot(aes(x = wday_orderTime, fill = wday_couponsReceived)) + geom_bar()
train %>% ggplot(aes(x = wday_couponsReceived, fill = wday_orderTime)) + geom_bar()

# every single person who spent their coupon on Tuesday had recieved their coupon on Tuesday, the same day
wday_sum <- train %>% group_by(wday_couponsReceived, wday_orderTime) %>% 
  summarize(count = length(orderID))

# most coupons are sent out on Tuesday
# all coupons expire before Tuesday
train %>% ggplot(aes(x = wday_couponsReceived, fill = factor(day_diff))) + geom_bar()



# ==================================================================
# coupon usage
# ==================================================================

# coupon 1 used the most, then coupon 2, then coupon 3
sum(train$coupon1Used)
sum(train$coupon2Used)
sum(train$coupon3Used)

train$coupons_used = train$coupon1Used + train$coupon2Used + train$coupon3Used
train %>% ggplot(aes(x = wday_couponsReceived, fill = factor(coupons_used))) + geom_bar()

usage = train %>% group_by(wday_couponsReceived) %>%
  summarize(none = round(sum(coupons_used == 0) / length(coupons_used), 2),
            one = round(sum(coupons_used == 1) / length(coupons_used), 2),
            two = round(sum(coupons_used == 2) / length(coupons_used), 2),
            three = round(sum(coupons_used == 3) / length(coupons_used), 2)) %>%
  melt(id = "wday_couponsReceived", 
       variable.name = "used",
       value.name = "perc")
usage <- usage[order(usage$wday_couponsReceived),]

qplot(used, perc, data = usage, facets = ~wday_couponsReceived, 
      geom="bar", stat = "identity")

# ==================================================================
# users
# ==================================================================
levels(train$userID) <- as.character(1:length(levels(train$userID)))

# ==================================================================
# what the hell is the difference between basePrice, price, and reward?
# ==================================================================
summary(train$basePrice1+train$reward1-train$price1)

# still no idea



