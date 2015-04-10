class <- read.csv("/Users/epwalsh/GitHub/dmc2015/data/raw_data/DMC_2015_orders_class.txt", sep="|")
train <- read.csv("/Users/epwalsh/GitHub/dmc2015/data/raw_data/DMC_2015_orders_train.txt", sep="|")

library(lubridate)
library(ggplot2)
library(dplyr)

# how does order time compare to the time the coupon was recieved?
# -----------------------------------------------------------------------
train$orderTime <- ymd_hms(train$orderTime)
train$couponsReceived <- ymd_hms(train$couponsReceived)
train$time_diff = train$orderTime - train$couponsReceived

# create variable for difference in times (minutes)
train$minute_diff = as.numeric(train$time_diff) / 60
train$day_diff = (ceiling_date(train$orderTime, "day") - ceiling_date(train$couponsReceived, "day")) / (3600*24)

# create variable for day of the week, for both recieved and order time
train$wday_orderTime <- wday(train$orderTime, label=T)
train$wday_couponsReceived <- wday(train$couponsReceived, label=T)

# indicates that coupons may have different expiration times
qplot(train$minute_diff, geom="histogram", colour=I("white"))

train %>% ggplot(aes(x = wday_orderTime, fill = wday_couponsReceived)) + geom_bar()
train %>% ggplot(aes(x = wday_couponsReceived, fill = wday_orderTime)) + geom_bar()
# almost everyone who received their coupon on Sunday spent their coupon on Sunday
# if they didn't use it on Sunday, they used it on Monday
train %>% subset(wday_couponsReceived == "Sun") %>% 
  ggplot(aes(x = wday_orderTime, fill = as.factor(day_diff))) + geom_bar()

# every single person who spent their coupon on Tuesday had recieved their coupon on Tuesday, the same day
wday_sum <- train %>% group_by(wday_couponsReceived, wday_orderTime) %>% 
  summarize(count = length(orderID))
summary(subset(train, wday_orderTime == "Tues")$minute_diff)




train %>% subset(basketValue < 1000) %>%
  ggplot(aes(x = minute_diff, y = basketValue)) + geom_point()


# coupon usage
# -----------------------------------------------------------------------
# coupon 1 used the most, then coupon 2, then coupon 3
sum(train$coupon1Used)
sum(train$coupon2Used)
sum(train$coupon3Used)

# what the hell is the difference between basePrice, price, and reward?
summary(train$basePrice1+train$reward1-train$price1)





