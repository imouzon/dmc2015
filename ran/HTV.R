HTVset1 <- readRDS("//Users/Ran/Dropbox/ISU/dmc2015/data/featureMatrix/HTVset1.rds")
HTVset2 <- readRDS("//Users/Ran/Dropbox/ISU/dmc2015/data/featureMatrix/HTVset2.rds")
HTVset3 <- readRDS("//Users/Ran/Dropbox/ISU/dmc2015/data/featureMatrix/HTVset3.rds")

names(HTVset1)
names(HTVset1$H)
head(HTVset1$H$orderID)

#########################
### Universal feature ###
#########################

data <- rbind(HTVset1$H, HTVset1$V, HTVset1$T, HTVset1$C)
user.ID <- unique(data$userID)

# Time between sent and received
# mean, max, min, median
TimeBtwnSentRec.info <- data %>% group_by(userID) %>% 
  summarize(n.usr = n(), sr.mean = mean(TimeBtwnSentRec), sr.max = max(TimeBtwnSentRec), 
            sr.min = min(TimeBtwnSentRec), sr.median = median(TimeBtwnSentRec))

# Time between received and expire
TimeBtwnRecExpire.info <- data %>% group_by(userID) %>% 
  summarize(n.usr = n(), re.mean = mean(TimeBtwnRecExpire), re.max = max(TimeBtwnRecExpire), 
            re.min = min(TimeBtwnRecExpire), re.median = median(TimeBtwnRecExpire))

# Time between received and order
TimeBtwnRecOrder.info <- data %>% group_by(userID) %>% 
  summarize(n.usr = n(), ro.mean = mean(TimeBtwnRecOrder), ro.max = max(TimeBtwnRecOrder), 
            ro.min = min(TimeBtwnRecOrder), ro.median = median(TimeBtwnRecOrder))

# Time between order and expire
TimeBtwnOrderExpire.info <- data %>% group_by(userID) %>% 
  summarize(n.usr = n(), oe.mean = mean(TimeBtwnOrderExpire), oe.max = max(TimeBtwnOrderExpire), 
            oe.min = min(TimeBtwnOrderExpire), oe.median = median(TimeBtwnOrderExpire))

# Order time
# number of orders in weekend, weekday, FridaySaturday, worktime, 
ordertime.info <- data %>% group_by(userID) %>% 
  summarize(n.usr = n(), order.weekend = sum(as.numeric(orderTimeWeekend) - 1), 
            order.FriSat = sum(as.numeric(orderTimeFriSat) - 1),
            order.weekday = sum(2 - as.numeric(orderTimeWeekend)),
            order.worktime = sum((orderTimeTime <= 18) & (orderTimeTime >= 8) & (orderTimeWeekend == 0)),
            order.early = sum(TimeBtwnRecOrder <= 24))

# Coupon received time
couponReceived.info <- data %>% group_by(userID) %>% 
  summarize(n.usr = n(), couponRec.weekend = sum(as.numeric(couponsReceivedWeekend) - 1), 
            couponRec.FriSat = sum(as.numeric(couponsReceivedFriSat) - 1),
            couponRec.weekday = sum(2 - as.numeric(couponsReceivedWeekend)))
