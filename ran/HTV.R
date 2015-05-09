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
  summarize(n.usr = n(), TimeBtwnSentRec.mean = mean(TimeBtwnSentRec), TimeBtwnSentRec.max = max(TimeBtwnSentRec), 
            TimeBtwnSentRec.min = min(TimeBtwnSentRec), TimeBtwnSentRec.median = median(TimeBtwnSentRec))

# Time between received and expire
TimeBtwnRecExpire.info <- data %>% group_by(userID) %>% 
  summarize(n.usr = n(), TimeBtwnRecExpire.mean = mean(TimeBtwnRecExpire), TimeBtwnRecExpire.max = max(TimeBtwnRecExpire), 
            TimeBtwnRecExpire.min = min(TimeBtwnRecExpire), TimeBtwnRecExpire.median = median(TimeBtwnRecExpire))

# Time between received and order
TimeBtwnRecOrder.info <- data %>% group_by(userID) %>% 
  summarize(n.usr = n(), TimeBtwnRecOrder.mean = mean(TimeBtwnRecOrder), TimeBtwnRecOrder.max = max(TimeBtwnRecOrder), 
            TimeBtwnRecOrder.min = min(TimeBtwnRecOrder), TimeBtwnRecOrder.median = median(TimeBtwnRecOrder))

# Time between order and expire
TimeBtwnOrderExpire.info <- data %>% group_by(userID) %>% 
  summarize(n.usr = n(), TimeBtwnOrderExpire.mean = mean(TimeBtwnOrderExpire), TimeBtwnOrderExpire.max = max(TimeBtwnOrderExpire), 
            TimeBtwnOrderExpire.min = min(TimeBtwnOrderExpire), TimeBtwnOrderExpire.median = median(TimeBtwnOrderExpire))

# Order time
# number of orders in weekend, weekday, FridaySaturday, worktime, early order (first day)
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

# user batch info
usr.batch <- data %>% group_by(userID, batchID) %>% summarize(n.order = n())
orderperweek <- usr.batch %>% group_by(userID) %>% 
  summarize(orderMaxPerWeek = max(n.order), orderMinPerWeek = min(n.order), ActiveWeek = n())

# saveRDS
usr.info <- cbind(TimeBtwnSentRec.info, TimeBtwnRecExpire.info[, -(1:2)], 
                  TimeBtwnRecOrder.info[, -(1:2)], TimeBtwnOrderExpire.info[, -(1:2)], 
                  ordertime.info[, -(1:2)], couponReceived.info[, -(1:2)], orderperweek[, -1])
saveRDS(usr.info, "//Users/Ran/Dropbox/ISU/dmc2015/features/feature_files/universal/user_info.rds")
