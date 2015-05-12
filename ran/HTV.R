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


###################
### coupon used ###
###################

data <- HTVset2$H
coupon1 <- data[, c(1, 3, 5:12, 29)]
coupon2 <- data[, c(1, 3, 13:20, 30)]
coupon3 <- data[, c(1, 3, 21:28, 31)]
names(coupon1) <- names(coupon2) <- names(coupon3) <-
  c("orderID", "userID", "couponID", "price", "basePrice", "reward",
    "premiumProduct", "brand", "productGroup", "categoryIDs", "couponUsed")  
H2_melt <- rbind(coupon1, coupon2, coupon3)
H2_melt <- H2_melt[order(H2_melt$orderID),]

# coupon user info
coupon.usr <- H2_melt %>% group_by(couponID, userID) %>% 
  summarize(n.coupon = n(), used = any(couponUsed != 0), n.used = sum(couponUsed))
coupon.usr.info <- coupon.usr %>% group_by(couponID) %>% 
  summarize(nUserSent = n(), nUserUsed = sum(used), UsedTwice = as.numeric(any(n.used > 1)), 
            prop = nUserUsed / nUserSent)

sum(coupon.usr.info$nUserUsed) / sum(coupon.usr.info$nUserSent)  # 0.2015
   