HTVset1 <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/data/featureMatrix/HTVset1.rds")
HTVset2 <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/data/featureMatrix/HTVset2.rds")
HTVset3 <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/data/featureMatrix/HTVset3.rds")

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
order.usr <- data[, c(1, 3)]
order.usr.info <- order.usr %>% left_join(usr.info)
order.usr.info <- order.usr.info[order(order.usr.info$orderID), ]
saveRDS(order.usr.info, "//Users/Ran/Google Drive/ISU/dmc2015/features/feature_files/universal/user_info.rds")


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

library(fitdistrplus)
alpha <- mmedist(coupon.usr.info$prop, "beta")$estimate[1]
beta <- mmedist(coupon.usr.info$prop, "beta")$estimate[2]

coupon.usr.info$prob <- (coupon.usr.info$nUserUsed + alpha) / (coupon.usr.info$nUserSent + alpha + beta)
saveRDS(coupon.usr.info, "//Users/Ran/Google Drive/ISU/dmc2015/ran/coupon_usr_info.rds")


###########
### SVM ###
###########

library(ggplot2)
library(lsr)
library(lubridate)
library(dplyr)
library(fitdistrplus)
library(e1071)

dat <- HTVset1
H <- dat$H
T <- dat$T
V <- dat$V
C <- dat$C
H_melt <- stackCoupons2(H, idcols = c(1:4, 32:49))
T_melt <- stackCoupons2(T, idcols = c(1:4, 32:49))
V_melt <- stackCoupons2(V, idcols = c(1:4, 32:49))
C_melt <- stackCoupons2(C, idcols = c(1:4, 32:49))

Feature <- addFeatures_HTV(H_melt, T_melt, V_melt)
Feature$H_melt <- Feature$H_melt[, -80]
Feature$T_melt <- Feature$T_melt[, -80]
Feature$V_melt <- Feature$V_melt[, -80]

# delete constant feature
keep <- which(as.numeric(apply(Feature$T_melt, 2, function(x){length(unique(as.factor(x))) > 1})) == 1)

# linear kernel
svmfit_linear <- svm(x = Feature$T_melt[, keep[c(34, 36:length(keep))]], 
                     y = as.factor(Feature$T_melt$couponUsed), 
                     kernel = "linear")
pred_linear_T <- predict(svmfit_linear, Feature$T_melt[, keep[c(34, 36:length(keep))]])
table_linear_T <- table(pred_linear_T, Feature$T_melt$couponUsed)
1 - sum(diag(table_linear_T)) / sum(table_linear_T)  # 0.2
# validation error
pred_linear <- predict(svmfit_linear, Feature$V_melt[, keep[c(34, 36:length(keep))]])
table_linear <- table(pred_linear, Feature$V_melt$couponUsed)
1 - sum(diag(table_linear)) / sum(table_linear)  # 0.1911

# polynomial kernel with default order 3
svmfit_polynomial <- svm(x = Feature$T_melt[, keep[c(34, 36:length(keep))]], 
                         y = as.factor(Feature$T_melt$couponUsed), 
                         kernel = "polynomial")
pred_polynomial_T <- predict(svmfit_polynomial, Feature$T_melt[, keep[c(34, 36:length(keep))]])
table_polynomial_T <- table(pred_polynomial_T, Feature$T_melt$couponUsed)
1 - sum(diag(table_polynomial_T)) / sum(table_polynomial_T)  # 0.1612
# validation error
pred_polynomial <- predict(svmfit_polynomial, Feature$V_melt[, keep[c(34, 36:length(keep))]])
table_polynomial <- table(pred_polynomial, Feature$V_melt$couponUsed)
1 - sum(diag(table_polynomial)) / sum(table_polynomial)  # 0.1903

# radial kernel with default gamma
svmfit_radial <- svm(x = Feature$T_melt[, keep[c(34, 36:length(keep))]], 
                     y = as.factor(Feature$T_melt$couponUsed), 
                     kernel = "radial")
pred_radial_T <- predict(svmfit_radial, Feature$T_melt[, keep[c(34, 36:length(keep))]])
table_radial_T <- table(pred_radial_T, Feature$T_melt$couponUsed)
1 - sum(diag(table_radial_T)) / sum(table_radial_T)  # 0.1823
# validation error
pred_radial <- predict(svmfit_radial, Feature$V_melt[, keep[c(34, 36:length(keep))]])
table_radial <- table(pred_radial, Feature$V_melt$couponUsed)
1 - sum(diag(table_radial)) / sum(table_radial)  # 0.1888

# radial kernel with half default gamma
svmfit_radial0.5 <- svm(x = Feature$T_melt[, keep[c(34, 36:length(keep))]], 
                        y = as.factor(Feature$T_melt$couponUsed), 
                        kernel = "radial", gamma = svmfit_radial$gamma/2)
pred_radial0.5_T <- predict(svmfit_radial0.5, Feature$T_melt[, keep[c(34, 36:length(keep))]])
table_radial0.5_T <- table(pred_radial0.5_T, Feature$T_melt$couponUsed)
1 - sum(diag(table_radial0.5_T)) / sum(table_radial0.5_T)  # 0.1953
# validation error
pred_radial0.5 <- predict(svmfit_radial0.5, Feature$V_melt[, keep[c(34, 36:length(keep))]])
table_radial0.5 <- table(pred_radial0.5, Feature$V_melt$couponUsed)
1 - sum(diag(table_radial0.5)) / sum(table_radial0.5)  # 0.1874

# radial kernel with double default gamma
svmfit_radial2 <- svm(x = Feature$T_melt[, keep[c(34, 36:length(keep))]], 
                      y = as.factor(Feature$T_melt$couponUsed), 
                      kernel = "radial", gamma = svmfit_radial$gamma * 2)
pred_radial2_T <- predict(svmfit_radial2, Feature$T_melt[, keep[c(34, 36:length(keep))]])
table_radial2_T <- table(pred_radial2_T, Feature$T_melt$couponUsed)
1 - sum(diag(table_radial2_T)) / sum(table_radial2_T)  # 0.1522
# validation error
pred_radial2 <- predict(svmfit_radial2, Feature$V_melt[, keep[c(34, 36:length(keep))]])
table_radial2 <- table(pred_radial2, Feature$V_melt$couponUsed)
1 - sum(diag(table_radial2)) / sum(table_radial2)  # 0.1881


####################
### Feature ver0 ###
####################

Feature <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.rds")
dat_tr_x <- Feature$train$X
col_pred <- c(which(names(dat_tr_x) %in% c("couponsReceivedTime",
                                           "orderTimeTime",
                                           "TimeBtwnSentRec",
                                           "TimeBtwnRecExpire",
                                           "TimeBtwnRecOrder",
                                           "TimeBtwnOrderExpire",
                                           "price",
                                           "bPrpr_ratio",
                                           "nCoupon")),
              which(grepl("est",names(dat_tr_x))))

# linear kernel
svmfit_linear <- svm(x = Feature$train$X[, col_pred], 
                     y = as.factor(Feature$train$y$couponUsed), 
                     kernel = "linear")
# validation error
pred_linear <- predict(svmfit_linear, Feature$validation$X[, col_pred])
table_linear <- table(pred_linear, Feature$validation$y$couponUsed)
1 - sum(diag(table_linear)) / sum(table_linear)  # 0.1896

# polynomial kernel with default order 3
svmfit_polynomial <- svm(x = Feature$train$X[, col_pred], 
                         y = as.factor(Feature$train$y$couponUsed), 
                         kernel = "polynomial")
# validation error
pred_polynomial <- predict(svmfit_polynomial, Feature$validation$X[, col_pred])
table_polynomial <- table(pred_polynomial, Feature$validation$y$couponUsed)
1 - sum(diag(table_polynomial)) / sum(table_polynomial)  # 0.1911

# radial kernel with default gamma
svmfit_radial <- svm(x = Feature$train$X[, col_pred], 
                     y = as.factor(Feature$train$y$couponUsed), 
                     kernel = "radial", probability = TRUE)
# validation error
pred_radial <- predict(svmfit_radial, Feature$validation$X[, col_pred], probability = TRUE)
table_radial <- table(pred_radial, Feature$validation$y$couponUsed)
1 - sum(diag(table_radial)) / sum(table_radial)  # 0.1869

prob_radial <- attr(pred_radial, "probabilities")[, 1]
prob_radial1 <- prob_radial
prob_radial1[prob_radial < 0.7] = 1
prob_radial1[prob_radial >= 0.7] = 0
table(prob_radial1,Feature$validation$y$couponUsed) 

# radial kernel with half default gamma
svmfit_radial0.5 <- svm(x = Feature$train$X[, col_pred], 
                        y = as.factor(Feature$train$y$couponUsed), 
                        kernel = "radial", gamma = svmfit_radial$gamma/2)
# validation error
pred_radial0.5 <- predict(svmfit_radial0.5, Feature$validation$X[, col_pred])
table_radial0.5 <- table(pred_radial0.5, Feature$validation$y$couponUsed)
1 - sum(diag(table_radial0.5)) / sum(table_radial0.5)  # 0.1888

# radial kernel with double default gamma
svmfit_radial2 <- svm(x = Feature$train$X[, col_pred], 
                      y = as.factor(Feature$train$y$couponUsed), 
                      kernel = "radial", gamma = svmfit_radial$gamma * 2)
# validation error
pred_radial2 <- predict(svmfit_radial2, Feature$validation$X[, col_pred])
table_radial2 <- table(pred_radial2, Feature$validation$y$couponUsed)
1 - sum(diag(table_radial2)) / sum(table_radial2)  # 0.1879

# sigmoid kernel
svmfit_sigmoid <- svm(x = Feature$train$X[, col_pred], 
                      y = as.factor(Feature$train$y$couponUsed), 
                      kernel = "sigmoid", coef0 = -4)
# validation error
pred_sigmoid <- predict(svmfit_sigmoid, Feature$validation$X[, col_pred])
table_sigmoid <- table(pred_sigmoid, Feature$validation$y$couponUsed)
1 - sum(diag(table_sigmoid)) / sum(table_sigmoid)  # 0.1906

# loss
pred <- prob_radial1
cpn1 <- (1:(length(pred)/3)) * 3 - 2
cpn2 <- cpn1 + 1
cpn3 <- cpn1 + 2
sum(Loss_calculator(as.numeric(pred[cpn1]), Feature$validation$y$couponUsed[cpn1],
                    as.numeric(pred[cpn2]), Feature$validation$y$couponUsed[cpn2],
                    as.numeric(pred[cpn3]), Feature$validation$y$couponUsed[cpn3]))
