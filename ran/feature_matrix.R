######################
### feature matrix ###
######################

setwd("//Users/Ran/Dropbox/ISU/dmc2015/")
train <- read.table("data/raw_data/DMC_2015_orders_train.txt", head = T, sep = "|")
num_obs <- length(orderID)

# order ID
orderID <- train$orderID
# user ID
userID <- train$userID
# numbered user ID
userID_fac <- as.numeric(as.factor(userID))
# basket value
basket_value <- train$basketValue
# num of coupon used
num_couponUsed_perorder <- apply(train[, 29:31], 1, sum)
  
order_total_times <- order_num_times <- 
  ave_basket_value <- med_basket_value <- basket_value_over500 <- 
  num_order_couponUsed_peruser <- total_num_couponUsed <- 
  total_num_coupon1Used <- total_num_coupon2Used <- total_num_coupon3Used <- rep(0, num_obs)
## user info
userID_table <- table(userID_fac)
for (i in 1:length(unique(userID_fac))){
  total <- length(as.factor(userID)[which(userID_fac == i)])
  # total times of orders one user made
  order_total_times[which(userID_fac == i)] <- total
  # which time of order one user made
  order_num_times[which(userID_fac == i)] <- 1:total
  
  # average basket value
  ave_basket_value[which(userID_fac == i)] <- mean(basket_value[which(userID_fac == i)])
  
  # median basket value
  med_basket_value[which(userID_fac == i)] <- median(basket_value[which(userID_fac == i)])
  
  # num of basket value > 500
  basket_value_over500[which(userID_fac == i)] <- sum(basket_value[which(userID_fac == i)] > 500)
  
  # num of orders he used coupon
  num_order_couponUsed_peruser[which(userID_fac == i)] <- sum(num_couponUsed_perorder[which(userID_fac == i)] > 0)
  
  # total num of coupon he used (plus coupon 123)
  total_num_couponUsed[which(userID_fac == i)] <- sum(num_couponUsed_perorder[which(userID_fac == i)])
  total_num_coupon1Used[which(userID_fac == i)] <- sum(train$coupon1Used[which(userID_fac == i)])
  total_num_coupon2Used[which(userID_fac == i)] <- sum(train$coupon2Used[which(userID_fac == i)])
  total_num_coupon3Used[which(userID_fac == i)] <- sum(train$coupon3Used[which(userID_fac == i)])
}



feature_matrix <- data.frame(orderID, userID, userID_fac, order_num_times, order_total_times, 
                             basket_value, ave_basket_value, med_basket_value, basket_value_over500,
                             num_couponUsed_perorder, num_order_couponUsed_peruser, 
                             total_num_couponUsed, total_num_coupon1Used, total_num_coupon2Used, total_num_coupon3Used)
write.csv(feature_matrix, "ran/feature_matrix.csv", row.names = F)

# orderID: order ID
# userID: user ID
# userID_fac: numerical user ID
# order_num_times: which time is the order among all orders one user made
# order_total_times: total number of orders one user made
# basket_value: basket value of the order
# ave_basket_value: average basket value of the orders that one user made
# med_basket_value: median basket value of the orders that one user made
# basket_value_over500: how many orders basket value > 500 among all orders that one user made
# num_couponUsed_perorder: how many coupons are used in one order
# num_order_couponUsed_peruser: how many orders that one user made used any coupon
# total_num_couponUsed: how many coupons are used per user
# total_num_coupon1Used: how many coupon1s are used per user
# total_num_coupon2Used: how many coupon2s are used per user
# total_num_coupon3Used: how many coupon3s are used per user