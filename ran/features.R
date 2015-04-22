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


###########################
### reward distribution ###
###########################

nlevels(as.factor(train_melt$reward))
levels(as.factor(train_melt$reward))

# reward histogram vs. coupon number
ggplot(train_melt, aes(x = train_melt$reward, fill = as.factor(train_melt$coupon_num))) +
  geom_histogram(binwidth = .2, position = "dodge") 
# reward histogram vs. whether the coupon is used
ggplot(train_melt, aes(x = train_melt$reward, fill = as.factor(train_melt$couponUsed))) +
  geom_histogram(binwidth = .2, position = "fill") 


###################################################
### couponID == brand, producGroup, categoryID? ###
###################################################

max_coupon <- max(table(train_melt$couponID))
max_couponID <- names(table(train_melt$couponID)[table(train_melt$couponID) == max_coupon])
table(train_melt$couponUsed[train_melt$couponID == max_couponID])

nlevels(as.factor(train_melt$coupon_num[train_melt$couponID == "89d62b666d585f03f262b1f6a6abdd2c"]))

# number of categories == number of columns coupon appear?
a <- rep(0, length(train_melt$userID))
for(i in 1:length(train_melt$userID)){
  a[i] <- (length(strsplit(as.character(train_melt$categoryIDs), split = ",")[[i]]) == 
             nlevels(as.factor(train_melt$coupon_num[train_melt$couponID == train_melt$couponID[i]])))
}

nlevels(train$couponID1)

couponUsed_table <- table(train_melt$couponID, train_melt$couponUsed)
couponUsed_table_melt <- melt(couponUsed_table)
hist(couponUsed_table[, 2], breaks = 100)
ggplot(couponUsed_table_melt, aes(x = value, fill = as.factor(Var2))) +
  geom_histogram(binwidth = 1, position = "fill") 


######################################
### how many times user made order ###
######################################

userID_table <- table(train$userID)
summary(as.numeric(userID_table))
hist(userID_table)
names(which(userID_table == 30))  # user ordered 30 times
train[train$userID == "2bab1752b217fdd3704199dead8fa372", ]


###################
### Text Mining ###
###################

attach(train_melt)
orderTime0 <- "2015-01-05 23:59:59"
train_melt$batch <- factor(ceiling(as.numeric(difftime(ymd_hms(orderTime), ymd_hms(orderTime0), unit = "weeks"))))
ggplot() + 
  geom_point(aes(x = ymd_hms(couponsReceived), y = ymd_hms(orderTime)), col = batch) +
  xlab("coupon_received_time") + ylab("order_time")

batch_num <- c(0, as.numeric(table(train_melt$batch)))
# couponID & batch count matrix
couponID_unique <- unique(couponID)
coupon_batch <- matrix(0, length(couponID_unique), length(batch_num)-1)
for (i in 1:length(couponID_unique)){
  for (j in 2:length(batch_num)){
    coupon_batch[i, j-1] <- sum(couponID[(batch_num[j-1] + 1):batch_num[j]] == couponID_unique[i])
  }
  if (i%%100 == 0) {print(i)}
}
colnames(coupon_batch) <- 1:9
rownames(coupon_batch) <- couponID_unique

# userID & batch count matrix
userID_unique <- unique(userID)
user_batch <- matrix(0, length(userID_unique), length(batch_num)-1)
for (i in 1:length(userID_unique)){
  for (j in 2:length(batch_num)){
    user_batch[i, j-1] <- sum(userID[(batch_num[j-1] + 1):batch_num[j]] == userID_unique[i])
  }
  if (i%%100 == 0) {print(i)}
}
colnames(user_batch) <- 1:9
rownames(user_batch) <- userID_unique

library(igraph)
termMatrix <- coupon_batch %*% t(coupon_batch)
# build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted = T, mode = "undirected")
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- NULL
V(g)$degree <- degree(g)

set.seed(2015)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout = layout1)

eigen(termMatrix)$va[1:10]
