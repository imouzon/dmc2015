train <- read.csv("/Users/epwalsh/GitHub/dmc2015/data/featureMatrix/train_ver0.0.csv")
class <- read.csv("/Users/epwalsh/GitHub/dmc2015/data/featureMatrix/class_ver0.0.csv")
# feat_mat <- readRDS("/Users/epwalsh/GitHub/dmc2015/features/feature_files/batchInfo_test.rds")

# Current features
nCoupTrain <- read.csv("~/GitHub/dmc2015/features/feature_files/nCoupTrain.csv")
nCoupClass <- read.csv("~/GitHub/dmc2015/features/feature_files/nCoupClass.csv")

# Export to features folder as csv
write.csv(nCoupTrain, "~/GitHub/dmc2015/features/feature_files/nCoupTrain.csv", quote = F, na = "", row.names = F)
write.csv(nCoupClass, "~/GitHub/dmc2015/features/feature_files/nCoupClass.csv", quote = F, na = "", row.names = F)

library(reshape2)
library(dplyr)
library(lubridate)

train$orderTime <- ymd_hms(train$orderTime)
class$orderTime <- ymd_hms(class$orderTime)

# Combine train and test set without response variables
d <- rbind(train[, -c(29:32)], class[, -c(29:32)])


# How many times each coupon seen across entire dataset
# ===================================================================
allCoups <- c(as.character(d$couponID1), 
              as.character(d$couponID2), 
              as.character(d$couponID3))
coupTab <- table(allCoups)
d$nCoupon1 <- unname(coupTab[as.character(d$couponID1)])
d$nCoupon2 <- unname(coupTab[as.character(d$couponID2)])
d$nCoupon3 <- unname(coupTab[as.character(d$couponID3)])

nCoupTrain <- d[1:nrow(train), c(1, 46:48)]
nCoupClass <- d[-c(1:nrow(train)), c(1, 46:48)]


## How many times each coupon seen in each column
# ===================================================================
coupTab1 <- table(as.character(d$couponID1))
coupTab2 <- table(as.character(d$couponID2))
coupTab3 <- table(as.character(d$couponID3))

# Training set
nCoupTrain$nCoup1Col1 <- unname(coupTab1[as.character(train$couponID1)])
nCoupTrain$nCoup1Col2 <- unname(coupTab2[as.character(train$couponID1)])
nCoupTrain$nCoup1Col3 <- unname(coupTab3[as.character(train$couponID1)])

nCoupTrain$nCoup2Col1 <- unname(coupTab1[as.character(train$couponID2)])
nCoupTrain$nCoup2Col2 <- unname(coupTab2[as.character(train$couponID2)])
nCoupTrain$nCoup2Col3 <- unname(coupTab3[as.character(train$couponID2)])

nCoupTrain$nCoup3Col1 <- unname(coupTab1[as.character(train$couponID3)])
nCoupTrain$nCoup3Col2 <- unname(coupTab2[as.character(train$couponID3)])
nCoupTrain$nCoup3Col3 <- unname(coupTab3[as.character(train$couponID3)])

# Test set
nCoupClass$nCoup1Col1 <- unname(coupTab1[as.character(class$couponID1)])
nCoupClass$nCoup1Col2 <- unname(coupTab2[as.character(class$couponID1)])
nCoupClass$nCoup1Col3 <- unname(coupTab3[as.character(class$couponID1)])

nCoupClass$nCoup2Col1 <- unname(coupTab1[as.character(class$couponID2)])
nCoupClass$nCoup2Col2 <- unname(coupTab2[as.character(class$couponID2)])
nCoupClass$nCoup2Col3 <- unname(coupTab3[as.character(class$couponID2)])

nCoupClass$nCoup3Col1 <- unname(coupTab1[as.character(class$couponID3)])
nCoupClass$nCoup3Col2 <- unname(coupTab2[as.character(class$couponID3)])
nCoupClass$nCoup3Col3 <- unname(coupTab3[as.character(class$couponID3)])

# Proportion of times each coupon seen in each column
# ===================================================================

# Training set
nCoupTrain$pCoup1Col1 <- nCoupTrain$nCoup1Col1 / nCoupTrain$nCoupon1
nCoupTrain$pCoup1Col2 <- nCoupTrain$nCoup1Col2 / nCoupTrain$nCoupon1
nCoupTrain$pCoup1Col3 <- nCoupTrain$nCoup1Col3 / nCoupTrain$nCoupon1

nCoupTrain$pCoup2Col1 <- nCoupTrain$nCoup2Col1 / nCoupTrain$nCoupon2
nCoupTrain$pCoup2Col2 <- nCoupTrain$nCoup2Col2 / nCoupTrain$nCoupon2
nCoupTrain$pCoup2Col3 <- nCoupTrain$nCoup2Col3 / nCoupTrain$nCoupon2

nCoupTrain$pCoup3Col1 <- nCoupTrain$nCoup3Col1 / nCoupTrain$nCoupon3
nCoupTrain$pCoup3Col2 <- nCoupTrain$nCoup3Col2 / nCoupTrain$nCoupon3
nCoupTrain$pCoup3Col3 <- nCoupTrain$nCoup3Col3 / nCoupTrain$nCoupon3

# Test set
nCoupClass$pCoup1Col1 <- nCoupClass$nCoup1Col1 / nCoupClass$nCoupon1
nCoupClass$pCoup1Col2 <- nCoupClass$nCoup1Col2 / nCoupClass$nCoupon1
nCoupClass$pCoup1Col3 <- nCoupClass$nCoup1Col3 / nCoupClass$nCoupon1

nCoupClass$pCoup2Col1 <- nCoupClass$nCoup2Col1 / nCoupClass$nCoupon2
nCoupClass$pCoup2Col2 <- nCoupClass$nCoup2Col2 / nCoupClass$nCoupon2
nCoupClass$pCoup2Col3 <- nCoupClass$nCoup2Col3 / nCoupClass$nCoupon2

nCoupClass$pCoup3Col1 <- nCoupClass$nCoup3Col1 / nCoupClass$nCoupon3
nCoupClass$pCoup3Col2 <- nCoupClass$nCoup3Col2 / nCoupClass$nCoupon3
nCoupClass$pCoup3Col3 <- nCoupClass$nCoup3Col3 / nCoupClass$nCoupon3

# nCoupTrain[is.na(nCoupTrain)] <- 0
# nCoupClass[is.na(nCoupClass)] <- 0

# Number of times each coupon appears in the current batch nCoupBatch
# ===================================================================
class <- read.csv("~/GitHub/dmc2015/data/featureMatrix/class_ver1.1.csv")
train <- read.csv("~/GitHub/dmc2015/data/featureMatrix/train_ver1.1.csv")
d <- rbind(train[, -c(29:32)], class[, -c(29:32)])

allCoups <- melt(d, id = c("orderID", "batchID"), c(5, 13, 21),
                 variable.name = "couponCol",
                 value.name = "couponID")

batchCounts <- allCoups %>% group_by(batchID, couponID) %>%
  summarize(counts = length(orderID))

allCoups2 <- inner_join(allCoups, batchCounts)

batchCountsW <- allCoups2[,-4] %>%
  dcast(orderID + batchID ~ couponCol, value.var = "counts")

names(batchCountsW)[3:5] <- c("nCoup1Batch", "nCoup2Batch", "nCoup3Batch")

nCoupTrain <- cbind(nCoupTrain, batchCountsW[1:nrow(nCoupTrain), 3:5])
nCoupClass <- cbind(nCoupClass, batchCountsW[nCoupClass$orderID, 3:5])



# First time the coupon was seen: firstTimeCoupRec
# ===================================================================
d$couponsReceived <- ymd_hms(d$couponsReceived)
dMelt <- d %>% melt(id = c("orderID", "couponsReceived"), c(5, 13, 21),
                    variable.name = "couponCol",
                    value.name = "couponID")

coupRecTimes <- dMelt %>% group_by(couponID) %>%
  summarize(firstTime = min(couponsReceived))

dMelt <- inner_join(dMelt, coupRecTimes, by = "couponID")

dMelt$firstTime <- as.character(dMelt$firstTime)
dWide <- dMelt %>% select(c(1, 3, 5)) %>%
  dcast(orderID ~ couponCol, value.var = "firstTime") 

names(dWide)[2:4] <- c("firstTimeCoup1Rec",
                       "firstTimeCoup2Rec", 
                       "firstTimeCoup3Rec")

nCoupTrain <- cbind(nCoupTrain, dWide[nCoupTrain$orderID, 2:4])
nCoupClass <- cbind(nCoupClass, dWide[nCoupClass$orderID, 2:4])


# First time the coupon was used: firstTimeCoupUsed
# ===================================================================

# ? Do we even need this?


# Coupon categories
# ===================================================================

# Read in the melted training and test data
trainM <- read.csv("~/GitHub/dmc2015/data/clean_data/melted_train_simple_name.csv")
classM <- read.csv("~/GitHub/dmc2015/data/clean_data/melted_test_simple_name.csv")

d <- rbind(trainM, classM)

rm(list = c("trainM", "classM"))

d$categoryIDs <- as.character(d$categoryIDs)

catIDs <- sapply(d$categoryIDs, strsplit, split = ":")

outCats <- matrix(rep(NA, 5*nrow(d)), ncol = 5)

for (i in 1:nrow(d)) {
  cats <- unlist(catIDs[[i]])
  for (j in 1:length(cats)) {
    outCats[i,j] <- cats[j]
  }
}

colnames(outCats) <- c("cat1", "cat2", "cat3", "cat4", "cat5")
d <- cbind(d, outCats)
d$cat1 <- as.character(d$cat1)
d$cat2 <- as.character(d$cat2)
d$cat3 <- as.character(d$cat3)
d$cat4 <- as.character(d$cat4)
d$cat5 <- as.character(d$cat5)
catM <- melt(d, id = c("orderID", "couponCol"), 16:20)
catI <- dcast(catM, orderID + couponCol ~ value, length, fill = 0)

d <- d[, -c(16:20)]

d <- cbind(d, catI[,-c(1:2)])
d <- d[, -ncol(d)]
rm(list=c("catM", "catI", "catIDs", "trainM", "classM"))

# We now have indicator columns for each unique category ID


# Brand and premium product
# ===================================================================
premSum <- d %>% group_by(couponID) %>%
  summarize(n = length(premiumProduct),
            prop = mean(premiumProduct))

d[1:nrow(trainM),] %>% group_by(brand) %>%
  summarize(propPrem = mean(premiumProduct),
            bVal = mean(basketValue),
            propUsed = mean(couponUsed)) %>%
  ggplot(aes(x = propPrem, y = bVal)) + geom_point()

d %>% group_by(brand) %>%
  summarize(propPrem = mean(premiumProduct),
            count = length(premiumProduct))
