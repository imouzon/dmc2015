train <- read.csv("/Users/epwalsh/GitHub/dmc2015/data/featureMatrix/train_ver0.0.csv")
class <- read.csv("/Users/epwalsh/GitHub/dmc2015/data/featureMatrix/class_ver0.0.csv")
# feat_mat <- readRDS("/Users/epwalsh/GitHub/dmc2015/features/feature_files/batchInfo_test.rds")

# Current features
nCoupTrain <- read.csv("~/GitHub/dmc2015/features/feature_files/nCoupTrain.csv")
nCoupClass <- read.csv("~/GitHub/dmc2015/features/feature_files/nCoupClass.csv")

# Export to features folder as csv
write.csv(nCoupTrain, "~/GitHub/dmc2015/features/feature_files/nCoupTrain.csv", quote = F, na = "", row.names = F)
write.csv(nCoupClass, "~/GitHub/dmc2015/features/feature_files/nCoupClass.csv", quote = F, na = "", row.names = F)


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


