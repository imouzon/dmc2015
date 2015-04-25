train <- read.csv("/Users/epwalsh/GitHub/dmc2015/data/featureMatrix/train_ver0.0.csv")
class <- read.csv("/Users/epwalsh/GitHub/dmc2015/data/featureMatrix/class_ver0.0.csv")
# feat_mat <- readRDS("/Users/epwalsh/GitHub/dmc2015/features/feature_files/batchInfo_test.rds")

library(lubridate)

train$orderTime <- ymd_hms(train$orderTime)
class$orderTime <- ymd_hms(class$orderTime)

# Combine train and test set without usage response variables
d <- rbind(train[, -c(29:32)], class[, -c(29:32)])

# How many times each coupon seen across entire dataset
allCoups <- c(as.character(d$couponID1), 
							as.character(d$couponID2), 
							as.character(d$couponID3))
coupTab <- table(allCoups)
d$nCoupon1 <- unname(coupTab[as.character(d$couponID1)])
d$nCoupon2 <- unname(coupTab[as.character(d$couponID2)])
d$nCoupon3 <- unname(coupTab[as.character(d$couponID3)])

nCoupTrain <- d[1:nrow(train), c(1, 46:48)]
nCoupClass <- d[-c(1:nrow(train)), c(1, 46:48)]
# Export to features folder as csv
write.csv(nCoupTrain, "~/GitHub/dmc2015/features/feature_files/nCoupTrain.csv", quote = F, na = "", row.names = F)
write.csv(nCoupClass, "~/GitHub/dmc2015/features/feature_files/nCoupClass.csv", quote = F, na = "", row.names = F)

# How many times coupon seen in col 1


# How many times coupon seen in col 2


# How many times coupon seen in col 3



