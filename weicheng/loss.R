library(dplyr)
#dat = readRDS("../data/clean_data/clean_data_v1.rds")
dat <- readRDS("../data/featureMatrix/HTVset1.rds")
trn <- rbind(dat$H, dat$T)
trn <- subset(trn, basketValue <= 5000)
tst <- dat$V
bsktLoss(trn, tst)
hist(trn$basketValue, 5000)
trn0 = dat$train
trn = trn0[trn0$basketValue<100000,]
trn = trn[order(trn$userID),]

trn1 = trn[as.numeric(trn$batchID)<9,]
tst1 = trn[as.numeric(trn$batchID)==9,]

Loss_calculator(0,0,0,0,0,0,272.77,tst$basketValue)

bsktLoss <- function(trn, tst, FUN=mean){
    trn = trn[order(trn$userID),]
    ## basketValues for users who occured in the training set
    bsv.users = aggregate(basketValue~userID, FUN, data=trn)
    ## basketValues for users who didn't occur in  the training set
    uid.rle = rle(as.character(trn$userID))
    idx = c(1, cumsum(uid.rle$lengths[-length(uid.rle$lengths)]) +1)
    ## basketValues the first time users purchase
    bsktValueFirstTime = trn$basketValue[idx]
    tst1 = tst[, c("userID"), drop=FALSE]
    tst1 = left_join(tst1, bsv.users, by = "userID")
    bsktValueFirstTime.stat = FUN(bsktValueFirstTime)
    tst1$basketValue[is.na(tst1$basketValue)] = bsktValueFirstTime.stat
    ## loss
    sum((tst1$basketValue - tst$basketValue)^2)/mean(tst$basketValue)^2
}

bsktLoss(trn1, tst1, median)
bsktLoss(trn1, tst1)






#############
dat <- readRDS("../data/featureMatrix/HTVset1.rds")
trn <- rbind(dat$H, dat$T)
tst <- dat$V
source("lossFun.R")
cpnLoss1 <- function(trn, tst, FUN=mean){
    prop_cpn1 = aggregate(coupon1Used~couponID1, FUN=mean,  data = trn)
    prop_cpn2 = aggregate(coupon2Used~couponID2, FUN=mean,  data = trn)
    prop_cpn3 = aggregate(coupon3Used~couponID3, FUN=mean,  data = trn)

    tst0 = tst[, c("coupon1Used", "coupon2Used", "coupon3Used")]
    tst0 = c(t(as.matrix(tst0)))
    tst1 = tst[, c("couponID1", "couponID2", "couponID3"), drop=FALSE]
    tst1 = left_join(tst1, prop_cpn1, by = "couponID1")
    tst1 = left_join(tst1, prop_cpn2, by = "couponID2")
    tst1 = left_join(tst1, prop_cpn3, by = "couponID3")
    tst1[is.na(tst1)] = 0.2
    tst1 = c(t(as.matrix(tst1[, 4:6])))
    lossFun(tst0, tst1)
}

cpnLoss1(trn, tst)


dat <- readRDS("../data/featureMatrix/HTVset1.rds")
trn <- rbind(dat$H, dat$T)
tst <- dat$V

cpnLoss2 <- function(trn, tst, FUN=mean){
    ## wide to long
    cpnIDs = trn[, c("couponID1", "couponID2", "couponID3")]
    for(i in 1:3)
        cpnIDs[,i] = as.character(cpnIDs[,i])

    cpnIDsLong = rep("", 3*nrow(cpnIDs))
    idx1 = seq(1, length(cpnIDsLong), by = 3)
    idx2 = seq(2, length(cpnIDsLong), by = 3)
    idx3 = seq(3, length(cpnIDsLong), by = 3)
    cpnIDsLong[idx1] = cpnIDs[, 1]
    cpnIDsLong[idx2] = cpnIDs[, 2]
    cpnIDsLong[idx3] = cpnIDs[, 3]

    cpnUsedLong = integer(3*nrow(cpnIDs))
    cpnUsed = trn[, c("coupon1Used", "coupon2Used", "coupon3Used")]
    cpnUsedLong[idx1] = cpnUsed[, 1]
    cpnUsedLong[idx2] = cpnUsed[, 2]
    cpnUsedLong[idx3] = cpnUsed[, 3]

    trn1 = data.frame(couponID = cpnIDsLong, couponUsed = cpnUsedLong)
    ## END wide to long

    prop_cpn = aggregate(couponUsed ~ couponID, FUN=mean,  data = trn1)
    prop_cpn$couponID = as.character(prop_cpn$couponID)

    tst0 = tst[, c("coupon1Used", "coupon2Used", "coupon3Used")]
    tst0 = c(t(as.matrix(tst0)))

    cpnIDs.tst = tst[, c("couponID1", "couponID2", "couponID3")]
    for(i in 1:3)
        cpnIDs.tst[,i] = as.character(cpnIDs.tst[,i])

    cpnIDsLong.tst = rep("", 3*nrow(cpnIDs.tst))
    idx1 = seq(1, length(cpnIDsLong.tst), by = 3)
    idx2 = seq(2, length(cpnIDsLong.tst), by = 3)
    idx3 = seq(3, length(cpnIDsLong.tst), by = 3)
    cpnIDsLong.tst[idx1] = cpnIDs.tst[, 1]
    cpnIDsLong.tst[idx2] = cpnIDs.tst[, 2]
    cpnIDsLong.tst[idx3] = cpnIDs.tst[, 3]

    tst1 = data.frame(couponID = cpnIDsLong.tst)
    tst1$couponID = as.character(tst1$couponID)
    tst1 = left_join(tst1, prop_cpn, by = "couponID")
    tst1[is.na(tst1)] = 0.2
    lossFun(tst0, tst1$couponUsed)
}

cpnLoss2(trn, tst)
