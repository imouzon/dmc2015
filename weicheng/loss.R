library(dplyr)
dat = readRDS("../data/clean_data/clean_data_v1.rds")
trn0 = dat$train
trn = trn0[trn0$basketValue<100000,]
trn = trn[order(trn$userID),]

trn1 = trn[as.numeric(trn$batchID)<9,]
tst1 = trn[as.numeric(trn$batchID)==9,]

bsktLoss <- function(trn, tst, FUN=mean){
    trn = trn[order(trn$userID),]
    ## basketValues for users who occured in the training set
    bsv.users = aggregate(basketValue~userID, FUN, data=trn)
    ## basketValues for users who didn't occure in  the training set
    uid.rle = rle(trn$userID)
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
