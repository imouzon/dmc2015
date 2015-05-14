dat = readRDS("../data/clean_data/clean_data_v1.rds")
trn0 = dat$train
trn = trn0[trn0$basketValue<100000,]

trn1 = trn[as.numeric(trn$batchID)<9,]
tst1 = trn[as.numeric(trn$batchID)==9,]

bsv.users = aggregate(basketValue~userID, mean, data=trn1)
## unique users
users = unique(trn1$userID)
userlist = list()
basketValueUnique = c()

for(i in 1:length(users)){
    userlist[[i]]=trn1[trn1$userID==users[i],]
    basketValueUnique = c(basketValueUnique, userlist[[i]]$basketValue[1])
}


tst2 = tst1[, c("userID"), drop=FALSE]
tst2 = left_join(tst2, bsv.users, by = "userID")

basketValueUnique.mean = mean(basketValueUnique)
tst2$basketValue[is.na(tst2$basketValue)] = basketValueUnique.mean

sum((tst2$basketValue - tst1$basketValue)^2)/mean(tst1$basketValue)^2
## 503.8027


## median
bsv.users = aggregate(basketValue~userID, median, data=trn1)
## unique users
users = unique(trn1$userID)
userlist = list()
basketValueUnique = c()

for(i in 1:length(users)){
    userlist[[i]]=trn1[trn1$userID==users[i],]
    basketValueUnique = c(basketValueUnique, userlist[[i]]$basketValue[1])
}

tst2 = tst1[, c("userID"), drop=FALSE]
tst2 = left_join(tst2, bsv.users, by = "userID")

basketValueUnique.median = median(basketValueUnique)
tst2$basketValue[is.na(tst2$basketValue)] = basketValueUnique.median

sum((tst2$basketValue - tst1$basketValue)^2)/mean(tst1$basketValue)^2
## 577.9097
