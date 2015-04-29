train = read.csv("./data/mc2015_train_simple.csv",
    stringsAsFactors=FALSE, header=TRUE)
class = read.csv("./data/mc2015_test_simple.csv",
    stringsAsFactors=FALSE, header=TRUE)

## nCoupon[i]Used = the number of times a coupon in column i is
## used in the training set alone
## pCoupon[i]Used = the proportion of times a coupon in column i is
## used in the training set alone
trn = train[,c(5,13,21, 29:31)]

coupons = unlist(trn[,1:3], use.names=FALSE)
couponUsed = unlist(trn[,4:6], use.names=FALSE)
nCouponUsed = aggregate(couponUsed, by=list(coupons), sum)

couponsTab = table(coupons)
trn$nCoupon1 = unname(couponsTab[as.character(trn$couponID1)])
trn$nCoupon2 = unname(couponsTab[as.character(trn$couponID2)])
trn$nCoupon3 = unname(couponsTab[as.character(trn$couponID3)])

trn$nCoupon1Used = nCouponUsed[trn$couponID1, 2]
trn$nCoupon2Used = nCouponUsed[trn$couponID2, 2]
trn$nCoupon3Used = nCouponUsed[trn$couponID3, 2]

trn$pCoupon1Used = trn$nCoupon1Used / trn$nCoupon1
trn$pCoupon2Used = trn$nCoupon2Used / trn$nCoupon2
trn$pCoupon3Used = trn$nCoupon3Used / trn$nCoupon3

## nCoup[i]Col[j]Used = the number of times a coupon that from
## column [i] appeared in column[j] is used in the training set alone
## pCoup[i]Col[j]Used = the proportion of times a coupon that from
## column [i] appeared in column[j] is used in the training set alone

## Some code below is modified from pete's folder: 03_features.R
# ===================================================================
coupTab1 <- table(trn$couponID1)
coupTab2 <- table(trn$couponID2)
coupTab3 <- table(trn$couponID3)

# Training set
trn$nCoup1Col1 <- unname(coupTab1[as.character(trn$couponID1)])
trn$nCoup1Col2 <- unname(coupTab2[as.character(trn$couponID1)])
trn$nCoup1Col3 <- unname(coupTab3[as.character(trn$couponID1)])

trn$nCoup2Col1 <- unname(coupTab1[as.character(trn$couponID2)])
trn$nCoup2Col2 <- unname(coupTab2[as.character(trn$couponID2)])
trn$nCoup2Col3 <- unname(coupTab3[as.character(trn$couponID2)])

trn$nCoup3Col1 <- unname(coupTab1[as.character(trn$couponID3)])
trn$nCoup3Col2 <- unname(coupTab2[as.character(trn$couponID3)])
trn$nCoup3Col3 <- unname(coupTab3[as.character(trn$couponID3)])

trn[is.na(trn)] <- 0

## Coupon used number for each column
nCoup1Used = aggregate(trn$coupon1Used, by=list(trn$couponID1), sum)
nCoup2Used = aggregate(trn$coupon2Used, by=list(trn$couponID2), sum)
nCoup3Used = aggregate(trn$coupon3Used, by=list(trn$couponID3), sum)

index = match(trn$couponID1, nCoup1Used$Group.1)
trn$nCoup1Col1Used = ifelse(is.na(index), 0, nCoup1Used[index, 2])
index = match(trn$couponID1, nCoup2Used$Group.1)
trn$nCoup1Col2Used = ifelse(is.na(index), 0, nCoup2Used[index, 2])
index = match(trn$couponID1, nCoup3Used$Group.1)
trn$nCoup1Col3Used = ifelse(is.na(index), 0, nCoup3Used[index, 2])

index = match(trn$couponID2, nCoup1Used$Group.1)
trn$nCoup2Col1Used = ifelse(is.na(index), 0, nCoup1Used[index, 2])
index = match(trn$couponID2, nCoup2Used$Group.1)
trn$nCoup2Col2Used = ifelse(is.na(index), 0, nCoup2Used[index, 2])
index = match(trn$couponID2, nCoup3Used$Group.1)
trn$nCoup2Col3Used = ifelse(is.na(index), 0, nCoup3Used[index, 2])

index = match(trn$couponID3, nCoup1Used$Group.1)
trn$nCoup3Col1Used = ifelse(is.na(index), 0, nCoup1Used[index, 2])
index = match(trn$couponID3, nCoup2Used$Group.1)
trn$nCoup3Col2Used = ifelse(is.na(index), 0, nCoup2Used[index, 2])
index = match(trn$couponID3, nCoup3Used$Group.1)
trn$nCoup3Col3Used = ifelse(is.na(index), 0, nCoup3Used[index, 2])

## Result verification
identical(as.integer(trn$nCoup1Col1Used + trn$nCoup1Col2Used + trn$nCoup1Col3Used), trn$nCoupon1Used)
identical(as.integer(trn$nCoup2Col1Used + trn$nCoup2Col2Used + trn$nCoup2Col3Used), trn$nCoupon2Used)
identical(as.integer(trn$nCoup3Col1Used + trn$nCoup3Col2Used + trn$nCoup3Col3Used), trn$nCoupon3Used)

## proportion
trn$pCoup1Col1Used = trn$nCoup1Col1Used / trn$nCoupon1Used
trn$pCoup1Col2Used = trn$nCoup1Col2Used / trn$nCoupon1Used
trn$pCoup1Col3Used = trn$nCoup1Col3Used / trn$nCoupon1Used

trn$pCoup2Col1Used = trn$nCoup2Col1Used / trn$nCoupon2Used
trn$pCoup2Col2Used = trn$nCoup2Col2Used / trn$nCoupon2Used
trn$pCoup2Col3Used = trn$nCoup2Col3Used / trn$nCoupon2Used

trn$pCoup3Col1Used = trn$nCoup3Col1Used / trn$nCoupon3Used
trn$pCoup3Col2Used = trn$nCoup3Col2Used / trn$nCoupon3Used
trn$pCoup3Col3Used = trn$nCoup3Col3Used / trn$nCoupon3Used

#### mapping for class data
## ==========================
tst = class[,c(1, 5,13,21)]

## column1
c1 = trn[,c("couponID1", "nCoupon1Used", "pCoupon1Used",
    "nCoup1Col1Used", "nCoup1Col2Used", "nCoup1Col3Used",
    "pCoup1Col1Used", "pCoup1Col2Used", "pCoup1Col3Used")]
c11 = unique(c1)

tst1 = tst[,1:2]
tmp = merge(tst1, c11, by="couponID1", incomparables = NA, all.x = TRUE)
tst1 = tmp[order(tmp$orderID),]

## column2
c2 = trn[,c("couponID2", "nCoupon2Used", "pCoupon2Used",
    "nCoup2Col1Used", "nCoup2Col2Used", "nCoup2Col3Used",
    "pCoup2Col1Used", "pCoup2Col2Used", "pCoup2Col3Used")]
c22 = unique(c2)

tst2 = tst[,c(1, 3)]
tmp = merge(tst2, c22, by="couponID2", incomparables = NA, all.x = TRUE)
tst2 = tmp[order(tmp$orderID),]

## column3
c3 = trn[,c("couponID3", "nCoupon3Used", "pCoupon3Used",
    "nCoup3Col1Used", "nCoup3Col2Used", "nCoup3Col3Used",
    "pCoup3Col1Used", "pCoup3Col2Used", "pCoup3Col3Used")]
c33 = unique(c3)

tst3 = tst[,c(1, 4)]
tmp = merge(tst3, c33, by="couponID3", incomparables = NA, all.x = TRUE)
tst3 = tmp[order(tmp$orderID),]

## Merge all together
oTst = cbind(orderID = tst$orderID, tst1[, -c(1:2)], tst2[, -c(1:2)], tst3[, -c(1:2)])
oTst = oTst[,c(1,2,10,18, 3, 11, 19, 4:6, 12:14, 20:22, 7:9, 15:17, 23:25)]

#### export
## ========
oTrn = trn[,-c(1:9, 16:24)]
oTrn = cbind(orderID = train$orderID, oTrn)
write.csv(oTrn, "./feature/couponUsed_train.csv", quote = FALSE, row.names=FALSE, na="")
write.csv(oTst, "./feature/couponUsed_class.csv", quote = FALSE, row.names=FALSE, na="")

## TEST
aa = read.csv("./feature/couponUsed_train.csv", header=TRUE)
bb = read.csv("./feature/couponUsed_class.csv", header=TRUE)
