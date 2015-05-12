library(dplyr)
library(missForest)

dset1 = readRDS("../data/featureMatrix/HTVset1.rds")

str(dset1)

h = dset1$orderids$h
v = dset1$orderids$v
t = 1:6053
t = t[-c(h, v)]
length(h); length(v); length(t)

H = dset1$H
H1 = H[,c("orderID", "couponID1", "couponID2", "couponID3", "coupon1Used", "coupon2Used", "coupon3Used")]

V = dset1$V
T = dset1$T
C = dset1$C

## nCoupon[i]Used = the number of times a coupon in column i is
## used in the training set alone
## pCoupon[i]Used = the proportion of times a coupon in column i is
## used in the training set alone
coupUse <- function(trn){
    trn$couponID1 = as.character(trn$couponID1)
    trn$couponID2 = as.character(trn$couponID2)
    trn$couponID3 = as.character(trn$couponID3)

    coupons = as.character(unlist(trn[,2:4], use.names=FALSE))
    couponUsed = unlist(trn[,5:7], use.names=FALSE)
    nCouponUsed = aggregate(couponUsed, by=list(coupons), sum)

    couponsTab = table(coupons)
    nCoupon1 = unname(couponsTab[trn$couponID1])
    nCoupon2 = unname(couponsTab[trn$couponID2])
    nCoupon3 = unname(couponsTab[trn$couponID3])

    index = match(trn$couponID1, nCouponUsed$Group.1)
    trn$nCoupon1Used = ifelse(is.na(index), 0, nCouponUsed[index, 2])
    index = match(trn$couponID2, nCouponUsed$Group.1)
    trn$nCoupon2Used = ifelse(is.na(index), 0, nCouponUsed[index, 2])
    index = match(trn$couponID3, nCouponUsed$Group.1)
    trn$nCoupon3Used = ifelse(is.na(index), 0, nCouponUsed[index, 2])

    trn$pCoupon1Used = trn$nCoupon1Used / nCoupon1
    trn$pCoupon2Used = trn$nCoupon2Used / nCoupon2
    trn$pCoupon3Used = trn$nCoupon3Used / nCoupon3

    ## nCoup[i]Col[j]Used = the number of times a coupon that from
    ## column [i] appeared in column[j] is used in the training set alone
    ## pCoup[i]Col[j]Used = the proportion of times a coupon that from
    ## column [i] appeared in column[j] is used in the training set alone

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

    ## proportion
    trn$pCoup1Col1Used = trn$nCoup1Col1Used / nCoupon1
    trn$pCoup1Col2Used = trn$nCoup1Col2Used / nCoupon1
    trn$pCoup1Col3Used = trn$nCoup1Col3Used / nCoupon1

    trn$pCoup2Col1Used = trn$nCoup2Col1Used / nCoupon2
    trn$pCoup2Col2Used = trn$nCoup2Col2Used / nCoupon2
    trn$pCoup2Col3Used = trn$nCoup2Col3Used / nCoupon2

    trn$pCoup3Col1Used = trn$nCoup3Col1Used / nCoupon3
    trn$pCoup3Col2Used = trn$nCoup3Col2Used / nCoupon3
    trn$pCoup3Col3Used = trn$nCoup3Col3Used / nCoupon3


    ## number/proportion of the coupon been used in the i-th column
    coupTab1 <- table(trn$couponID1)
    coupTab2 <- table(trn$couponID2)
    coupTab3 <- table(trn$couponID3)

    nCoupon1in1 = unname(coupTab1[trn$couponID1])
    nCoupon2in2 = unname(coupTab2[trn$couponID2])
    nCoupon3in3 = unname(coupTab3[trn$couponID3])

    trn$pCoupon1in1 = trn$nCoup1Col1Used / nCoupon1in1
    trn$pCoupon2in2 = trn$nCoup2Col2Used / nCoupon2in2
    trn$pCoupon3in3 = trn$nCoup3Col3Used / nCoupon3in3
    trn
}

## Test
trn = coupUse(H1)

#### mapping for class data
## ==========================
## column1
c1 = trn[,c("couponID1", "nCoupon1Used", "pCoupon1Used",
    "nCoup1Col1Used", "nCoup1Col2Used", "nCoup1Col3Used",
    "pCoup1Col1Used", "pCoup1Col2Used", "pCoup1Col3Used",
    "pCoupon1in1")]
c11 = unique(c1)

## column2
c2 = trn[,c("couponID2", "nCoupon2Used", "pCoupon2Used",
    "nCoup2Col1Used", "nCoup2Col2Used", "nCoup2Col3Used",
    "pCoup2Col1Used", "pCoup2Col2Used", "pCoup2Col3Used",
    "pCoupon2in2")]
c22 = unique(c2)

## column3
c3 = trn[,c("couponID3", "nCoupon3Used", "pCoupon3Used",
    "nCoup3Col1Used", "nCoup3Col2Used", "nCoup3Col3Used",
    "pCoup3Col1Used", "pCoup3Col2Used", "pCoup3Col3Used",
    "pCoupon3in3")]
c33 = unique(c3)

## Mapping to train set
tst = T[,c(5,13,21)]
tmp=left_join(tst, c11, by="couponID1")
tmp=left_join(tmp, c22, by="couponID2")
tmp=left_join(tmp, c33, by="couponID3")

tmp1 = impute.missing1000 = missForest(tmp[,-c(1:3)], maxiter=10, ntree = 1000, verbose =TRUE, mtry = 5, replace = TRUE)
T1 = cbind(orderID = T$orderID, tmp1$ximp)
T1[T1 < 1e-4] = 0

## Mapping to validation set
tst = V[,c(5,13,21)]
tmp=left_join(tst, c11, by="couponID1")
tmp=left_join(tmp, c22, by="couponID2")
tmp=left_join(tmp, c33, by="couponID3")

tmp2 = impute.missing1000 = missForest(tmp[,-c(1:3)], maxiter=10, ntree = 1000, verbose =TRUE, mtry = 5, replace = TRUE)
V1 = cbind(orderID = V$orderID, tmp2$ximp)
V1[V1 < 1e-4] = 0

## Mapping to class set
tst = C[,c(5,13,21)]
tmp=left_join(tst, c11, by="couponID1")
tmp=left_join(tmp, c22, by="couponID2")
tmp=left_join(tmp, c33, by="couponID3")

tmp3 = impute.missing1000 = missForest(tmp[,-c(1:3)], maxiter=10, ntree = 1000, verbose =TRUE, mtry = 5, replace = TRUE)
C1 = cbind(orderID = C$orderID, tmp3$ximp)
C1[C1 < 1e-4] = 0

##put it into a list named train and class
features = list(T=T1, V=V1, C=C1)

##save it as an RDS file
saveRDS(features,file="./feature/couponUsed.rds")

