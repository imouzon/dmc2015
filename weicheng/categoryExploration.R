trn = read.csv("./data/mc2015_train_simple.csv",
    stringsAsFactors=FALSE, header=TRUE)
tst = read.csv("./data/mc2015_test_simple.csv",
    stringsAsFactors=FALSE, header=TRUE)

trn = within(trn, {
    orderTime = as.POSIXct(orderTime)
    couponsReceived = as.POSIXct(couponsReceived)
})
tst = within(tst, {
    orderTime = as.POSIXct(orderTime)
    couponsReceived = as.POSIXct(couponsReceived)
})

dat=trn

## ---- Q1
catlist= lapply(c(dat$categoryIDs1, dat$categoryIDs2,
    dat$categoryIDs3), FUN = function(x) strsplit(x, split=":")[[1]])
catNum = sapply(catlist, length)
max(catNum)

## ---- Q2-1
## coupon usage rate for coupons that belong to different number of categories.
catlist= lapply(dat$categoryIDs1, FUN = function(x) strsplit(x, split=":")[[1]])
catNum = sapply(catlist, length)
mat = matrix(0, 5, 5)
for(i in 1:5){
    tmpDat = dat[catNum==i,]
    mat[i,] = c(nrow(tmpDat), sum(tmpDat$coupon1Used), sum(tmpDat$coupon1Used)/nrow(tmpDat), mean(tmpDat$basketValue), median(tmpDat$basketValue))
}
rownames(mat) = c("catNum1", "catNum2", "catNum3", "catNum4", "catNum5")
colnames(mat) = c("totalNum", "coup1UsedNum","coup1UsedRatio", "meanBasketValue", "medianBasketValue")
mat

## ---- Q2-2
catlist= lapply(dat$categoryIDs2, FUN = function(x) strsplit(x, split=":")[[1]])
catNum = sapply(catlist, length)
mat = matrix(0, 5, 5)
for(i in 1:5){
    tmpDat = dat[catNum==i,]
    mat[i,] = c(nrow(tmpDat), sum(tmpDat$coupon2Used), sum(tmpDat$coupon2Used)/nrow(tmpDat), mean(tmpDat$basketValue), median(tmpDat$basketValue))
}
rownames(mat) = c("catNum1", "catNum2", "catNum3", "catNum4", "catNum5")
colnames(mat) = c("totalNum", "coup2UsedNum", "coup2UsedRatio", "meanBasketValue", "medianBasketValue")
mat

## ---- Q2-3
catlist= lapply(dat$categoryIDs3, FUN = function(x) strsplit(x, split=":")[[1]])
catNum = sapply(catlist, length)
mat = matrix(0, 5, 5)
for(i in 1:5){
    tmpDat = dat[catNum==i,]
    mat[i,] = c(nrow(tmpDat), sum(tmpDat$coupon3Used), sum(tmpDat$coupon3Used)/nrow(tmpDat), mean(tmpDat$basketValue), median(tmpDat$basketValue))
}
rownames(mat) = c("catNum1", "catNum2", "catNum3", "catNum4", "catNum5")
colnames(mat) = c("totalNum", "coup3UsedNum", "coup3UsedRatio", "meanBasketValue", "medianBasketValue")
mat

## ---- Q3-1
mat = matrix(0, 31, 5)
rownames(mat) = paste0("category", 1:31)
colnames(mat) = c("totalNum", "coup1UsedNum", "coup1UsedRatio", "meanBasketValue", "medianBasketValue")

for(i in 1:31){
    catlist= lapply(dat$categoryIDs1, FUN = function(x)
        as.integer(strsplit(x, split=":")[[1]]))
    cat1.index = sapply(catlist, FUN=function(x) as.logical(sum(x %in% i)))
    dat.cat1 = dat[cat1.index,]
    mat[i,] = c(nrow(dat.cat1), sum(dat.cat1$coupon1Used), sum(dat.cat1$coupon1Used)/nrow(dat.cat1), mean(dat.cat1$basketValue), median(dat.cat1$basketValue))
}
mat

## ---- Q3-2
mat = matrix(0, 31, 5)
rownames(mat) = paste0("category", 1:31)
colnames(mat) = c("totalNum", "coup3UsedNum", "coup3UsedRatio", "meanBasketValue", "medianBasketValue")
for(i in 1:31){
    catlist= lapply(dat$categoryIDs2, FUN = function(x)
        as.integer(strsplit(x, split=":")[[1]]))
    cat2.index = sapply(catlist, FUN=function(x) as.logical(sum(x %in% i)))
    dat.cat2 = dat[cat2.index,]
    mat[i,] = c(nrow(dat.cat2), sum(dat.cat2$coupon2Used), sum(dat.cat2$coupon2Used)/nrow(dat.cat2), mean(dat.cat2$basketValue), median(dat.cat2$basketValue))
}
mat

## ---- Q3-3
mat = matrix(0, 31, 5)
rownames(mat) = paste0("category", 1:31)
colnames(mat) = c("totalNum", "coup3UsedNum", "coup3UsedRatio", "meanBasketValue", "medianBasketValue")
for(i in 1:31){
    catlist= lapply(dat$categoryIDs3, FUN = function(x)
        as.integer(strsplit(x, split=":")[[1]]))
    cat3.index = sapply(catlist, FUN=function(x) as.logical(sum(x %in% i)))
    dat.cat3 = dat[cat3.index,]
    mat[i,] = c(nrow(dat.cat3), sum(dat.cat3$coupon3Used), sum(dat.cat3$coupon3Used)/nrow(dat.cat3), mean(dat.cat3$basketValue), median(dat.cat3$basketValue))
}
mat

## ---- Q4
## category matrix
catmat = matrix(0, nrow(dat), 31)
catlist = lapply(dat$categoryIDs1, FUN = function(x)
    as.integer(strsplit(x, split=":")[[1]]))
for(i in 1:31)
    catmat[, i] = sapply(catlist, FUN = function(x) sum(x%in%i))
catcor = cor(catmat)
index = which(catcor[upper.tri(catcor)] >0.3)
catcor[upper.tri(catcor)][index]
catcor[11,21]

## ---- others
tmp = unique(dat$categoryIDs1)
catlist = lapply(tmp, FUN = function(x) strsplit(x, split=":")[[1]])
index = sapply(catlist, length)
cat=list()

for(i in 1:max(index))
    cat[[i]] = unlist(catlist[index==i])

cat[[2]]%in%cat[[1]]
cat[[3]]%in%cat[[1]]
cat[[4]]%in%cat[[1]]
cat[[5]]%in%cat[[1]]
cat[[1]]%in%cat[[5]]

cat[[5]]%in%unlist(cat[1:4])
cat[[4]]%in%unlist(cat[1:3])
cat[[3]]%in%unlist(cat[1:2])
cat[[2]]%in%unlist(cat[1])


