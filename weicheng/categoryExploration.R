trn = read.table("./data/mc2015_train_simple.csv",
    stringsAsFactors=FALSE,sep ="|", header=TRUE)
tst = read.table("./data/mc2015_test_simple.csv",
    stringsAsFactors=FALSE,sep ="|", header=TRUE)

trn = within(trn.simple, {
    orderTime = as.POSIXct(orderTime)
    couponsReceived = as.POSIXct(couponsReceived)
})
tst = within(tst.simple, {
    orderTime = as.POSIXct(orderTime)
    couponsReceived = as.POSIXct(couponsReceived)
})


## coupon usage rate for coupons that belong to different number of categories.
dat=trn

catlist= lapply(dat$categoryIDs1, FUN = function(x) strsplit(x, split=":")[[1]])
catNum = sapply(catlist, length)
for(i in 1:5){
    tmpDat = dat[catNum==i,]
    cat(nrow(dat[catNum==i,]), sum(tmpDat$coupon1Used > 0)/nrow(tmpDat), sum(tmpDat$coupon1Used + tmpDat$coupon2Used + tmpDat$coupon3Used > 0)/nrow(tmpDat), mean(tmpDat$basketValue),"\n")
}

catlist= lapply(dat$categoryIDs2, FUN = function(x) strsplit(x, split=":")[[1]])
catNum = sapply(catlist, length)
for(i in 1:5){
    tmpDat = dat[catNum==i,]
     cat(nrow(dat[catNum==i,]),sum(tmpDat$coupon2Used > 0)/nrow(tmpDat), sum(tmpDat$coupon1Used + tmpDat$coupon2Used + tmpDat$coupon3Used > 0)/nrow(tmpDat), mean(tmpDat$basketValue),"\n")
}

catlist= lapply(dat$categoryIDs3, FUN = function(x) strsplit(x, split=":")[[1]])
catNum = sapply(catlist, length)
for(i in 1:5){
    tmpDat = dat[catNum==i,]
    cat(nrow(dat[catNum==i,]),sum(tmpDat$coupon3Used > 0)/nrow(tmpDat), sum(tmpDat$coupon1Used + tmpDat$coupon2Used + tmpDat$coupon3Used > 0)/nrow(tmpDat), mean(tmpDat$basketValue),"\n")
}


## coupon1 usage rate for each category
for(i in 1:31){
    catlist= lapply(dat$categoryIDs1, FUN = function(x)
        as.integer(strsplit(x, split=":")[[1]]))
    cat1.index = sapply(catlist, FUN=function(x) as.logical(sum(x %in% i)))
    dat.cat1 = dat[cat1.index,]
    couponRate <- function(dat){
        sum(dat$coupon1Used > 0)/nrow(dat)
    }
    cat(nrow(dat.cat1), sum(dat.cat1$coupon1Used), couponRate(dat.cat1), mean(dat.cat1$basketValue),"\n")
}

for(i in 1:31){
    catlist= lapply(dat$categoryIDs2, FUN = function(x)
        as.integer(strsplit(x, split=",")[[1]]))
    cat21.index = sapply(catlist, FUN=function(x) as.logical(sum(x %in% i)))
    dat.cat1 = dat[cat21.index,]
    couponRate <- function(dat){
        sum(dat$coupon2Used > 0)/nrow(dat)
    }
    cat(nrow(dat.cat1), sum(dat.cat1$coupon2Used), couponRate(dat.cat1), mean(dat.cat1$basketValue),"\n")
}

for(i in 1:31){
    catlist= lapply(dat$categoryIDs3, FUN = function(x)
        as.integer(strsplit(x, split=",")[[1]]))
    cat31.index = sapply(catlist, FUN=function(x) as.logical(sum(x %in% i)))
    dat.cat1 = dat[cat31.index,]
    couponRate <- function(dat){
        sum(dat$coupon3Used > 0)/nrow(dat)
    }
    cat(nrow(dat.cat1), sum(dat.cat1$coupon3Used), couponRate(dat.cat1), mean(dat.cat1$basketValue),"\n")
}


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


