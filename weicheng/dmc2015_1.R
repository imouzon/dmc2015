## It's terrible to see the long encrypted ID codes,
## so reform these columns to integer which would be
## much easier to deal with
## train data
dat = read.table("../data/raw_data/DMC_2015_orders_train.txt",
    stringsAsFactors=FALSE,sep ="|", header=TRUE)

## levels of couponID
couponId = unique(c(dat$couponID1, dat$couponID2, dat$couponID3))
brand = unique(c(dat$brand1, dat$brand2, dat$brand3))
categoryIDs = unique(c(dat$categoryIDs1, dat$categoryIDs2, dat$categoryIDs3))
productGroup = unique(c(dat$productGroup1, dat$productGroup2, dat$productGroup3))
userID = unique(dat$userID)

couponID1 = match(dat$couponID1, couponId)
couponID2 = match(dat$couponID2, couponId)
couponID3 = match(dat$couponID3, couponId)
brand1 = match(dat$brand1, brand)
brand2 = match(dat$brand2, brand)
brand3 = match(dat$brand3, brand)
categoryIDs1 = match(dat$categoryIDs1, categoryIDs)
categoryIDs2 = match(dat$categoryIDs2, categoryIDs)
categoryIDs3 = match(dat$categoryIDs3, categoryIDs)
productGroup1 = match(dat$productGroup1, productGroup)
productGroup2 = match(dat$productGroup2, productGroup)
productGroup3 = match(dat$productGroup3, productGroup)

dat$couponID1 = couponID1
dat$couponID2 = couponID2
dat$couponID3 = couponID3
dat$brand1 = brand1
dat$brand2 = brand2
dat$brand3 = brand3
dat$categoryIDs1 = categoryIDs1
dat$categoryIDs2 = categoryIDs2
dat$categoryIDs3 = categoryIDs3
dat$productGroup1 = productGroup1
dat$productGroup2 = productGroup2
dat$productGroup3 = productGroup3
dat$userID = match(dat$userID, userID)

str(dat)
write.csv(dat, file = "dmc2015_train.csv")
