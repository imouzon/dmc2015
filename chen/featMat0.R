
########  ------  Counting number of sub-category IDs  ------  ########
countCat = function(dat){
  df = as.data.frame(5 - apply(dat, 1, function(vec) sum(is.na(vec))))
  colnames(df) = "nSubCats"
  return(df)
}



d = readRDS("datClean.rds")
names(d)
hist(d$price)
hist(d$basePrice)

nlevels(as.factor(d$price))
nlevels(as.factor(d$basePrice))
nlevels(as.factor(d$reward))

mylist = c("orderID", "couponID", "price", "basePrice", "reward", "premiumProduct",
           "brand", "productGroup", "catSub1", "catSub2","catSub3","catSub4","catSub5", "couponUsed", "basketValue")
mydat <- d[ , mylist]
# saveRDS(mydat, file = "datClean2.rds")
mydat = readRDS("datClean2.rds")

nSubCats <- countCat(mydat[,9:13])
mydat = cbind(mydat, nCatSubs)


z1 <- mydat[mydat$nSubCats == 1 ,]
z2 <- mydat[mydat$nSubCats == 2 ,]
z3 <- mydat[mydat$nSubCats == 3 ,]
z4 <- mydat[mydat$nSubCats == 4 ,]
z5 <- mydat[mydat$nSubCats == 5 ,]
 
prodInfo = unique(mydat[,c("couponID", "price", "basePrice", "reward", "brand", "productGroup",
         "catSub1", "catSub2",	"catSub3", "catSub4",	"catSub5","nSubCats")]) %>% arrange(-nSubCats,brand,reward)
prodInfo.order = prodInfo[order(prodInfo$nSubCats), ]

count(prodInfo$brand)




# 22.6% of products are premium products
nrow(mydat[mydat$premiumProduct == 1,])/nrow(mydat)
nrow(mydat[mydat$premiumProduct == 0,])

levels(as.factor(mydat[mydat$premiumProduct==1,]$reward))
nlevels(as.factor(mydat[mydat$premiumProduct==0,]$reward))

nrow(mydat[mydat$premiumProduct==1 & mydat$reward == 0,])
nrow(mydat[mydat$premiumProduct==1 & mydat$reward == 0.63,])
nrow(mydat[mydat$premiumProduct==1 & mydat$reward == 0.94,])
nrow(mydat[mydat$premiumProduct==1 & mydat$reward == 1.26,])
nrow(mydat[mydat$premiumProduct==1 & mydat$reward == 1.57,])
nrow(mydat[mydat$premiumProduct==1 & mydat$reward == 1.88,])
nrow(mydat[mydat$premiumProduct==1 & mydat$reward == 2.2,])
nrow(mydat[mydat$premiumProduct==1 & mydat$reward == 2.51,])
nrow(mydat[mydat$premiumProduct==1 & mydat$reward == 3.14,])
nrow(mydat[mydat$premiumProduct==1 & mydat$reward == 6.28,])

z <- mydat[mydat$premiumProduct==1 & mydat$reward == 1.26,]




hist(mydat[mydat$premiumProduct==1 & mydat$reward == 1.26,]$price)
hist(mydat[mydat$premiumProduct==1 & mydat$reward == 1.26 & mydat$basePrice > 30, ]$basePrice)
range(mydat[mydat$premiumProduct==1 & mydat$reward == 1.26, ]$basePrice)

hist(mydat[mydat$premiumProduct==1 & mydat$reward == 1.26 & mydat$basePrice > 10, ]$basePrice)

mydat[mydat$premiumProduct==1 & mydat$reward == 1.26 & mydat$basePrice > 10,]

nrow(mydat[mydat$basePrice == 324.17, ])

mydat[mydat$premiumProduct==1, ]$basePrice


mydat[mydat$premiumProduct==1 & mydat$reward == 1.26 & mydat$brand %in% "brand1", ]
d[d$premiumProduct==1 & d$reward == 1.26 & d$brand %in% "brand3", ]
# d[d$premiumProduct==1 & d$reward == 1.26 & d$brand %in% "brand4", ]
d[d$premiumProduct==1 & d$reward == 1.26 & d$brand %in% "brand5", ]
d[d$premiumProduct==1 & d$reward == 1.26 & d$brand %in% "brand16", ]
d[d$premiumProduct==1 & d$reward == 1.26 & d$brand %in% "brand17", ]
d[d$premiumProduct==1 & d$reward == 1.26 & d$brand %in% "brand18", ]
d[d$premiumProduct==1 & d$reward == 1.26 & d$brand %in% "brand20", ]
d[d$premiumProduct==1 & d$reward == 1.26 & d$brand %in% "brand23", ]

brand1
brand3
brand4
brand5
brand16
brand17
brand18
brand20
brand23


z <- d[d$premiumProduct==1 & d$reward == 1.26 & d$brand %in% "brand4", ]



