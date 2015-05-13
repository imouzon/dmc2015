source("read_data.R")


coupon1 = train[,c("coupon1Used", "couponID1")]
coupon2 = train[,c("coupon2Used", "couponID2")]
coupon3 = train[,c("coupon3Used", "couponID3")]

names(coupon1) <- c("used", "coupon")
names(coupon2) <- c("used", "coupon")
names(coupon3) <- c("used", "coupon")

coupon1$coupon <- as.character(coupon1$coupon)
coupon1$place <- 1
coupon2$coupon <- as.character(coupon2$coupon)
coupon2$place <- 2
coupon3$coupon <- as.character(coupon3$coupon)
coupon3$place <- 3

couponData = rbind(coupon1, coupon2, coupon3)
couponData$coupon = as.factor(couponData$coupon)
levels(couponData$coupon) = 1:length(levels(couponData$coupon))

couponSummary = couponData %>% group_by(coupon, place) %>%
  summarize(count = length(used),
            used = sum(used))

# get all coupons that appeared in every order
tab = table(couponSummary$coupon)
ids = as.numeric(which(tab == 3))

index = rep(NA, 3*length(ids))
j = 1
for (i in ids) {
  index[j:(j+2)] = which(couponSummary$coupon == i)
  j = j + 3
}

couponSum2 = couponSummary[index,]
couponSum2$rate = couponSum2$used / couponSum2$count
