library(ggplot2)
library(reshape2)

dat = read.table("../data/raw_data/DMC_2015_orders_train.txt",
    sep ="|", header=TRUE)

names(dat)
str(dat)

## ---- Q1
## Q: Does one particular coupon belong to only one couponID?
## A: No
nlevels(dat$couponID1) + nlevels(dat$couponID2) + nlevels(dat$couponID3)

coupon = c(as.character(dat$couponID1), as.character(dat$couponID2),
    as.character(dat$couponID3))

length(unique(coupon))

## ---- Q2
## Q: Reward distributions?
## reward1, 2, 3 have the same reward 'standard'
sort(unique(dat$reward1))
sort(unique(dat$reward2))
sort(unique(dat$reward3))
## barplot
reward = with(dat,
     {
         rbind(table(reward1), table(reward2), table(reward3))
     })
barplot(reward, beside=TRUE, legend.text = c("reward1", "reward2", "reward3"))

## ---- Q3
## Q: Price distributions?
summary(dat$price1)
summary(dat$price2)
summary(dat$price3)
price = melt(dat[c("price1", "price2", "price3")])
ggplot(price, aes(value, fill = variable)) + geom_histogram(position = "dodge")
ggplot(price, aes(value, fill = variable)) + geom_histogram(binwidth = 1, position = "dodge") + xlim(0, 20)

## ---- Q4
## Q: Price distributions?
basePrice = melt(dat[c("basePrice1", "basePrice2", "basePrice3")])
ggplot(basePrice, aes(value, fill = variable)) + geom_histogram(position = "dodge")
ggplot(basePrice, aes(value, fill = variable)) + geom_histogram(binwidth = 1, position = "dodge") + xlim(0, 25)

## ---- Q5
## Q: basketValue distribution?
summary(dat$basketValue)
hist(dat$basketValue[dat$basketValue<1000])

## ---- Q6
## Q: coupon usage: 1 > 2 > 3
sum(dat$coupon1Used)
sum(dat$coupon2Used)
sum(dat$coupon3Used)
## total coupon usage rate
sum(dat$coupon1Used + dat$coupon2Used + dat$coupon3Used > 0)/nrow(dat)

## The distributions of basketValue for customers who used coupon and
## who didn't use coupon are similar
coupon_yes= dat[dat$coupon1Used + dat$coupon2Used + dat$coupon3Used > 0,]
coupon_no= dat[dat$coupon1Used + dat$coupon2Used + dat$coupon3Used ==  0,]

summary(coupon_yes$basketValue)
summary(coupon_no$basketValue)

coupon = data.frame(basketValue = c(coupon_yes$basketValue, coupon_no$basketValue),
    couponUsed = c(rep("yes", nrow(coupon_yes)), rep("no", nrow(coupon_no))))

ggplot(coupon, aes(basketValue, fill = couponUsed)) +
    geom_histogram(position = "dodge") + xlim(0, 500)

## ---- Q7
## Q:
## basketValue vs coupon usage rate
dat1 = dat[,29:32]

bv = seq(45, 1000, by = 5)
w = 30
couponRate = numeric(length(bv))
for(i in 2:length(bv)){
    tmp = dat1[(dat1$basketValue > bv[i-1]-w) & (dat1$basketValue < bv[i]+w),]
    couponRate[i-1] = sum(tmp$coupon1Used + tmp$coupon2Used + tmp$coupon3Used > 0)/nrow(tmp)
}
plot(bv, couponRate, type="l", xlab="basketValue")

## basketValue > 1000
highBV = dat[dat$basketValue > 1000,]
sum(highBV$coupon1Used + highBV$coupon2Used + highBV$coupon3Used > 0)/nrow(highBV)


range(dat$price1)
range(dat$basePrice1)
plot(dat$price1, dat$basePrice1, ylim = c(0, 100))
plot(dat$price1, log(dat$basePrice1), ylim = c(0, 10))

plot(dat$price1, dat$reward1)
plot(dat$price2, dat$reward2)
plot(dat$price3, dat$reward3)



