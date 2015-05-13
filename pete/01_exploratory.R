# read and manip data
source("read_data.R")



class <- read.csv("/Users/epwalsh/GitHub/dmc2015/data/raw_data/DMC_2015_orders_class.txt", sep="|")
train <- read.csv("/Users/epwalsh/GitHub/dmc2015/data/raw_data/DMC_2015_orders_train.txt", sep="|")

library(lubridate)
library(ggplot2)
library(dplyr)
library(reshape2)

# ==================================================================
# how does order time compare to the time the coupon was recieved?
# ==================================================================

train$orderTime <- ymd_hms(train$orderTime)
train$couponsReceived <- ymd_hms(train$couponsReceived)
train$time_diff = train$orderTime - train$couponsReceived

# create variable for difference in times (minutes)
train$minute_diff = as.numeric(train$time_diff) / 60
train$day_diff = (ceiling_date(train$orderTime, "day") - 
      ceiling_date(train$couponsReceived, "day")) / (3600*24)

# create variable for day of the week, for both recieved and order time
train$wday_orderTime <- wday(train$orderTime, label=T)
train$wday_couponsReceived <- wday(train$couponsReceived, label=T)

qplot(train$minute_diff, geom="histogram", colour=I("white"))

train %>% ggplot(aes(x = wday_orderTime, fill = wday_couponsReceived)) + 
  geom_bar()
plot01 <- train %>% ggplot(aes(x = wday_couponsReceived, fill = wday_orderTime)) + 
  geom_bar()
jpeg("plot01.jpg")
plot01
dev.off()

wday_sum <- train %>% group_by(wday_couponsReceived, wday_orderTime) %>% 
  summarize(count = length(orderID))
wday_sum

# most coupons are sent out on Tuesday
# all coupons expire before Tuesday
train %>% ggplot(aes(x = wday_couponsReceived, fill = factor(day_diff))) + 
  geom_bar()



# ==================================================================
# how does coupons usage vary by day received? 
# ==================================================================

train$coupons_used = train$coupon1Used + train$coupon2Used + train$coupon3Used
plot02 <- train %>% ggplot(aes(x = wday_couponsReceived, fill = factor(coupons_used))) + 
  geom_bar()
jpeg("plot02.jpg")
plot02
dev.off()

usage = train %>% group_by(wday_couponsReceived) %>%
  summarize(none = round(sum(coupons_used == 0) / length(coupons_used), 2),
            one = round(sum(coupons_used == 1) / length(coupons_used), 2),
            two = round(sum(coupons_used == 2) / length(coupons_used), 2),
            three = round(sum(coupons_used == 3) / length(coupons_used), 2)) %>%
  melt(id = "wday_couponsReceived", 
       variable.name = "used",
       value.name = "perc")
usage <- usage[order(usage$wday_couponsReceived),]

plot03 <- qplot(used, perc, data = usage, facets = ~wday_couponsReceived, 
      geom="bar", stat = "identity")
jpeg("plot03.jpg")
plot03
dev.off()


# ==================================================================
# how does placement (first, second, or third) affect coupon usage? 
# ==================================================================

# coupon 1 used the most, then coupon 2, then coupon 3
sum(train$coupon1Used)
sum(train$coupon2Used)
sum(train$coupon3Used)


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

# get unique coupon IDs
uniqueCoupons = levels(couponData$coupon)

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

# coupons that were sent out in each order, used more often when 1st coupon
couponSum3 = couponSum2 %>% group_by(place) %>%
  summarize(count = sum(count),
            used = sum(used),
            rate = sum(used) / sum(count))

# plot this
# couponSum4 = t(cbind(couponSum3[,c(1,4)], 1 - couponSum3$rate))[2:3,]
couponSum4 = rbind(couponSum3$used, couponSum3$count - couponSum3$used)
rownames(couponSum4) = c("used", "not used")
colnames(couponSum4) = c("1", "2", "3")
prop.table(couponSum4, 2)
jpeg("figures/plot04.jpg")
barplot(prop.table(couponSum4, 2), col=c("slateblue", "mediumseagreen"),
        legend=rownames(couponSum4), xlab = "position", 
        main = "Usage of coupons that appear in all 3 positions")
dev.off()


