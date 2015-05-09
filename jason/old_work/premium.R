## Loading libraries and reading in the data.
library(dplyr)
library(ggvis)
library(lubridate)
library(magrittr)
library(stringr)
library(tidyr)
trn <- read.csv("../data/clean_data/train_simple_name.csv",
                as.is = T, header = T) %>% tbl_df()
tst <- read.csv("../data/clean_data/test_simple_name.csv",
                as.is = T, header = T) %>% tbl_df()

## For each coupon, let's determine the total number of times the coupon is used
## and whether it's associated with a premium product or not. First I'll make it
## so each member of couponID1, couponID2, and couponID3 gets its own row with the
## associated information.
trn2 <- trn %>%
  gather(key = position, value = couponID, couponID1, couponID2, couponID3) %>%
  mutate(position = as.numeric(str_extract(position, "\\d"))) %>%
  mutate(price = rep(NA, length(position)),
         basePrice = rep(NA, length(position)),
         reward = rep(NA, length(position)),
         premiumProduct = rep(NA, length(position)),
         brand = rep(NA, length(position)),
         productGroup = rep(NA, length(position)),
         categoryIDs = rep(NA, length(position)),
         used = rep(NA, length(position)))
for (i in 1:length(trn2$used)) {
  if (trn2$position[i] == 1)
    trn2[i, 32:39] <- trn2[i, 6:13]
  else if (trn2$position[i] == 2)
    trn2[i, 32:39] <- trn2[i, 14:21]
  else
    trn2[i, 32:39] <- trn2[i, 22:29]
}
trn2 <- trn2 %>%
    select(orderID:basketValue, position:used)

## Group by coupon ID and determine if it's a premium product
## and the number of times it was used.
trn2 <- trn2 %>%
    group_by(couponID) %>%
        summarize(num.orders = n(),
                  used = sum(used),
                  premium = sum(premiumProduct))

## If a coupon's product is labeled as premium for one order, is it labeled
## as premium for all other orders in which it appears? Need to check if the
## number of times a coupon appears in the dataset = number of times it's labeled
## as premium.
prem.indices <- which(trn2$premium != 0)
all.equal(trn2$num.orders[prem.indices],
          trn2$premium[prem.indices])
## Yes; our intuition is correct.

trn2$premium <- trn2$premium != 0
ggplot(subset(trn2, used != 0),
       aes(x = premium, y = used / num.orders, fill = premium)) +
    geom_boxplot()
