## Loading necessary libraries.
library(dplyr)
library(ggvis)
library(lubridate)
library(magrittr)
library(stringr)
library(tidyr)

## Reading in the data, trn for "training", tst for "test".
## Make the dates nicer with lubridate functions, and
## turn the data frames into tbl_dfs to make things sane.
trn = read.delim("../data/raw_data/DMC_2015_orders_train.txt",
  stringsAsFactors = FALSE, sep = "|", quote = "") %>%
  mutate(orderTime = ymd_hms(orderTime),
         couponsReceived = ymd_hms(couponsReceived)) %>%
  tbl_df()
tst = read.delim("../data/raw_data/DMC_2015_orders_class.txt",
  stringsAsFactors = FALSE, sep = "|", quote = "") %>%
  mutate(orderTime = ymd_hms(orderTime),
         couponsReceived = ymd_hms(couponsReceived)) %>%
  tbl_df()

## Goal: For each coupon, minimize the basket values over all orders in
## which that coupon is used. I use the column name "position" under
## the assumption that the customers see coupon 1 first in the list,
## then coupon 2, then coupon 3 (perhaps, say, in an email).
trn2 <- trn %>%
  gather(key = position, value = couponID, couponID1, couponID2, couponID3) %>%
  mutate(position = as.numeric(str_extract(position, "\\d"))) %>%
  filter(coupon1Used | coupon2Used | coupon3Used) %>%
  mutate(price = rep(NA, length(position)),
         basePrice = rep(NA, length(position)),
         reward = rep(NA, length(position)),
         premiumProduct = rep(NA, length(position)),
         brand = rep(NA, length(position)),
         productGroup = rep(NA, length(position)),
         categoryIDs = rep(NA, length(position)),
         used = rep(NA, length(position)))

## Now I fill in the new columns with the relevant information.
for (i in 1:length(trn2$used)) {
  if (trn2$position[i] == 1) {
    trn2[i, 32:38] <- trn2[i, 5:11]
    trn2$used[i] <- trn2$coupon1Used[i]
  }
  else if (trn2$position[i] == 2) {
    trn2[i, 32:38] <- trn2[i, 12:18]
    trn2$used[i] <- trn2$coupon2Used[i]
  }
  else {
    trn2[i, 32:38] <- trn2[i, 19:25]
    trn2$used[i] <- trn2$coupon3Used[i]
  }
}

## Updating trn2 just in case it's useful later.
trn2 <- trn2 %>%
  select(orderID:couponsReceived, basketValue:used)

## Get the results!
coupon.tdf <- trn2 %>%
  filter(used == 1) %>%
  group_by(couponID) %>%
  summarize(upper.bound = min(basketValue),
            num.orders = n(),
            unique.basket.vals = n_distinct(basketValue)) %>%
  arrange(desc(unique.basket.vals))

## Creating features upper1, upper2, and upper3 for each orderID.
trn3 <- trn # Copying the data so as not to corrupt the original.
trn3 <- trn3 %>% mutate(upper1 = rep(NA, dim(trn3)[1]),
                        upper2 = upper1,
                        upper3 = upper2)

## Main update. Note that the couponIDs are in columns 5,
## 13, and 21. ## The upper columns created above  are now columns
## 33-35 of trn3.
for (i in 1:dim(trn3)[1]) {
    if (trn3$couponID1[i] %in% coupon.tdf$couponID)
        trn3$upper1[i] <- coupon.tdf$upper.bound[which(coupon.tdf$couponID ==
                                                       trn3$couponID1[i])]
    if (trn3$couponID2[i] %in% coupon.tdf$couponID)
        trn3$upper2[i] <- coupon.tdf$upper.bound[which(coupon.tdf$couponID ==
                                                           trn3$couponID2[i])]
    if (trn3$couponID3[i] %in% coupon.tdf$couponID)
        trn3$upper3[i] <- coupon.tdf$upper.bound[which(coupon.tdf$couponID ==
                                                       trn3$couponID3[i])]
}

## Pack these features up in a csv with orderID.
trn3 <- trn3 %>%
    select(orderID, upper1:upper3)
