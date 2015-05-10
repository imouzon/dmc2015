## Loading necessary libraries.
library(dplyr)
library(ggvis)
library(lubridate)
library(magrittr)
library(stringr)
library(tidyr)

## If you're unfamiliar with dplyr, here are some commands
## that will help you understand how this code works:

## filter: select rows matching some criterion
## select: select columns matching some criterion
## arrange: arrange rows according to some variable
## mutate: add new variables, possibly as functions of old ones
## summarize: reduce variables to values, like the summarize part of ddply
## group_by: splits data frames up according to a certain (set of) variable(s)

## Most of these commands can also be done in base R, but the advantage
## to using dplyr is that they (1) may be faster than the base R versions
## and (2) have been designed to work well with pipelines (the %>% operator).
## The updated versions of reshape2 and ggplot2, called tidyr and ggvis,
## respectively, are also designed to work with pipelines.

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

## Make a plot. This is saved in upper_bounds.svg in my github folder.
# cutoff <- 1000

# coupon.tdf %>%
#   filter(upper.bound < cutoff) %>%
#   ggvis(x = ~upper.bound) %>%
#   layer_histograms(width = 17)

## Which coupons are associated with multiple orders that have
## the same basket value? Find those coupons and then find all the
## records in trn2 corresponding to those coupons.
same.val.tdf <- coupon.tdf %>%
  filter(unique.basket.vals != num.orders)

same.val.orders <- trn2 %>%
  filter(couponID %in% same.val.tdf$couponID) %>%
  filter(used == 1) %>%
  arrange(couponID) %>%
  select(couponID, basketValue, orderID)

## Are any of the bound-defining basket values repeated more than once?
same.val.orders %>%
  print.data.frame()

####################################################################################

## "De-coupon" the dataset where we say that any values below some cutoff
## are the "true values" of (product + coupon).
cutoff2 <- 200

## We are pretending like (product + coupon) values above the
## prespecified cutoff are just the mean of the values below that cutoff.
true.vals <- coupon.tdf
coupon.tdf$upper.bound[coupon.tdf$upper.bound > cutoff2] <-
  mean(coupon.tdf$upper.bound[coupon.tdf$upper.bound <= cutoff2])
trn3 <- trn # Just so I don't screw up the original dataset.

trn3.nrows <- dim(trn3)[1]
for (i in 1:trn3.nrows) {
  if (trn3$coupon1Used[i] == 1)
    if (trn3$couponID1[i] %in% true.vals$couponID) {
      index = which(true.vals$couponID == trn3$couponID1[i])
      trn3$basketValue[i] <- trn3$basketValue[i] - true.vals$upper.bound[index]
    }
  else if (trn3$coupon2Used[i] == 1)
    if (trn3$couponID2[i] %in% true.vals$couponID) {
      index = which(true.vals$couponID == trn3$couponID2[i])
      trn3$basketValue[i] <- trn3$basketValue[i] - true.vals$upper.bound[index]
    }
  else if (trn3$coupon3Used[i] == 1)
    if (trn3$couponID3[i] %in% true.vals$couponID) {
      index = which(true.vals$couponID == trn3$couponID3[i])
      trn3$basketValue[i] <- trn3$basketValue[i] - true.vals$upper.bound[index]
    }
}

## Did we accidentally reduce any of the basket values to below $0?
sum(trn3$basketValue < 0)
## Nope!

## We are now justified in killing off all the coupon columns
## in trn3 and trying to model basket value with the remaining variables.
## This is analogous to modeling a stationary time series after removing trend
## and seasonal components.
trn3 <- trn3 %>%
  select(orderID:couponsReceived, basketValue)

## The span of time between when the coupons were received and when the order
## was placed.
trn3$timespan <- trn3$orderTime - trn3$couponsReceived
trn3 <- trn3 %>%
  select(-orderTime, -couponsReceived)


countem <- trn %>%
  group_by(userID) %>%
  summarize(count = n()) %>%
  ggvis(x = ~count) %>%
  layer_histograms()
