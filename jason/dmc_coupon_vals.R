## Loading necessary libraries.
library(dplyr)
library(ggplot2)
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
  mutate(used = rep(NA, length(position)))

## Now I fill in the used column with the relevant information.
for (i in 1:length(trn2$used)) {
  if (trn2$position[i] == 1)
    trn2[i, 32] <- trn2$coupon1Used[i]
  else if (trn2$position[i] == 2)
    trn2[i, 32] <- trn2$coupon2Used[i]
  else
    trn2[i, 32] <- trn2$coupon3Used[i]
}

## Get the results!
trn2 <- trn2 %>%
  filter(used == 1) %>%
  group_by(couponID) %>%
  summarize(upper.bound = min(basketValue),
            count = n(),
            unique.basket.vals = n_distinct(basketValue)) %>%
  arrange(desc(unique.basket.vals))

## Make a plot
plot.me <- trn2 %>%
  filter(upper.bound < 1000)

jpeg('upper_bounds.jpg')
ggplot(data = plot.me, aes(x = upper.bound)) +
  geom_histogram(binwidth = 17)
dev.off()

## Which coupons are associated with multiple orders that have
## the same basket value?
same.val <- with(trn2, which(unique.basket.vals != count))
trn2[same.val, ]
