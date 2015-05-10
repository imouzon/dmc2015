## Loading necessary libraries. Sourcing other files.
library(dplyr)
library(ggvis)
library(stringr)
library(tidyr)
source("cubs.R")

## Reading in the data. Converting to tbl_df() for sanity.
trn <- readRDS("../data/clean_data/universalCleanData.rds") %>%
    tbl_df()

coupon.tdf <- cubs(trn)

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
