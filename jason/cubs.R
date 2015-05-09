## cubs stands for "coupon upper bounds". This function reads in the type of
## data we're using and outputs a data frame with columns:
## couponID: self-explanatory
## upper.bound: the smallest possible value of (product + coupon)
## num.orders: how many orders the coupon was used in
## unique.basket.vals: how many unique basket values there were for those orders
cubs <- function(trn) {
    library(dplyr)
    library(ggvis)
    library(stringr)
    library(tidyr)

    trn <- trn %>% tbl_df()

    ## Goal: For each coupon, minimize the basket values over all orders in
    ## which that coupon is used. I use the column name "position" under
    ## the assumption that the customers see coupon 1 first in the list,
    ## then coupon 2, then coupon 3 (perhaps, say, in an email).
    trn2 <- trn %>%
        mutate(couponID1 = as.character(couponID1),
               couponID2 = as.character(couponID2),
               couponID3 = as.character(couponID3)) %>%
        gather(key = position, value = couponID, couponID1, couponID2, couponID3) %>%
        mutate(position = as.numeric(str_extract(position, "\\d"))) %>%
        filter(coupon1Used | coupon2Used | coupon3Used) %>%
        mutate(used = rep(NA, length(position)))

    ## Now I fill in the new columns with the relevant information.
    for (i in 1:length(trn2$used)) {
        if (trn2$position[i] == 1)
            trn2$used[i] <- trn2$coupon1Used[i]
        else if (trn2$position[i] == 2)
            trn2$used[i] <- trn2$coupon2Used[i]
        else
            trn2$used[i] <- trn2$coupon3Used[i]
    }

    ## Get the results!
    coupon.tdf <- trn2 %>%
        filter(used == 1) %>%
        group_by(couponID) %>%
        summarize(upper.bound = min(basketValue),
                  num.orders = n(),
                  unique.basket.vals = n_distinct(basketValue)) %>%
        arrange(desc(unique.basket.vals))

    return(coupon.tdf)
}
