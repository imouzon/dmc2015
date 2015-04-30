#Feature Description

- **Column Name**: The name of the variable as it appears in the feature matrix
- **NAs?**: Do the values exist for every entry
- **Type**: Left blank if no category applies. H = historical data used (uses data about previous orders), C = complete data used (aggregates information about all orders), C0 = complete data used except for current observation
- **Ys?**: Does the feature depend on our outcomes (the `y`s). Yes if the feature depends on known `couponUsed` and `basketValue`, No if it is not based on the responses we are modeling
- **inX**: is the feature in the current version of the feature matrix yet? (this is No if you are just now adding this feature)
- **Briefly**: A simple description of your feature
- **Detailed**: A more in depth description of your feature (only do this if it is very, very complicated)

| Column Name | NAs? | Type |  Ys? | Briefly | Detailed |
|:------------|:----:|:----:|:----:|:--------|:---------|
couponsReceivedDate | N | N | N | year-month-day the coupons were received | |
couponsReceivedTime | N | N | N | hour-minute-second the coupons were received | |
couponsReceivedDoW  | N | N | N | Day of week the coupons were received | |
couponsReceivedWeekend | N | N | N | 1 if coupons were received on Saturday/Sunday, 0 otherwise | |
couponsReceivedFriSat| N | N | N | 1 if the coupons were received on Friday/Saturday, 0 otherwise | |
orderTimeDate| N | N | N | year-month-day the order was made | |
orderTimeTime| N | N | N | hour-minute-second the order was made | |
orderTimeDoW| N | N | N | Day of week the order was made | |
orderTimeWeekend| N | N | N | 1 if the order was made on a weekend, 0 otherwise | |
orderTimeFriSat| N | N | N | 1 if the order was made on a Friday/Saturday, 0 otherwise | |
batchID| N | N | N | An integer indicating which batch the order belonged to, (1 to 11) | |
couponsExpire| N | N | N | year-month-day-hour-minute-second the coupons expire | |
couponsSent| N | N | N | year-month-day-hour-minute-second the couponse were sent | |
TimeBtwnSentRec| N | N | N | Time between couponse being sent and coupons being received (in hours) | |
TimeBtwnRecExpire| N | N | N | Time between coupons being received and coupons expiring (in hours) | |
TimeBtwnRecOrder| N | N | N | Time between coupons being received and order being placed (in hours) | |
TimeBtwnOrderExpire| N | N | N | Time between order being placed and coupons expiring (in hours) ||
nCoupon1 | N | C | N | Total times coupon 1 was seen across test and train set | |
nCoupon2 | N | C | N | Total times coupon 2 was seen across test and train set | | 
nCoupon3 | N | C | N | Total times coupon 3 was seen across test and train set | |
nCoup[ i ]Col[ j ] | N | C | N | Total times coupon ```[i]``` appeared in column ```[j]``` for i = 1, 2, 3, and j = 1, 2, 3 
pCoup[ i ]Col[ j ] | N | C | N | Proportion of times coupon ```[i]``` appeared in column ```[j]``` for i = 1, 2, 3 and j = 1, 2, 3
nCoupon1Used | N | H | Y | Total times coupon 1 was used across all columns in train set | |
nCoupon2Used | N | H | Y | Total times coupon 2 was used across all columns in train set | | 
nCoupon3Used | N | H | Y | Total times coupon 3 was used across all columns in train set | |
pCoupon1Used | N | H | Y | nCoupon1Used / (total times coupon 1 was seen in the train set) | |
pCoupon2Used | N | H | Y | nCoupon2Used / (total times coupon 2 was seen in the train set) | | 
pCoupon3Used | N | H | Y | nCoupon3Used / (total times coupon 3 was seen in the train set) | |
nCoup[ i ]Col[ j ]Used | N | H | Y | Similar definition as before. $\sum_{j=1}^{3}nCoup[i]Col[j]Used = nCoupon[i]Used$
pCoup[ i ]Col[ j ]Used | N | H | Y | Similar definition as before. nCoup[ i ]Col[ j ]Used / nCoupon[i]Used.
nCoup[ i ]Batch | N | C | N | The number of times coupon from column ```i``` appeared in the current batch.
