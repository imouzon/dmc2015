How to use those Excels
========================================================

There are three Excels now: (all the IDS are trmmed to first 6 characters [verified to be a safe way and easy to work with] )

## user_summary, coupon_summary, and test_summary 

For me it is easy to track indexes and pull out some information.







```r
# user_summary, coupon_summary are from train set. They are ordered unique userID and ordered unique couponID1-3

# Ex:  From user_summary. So the 6th line means: user "013965" appeared in the 1675, 2288, 3534, 5070 rows of the train data set. 
#I could easily pull out that user:    train[c(1675, 2288, 3534, 5070), ]

#   user  index
# 1	003f47	2304
# 2	0060d4	324
# 3	008ca9	3061
# 4	010f3b	4363
# 5	0118b7	2558
# 6	013965	1675, 2288, 3534, 5070


# Ex:  From coupon_summary. 
#the 1st line means: coupon "000c1c" appeared in the 5300 row of train data set in column couponID1, and #did not appear in column couponID2 and couponID3.
# the 2nd line means: coupon "0020c3" appeared in 5950 row of train data set only in column couponID3.


# 1 000c1c	5300   |      |   
# 2	0020c3	   |      |   5950
# 3	00393f	   |   5831   |   1541
# 4	004f57	   |      |   1783, 3690
```

The next is about test_summary


```r
# test_summary is all about test set. It has 669 rows. Each row shows the information about where the userID appeared and where the coupons appeared.

# It is hard to paste test_summary here. So I will fake one example to show how to use it
# the 3rd row of test set:     user appeared in 813th row in the train set
#                             coupon1 did not appear before
#                             coupon2 only appeared as couponID2 in 184 row in train set
#                             coupon3 only appeared as couponID3 in 2343 row in train set

# the 6th row of test set:     user never appeared before in the train set
#                             coupon1 only appeared as couponID1 in 5th row in train set
#                         coupon2 appeared as couponID1 in 1st row, as couponID2 in 20th row in train set
#                             coupon3 only appeared as couponID1 in 5th row in train set



#     user	coupon1	         coupon2	                  coupon3
# 3   813	   |  |   	      | 184  |   	               |   |   2343
# 6          | 5|           1 | 20 |                   5|  |      
```


