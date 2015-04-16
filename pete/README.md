## Pete's folder

### Findings
----

#### All coupons expire before Tuesday

```r
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
```

```r
train %>% ggplot(aes(x = wday_couponsReceived, fill = wday_orderTime)) + 
	geom_bar()
```
![img](plot01.jpg)

#### Coupon 1 is used the most, followed by coupon 2, and then coupon 3

```r
sum(train$coupon1Used)
# [1] 1438
sum(train$coupon2Used)
# [1] 1129
sum(train$coupon3Used)
# [1] 1008
```

### Coupon usage rate is greater for people receiving coupons later in the week

```r
train$coupons_used = train$coupon1Used + train$coupon2Used + train$coupon3Used
train %>% ggplot(aes(x = wday_couponsReceived, fill = factor(coupons_used))) + 
	geom_bar()
```
![img](plot02.jpg)
