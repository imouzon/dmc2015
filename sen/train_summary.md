Train set summary
========================================================

Try to gather some information.



```r
# read data

train = read.delim("/Users/senzhou/Desktop/dmc15/DMC 2015/DMC_2015_orders_train.txt",header = T, sep = "|")
test = read.delim("/Users/senzhou/Desktop/dmc15/DMC 2015/DMC_2015_orders_class.txt",header = T, sep = "|")

# clean the data:
# 1. change factor to character

i = sapply(train, is.factor)
train[,i] = sapply(train[,i], as.character)

j = sapply(test, is.factor)
test[,j] = sapply(test[,j], as.character)

# 2. truncate the character to easily work with

## take some columns out first

tr = train[,c(2:5,13,21,29:32)]
te = test[,c(2:5,13,21,29:32)]


trunc = function(d){
  for (i in c(2,4,5,6)){
    d[,i] = strtrim(d[,i], 6)
  }
  return(d)
} 

tr1 = trunc(tr)
te1 = trunc(te)
```



```r
# work with time

library(lubridate)

diff = function(d){
  diff = as.numeric(ymd_hms(d$orderTime) - ymd_hms(d$couponsReceived))
  return(diff)
}

tr1$gap = diff(tr1)
te1$gap = diff(te1)

tr1$orderDay = wday(tr1$orderTime, label = T)
tr1$recDay = wday(tr1$couponsReceived, label = T)
te1$orderDay = wday(te1$orderTime, label = T)
te1$recDay = wday(te1$couponsReceived, label = T)


# reorder the columns to better work with

tr2 = tr1[c(3,1,13,12,11,2,4:10)]
te2 = te1[c(3,1,13,12,11,2,4:10)]
```



```r
# 

uni_user = sort(unique(tr2$userID))
uni_coupon = sort(unique(c(tr2$couponID1, tr2$couponID2, tr2$couponID3)))





u1 = list()

user_summary = data.frame(user = character(), 
                           index = character(), 
                           stringsAsFactors = F)

for (i in 1:length(uni_user)){
  u1[[i]] = grep(uni_user[i], tr2[,6])     
  
  user_summary[i,1] = uni_user[i]
  user_summary[i,2] = toString(u1[[i]])
}



coup1 = list()

coupon_summary = data.frame(coupon = character(), 
                           index = character(), 
                           stringsAsFactors = F)

for (i in 1:length(uni_coupon)){

  coup1[[i]] = lapply(tr2[7:9],  function(x) grep(uni_coupon[i], x)) 
  mid = coup1[[i]]            # use this for short written later
  coupon_summary[i,1] = uni_coupon[i]
  coupon_summary[i,2] = paste(toString(mid[[1]]),toString(mid[[2]]),toString(mid[[3]]), sep = "   |   ")

}  



library(xlsx)
```

```
## Loading required package: rJava
## Loading required package: xlsxjars
```

```r
write.xlsx(user_summary, "/Users/senzhou/Desktop/dmc15/user_summary.xlsx")
write.xlsx(coupon_summary, "/Users/senzhou/Desktop/dmc15/coupon_summary.xlsx")
```

