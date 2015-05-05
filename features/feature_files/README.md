Store features you create as RDS files hear.

RDS files have many advantages over CSV.
-  They are easier to write
-  They are easier to read
-  They preserve formats like factor, numeric, logical, etc.

*No one cares if you use don't write a CSV - just write and RDS*

Here's how:
```r
#save a data set with just orderID and features
tr = tr[,c("orderID","feature1","feature2")]
cl = cl[,c("orderID","feature1","feature2")]

#put it into a list named train and class
features = list("train" = tr, "class" = cl)

#save it as an RDS file
saveRDS(features,file="./feautres/feature_files/myFeatures.rds")
```
It's that easy
