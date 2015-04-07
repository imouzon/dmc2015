setwd("//Users/Ran/Documents/ISU/dmc2015/")
train <- read.table("data/raw_data/DMC_2015_orders_train.txt", head = T, sep = "|")
class <- read.table("data/raw_data/DMC_2015_orders_class.txt", head = T, sep = "|")

names(train)
summary(train)
attach(train)
nlevels(orderTime)
train[orderTime %in% names(table(orderTime)[table(orderTime) == 2]),]

nlevels(train$couponID1)
