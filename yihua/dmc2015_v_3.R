#------------------------------**------------------------------# 
#
# DMC_2015: coupon feature (combined columns)
# dmc2015_v_3.R
# 
#------------------------------**------------------------------# 

library(ggplot2)
library(lubridate)
library(dplyr)
library(fitdistrplus)
library(randomForest)
library(e1071)

setwd('/Users/yihuali/Documents/dmc2015')

dat <- readRDS('./data//featureMatrix//featMat_based-on-HTVset1_LONG_ver0.3.rds')
train.x <- dat$train$X
train.y <- dat$train$y
class.x <- dat$class$X
class.y <- dat$class$y
valid.x <- dat$validation$X
valid.y <- dat$validation$y

feature.318 <- readRDS('./penglh/imp_rf_col.rds')
feature.318 <- as.character(feature.318$col_name)
feature.c50 <- readRDS('./penglh/imp_c50_col_name.rds')
feature.c50 <- as.character(feature.c50)
feature.lasso <- readRDS('./penglh/imp_lasso_col_name_set1.rds')
feature.ada <- readRDS('./penglh/imp_ada.rds')
feature.ada <- names(feature.ada)
feature.305 <- readRDS('./penglh/imp_rf_SET3.rds')
feature.lasso.set3 <- readRDS('./penglh/imp_lasso_col_name_set3.rds')
feature.c50.set3 <- readRDS('./penglh/imp_c50_col_name_set3.rds')

feature <- intersect(feature.318, names(train.x))
length(feature)
rf <- randomForest(x=train.x[,feature],
                   y=as.factor(train.y$couponUsed),
                   xtest=valid.x[,feature],
                   ytest=as.factor(valid.y$couponUsed),
                   keep.forest=TRUE)
rf.pred <- predict(rf, valid.x[,feature], type="prob")

result <- data.frame(orderID=valid.y$orderID,
                     couponUsed=valid.y$couponUsed, 
                     couponcol=valid.x$couponCol, 
                     predicted=rf.pred[,2])
loss <- Loss_calculator(coupon1pred=result$predicted[result$couponcol==1], 
                        coupon1true=result$couponUsed[result$couponcol==1],
                        coupon2pred=result$predicted[result$couponcol==2], 
                        coupon2true=result$couponUsed[result$couponcol==2],
                        coupon3pred=result$predicted[result$couponcol==3], 
                        coupon3true=result$couponUsed[result$couponcol==3])
sum(loss)
# 305 6561.542
# c50 6533.521
# rf 100 6566.606
# lasso 6634.932
# AdaBoost 

y = as.vector(rf$importance)
names(y)=rownames(rf$importance)
y <- sort(y, decreasing=TRUE)
v <- names(y[1:100])
saveRDS(names(y), './penglh/imp_rf_SET3.rds')

train <- cbind(train.x[,feature], as.factor(train.y$couponUsed))
names(train)[ncol(train)] <- 'couponUsed'
valid <- cbind(valid.x[,feature], as.factor(valid.y$couponUsed))
names(valid)[ncol(valid)] <- 'couponUsed'

library(wsrf)
rf1 <- wsrf(couponUsed~., data=train, ntrees=1000)
