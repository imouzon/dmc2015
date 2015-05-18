#------------------------------**------------------------------# 
#
# DMC_2015: coupon feature (combined columns)
# dmc2015_v_5_wsrf.R
# 
#------------------------------**------------------------------# 

library(ggplot2)
library(lubridate)
library(dplyr)
library(fitdistrplus)
library(randomForest)
library(wsrf)

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

train <- cbind(train.x[,feature], as.factor(train.y$couponUsed))
names(train)[ncol(train)] <- 'couponUsed'
valid <- cbind(valid.x[,feature], as.factor(valid.y$couponUsed))
names(valid)[ncol(valid)] <- 'couponUsed'

rf1 <- wsrf(couponUsed~., data=train, ntrees=1000)
rf1.pred <- predict(rf1, newdata=valid[,-ncol(valid)], 
                    type="prob")
loss <- Loss_calculator(coupon1pred=rf1.pred[seq(1,nrow(valid),by=3)], 
                        coupon1true=as.numeric(valid$couponUsed[seq(1,nrow(valid),by=3)])-1,
                        coupon2pred=rf1.pred[seq(2,nrow(valid),by=3)], 
                        coupon2true=as.numeric(valid$couponUsed[seq(2,nrow(valid),by=3)])-1,
                        coupon3pred=rf1.pred[seq(3,nrow(valid),by=3)], 
                        coupon3true=as.numeric(valid$couponUsed[seq(3,nrow(valid),by=3)])-1)
sum(loss)
