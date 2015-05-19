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

dat <- readRDS('./data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.8.rds')

train.x <- dat$train$X
train.y <- dat$train$y
class.x <- dat$class$X
class.y <- dat$class$y
valid.x <- dat$validation$X
valid.y <- dat$validation$y
na.col.train <- unique(which(is.na(train.x), arr.ind=TRUE)[,2])
na.col.valid <- unique(which(is.na(valid.x), arr.ind=TRUE)[,2])
train.x <- train.x[,-c(na.col.train, na.col.valid)]
valid.x <- valid.x[,-c(na.col.train, na.col.valid)]
na.col.class <- unique(which(is.na(class.x), arr.ind=TRUE)[,2])

# names(class.x[na.col.class]) %in% feature
# 
# length(intersect(names(dat$V_melt), names(dat$C_melt)))

feature.386 <- readRDS('./penglh/imp_set1/imp_corr_col_name.rds')
# feature.318 <- as.character(feature.318$col_name)
# feature.c50 <- readRDS('./penglh/imp_c50_col_name.rds')
# feature.c50 <- as.character(feature.c50)
# feature.lasso <- readRDS('./penglh/imp_lasso_col_name_set1.rds')
# feature.ada <- readRDS('./penglh/imp_ada.rds')
# feature.ada <- names(feature.ada)
# feature.305 <- readRDS('./penglh/imp_rf_SET3.rds')
# feature.lasso.set3 <- readRDS('./penglh/imp_lasso_col_name_set3.rds')
# feature.c50.set3 <- readRDS('./penglh/imp_c50_col_name_set3.rds')
# feature.gbm.set1 <- readRDS('./weicheng/data/imp_gbm_set1.rds')
# feature.gbm.set3 <- readRDS('./weicheng/data/imp_gbm_set3.rds')
# feature.crf <- readRDS('./pete/predictions/importance.rds')
# feature.crf <- feature.crf[order(-feature.crf$h1_imp),]
# feature.crf.set1 <- as.character(feature.crf$var[1:150])
# feature.v4.set1 <- readRDS('./penglh/imp_corr_v4.rds')
# feature.rf.v4.set1 <- readRDS('./penglh/imp_set1_v4/imp_rf_set1_v4.rds')
# feature.lasso.v4.set1 <- readRDS('./penglh/imp_set1_v4/imp_lasso_set1_v4.rds')
# feature.c50.v4.set1 <- readRDS('./penglh/imp_set1_v4/imp_c50_set1_v4.rds')

feature <- intersect(feature.386, names(train.x))

length(feature)

rf <- randomForest(x=train.x[,feature],
                   y=as.factor(train.y$couponUsed),
                   xtest=valid.x[,feature],
                   ytest=as.factor(valid.y$couponUsed),
                   keep.forest=TRUE, ntree=500)
rf.pred <- predict(rf, valid.x[,feature], type="prob")
rf.pred <- predict(rf, class.x[,feature], type="prob")
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
varImpPlot(rf)
y = as.vector(rf$importance)
names(y)=rownames(rf$importance)
y <- sort(y, decreasing=TRUE)
v <- names(y[1:150])
saveRDS(names(y), './penglh/imp_set1/imp_rf150_col_name.rds')
