#------------------------------**------------------------------# 
#
# DMC_2015: coupon feature (combined columns)
# dmc2015_v_4_WIDE.R
# 
#------------------------------**------------------------------# 

library(ggplot2)
library(lubridate)
library(dplyr)
library(fitdistrplus)
library(randomForest)
library(e1071)

setwd('/Users/yihuali/Documents/dmc2015')

WIDE <- readRDS('./data/featureMatrix/featMat_based-on-HTVset1_WIDE_ver0.3.rds')
summary(WIDE)
train.x <- WIDE$train$X
train.y <- WIDE$train$y
valid.x <- WIDE$validation$X
valid.y <- WIDE$validation$y
class.x <- WIDE$class$X
class.y <- WIDE$class$y
dim(train.x)
dim(train.y)
dim(valid.x)
dim(valid.y)
dim(class.x)
dim(class.y)

train.x1 <- train.x %>% select(grep("*_1", names(train.x)))
valid.x1 <- valid.x %>% select(grep("*_1", names(valid.x)))
train.x2 <- train.x %>% select(grep("*_2", names(train.x)))
valid.x2 <- valid.x %>% select(grep("*_2", names(valid.x)))
train.x3 <- train.x %>% select(grep("*_3", names(train.x)))
valid.x3 <- valid.x %>% select(grep("*_3", names(valid.x)))
rf1 <- randomForest(x=train.x1, 
                    y=as.factor(train.y$coupon1Used), 
                    xtest=valid.x1,
                    ytest=as.factor(valid.y$coupon1Used),
                    keep.forest=TRUE)
rf2 <- randomForest(x=train.x2, 
                    y=as.factor(train.y$coupon2Used), 
                    xtest=valid.x2,
                    ytest=as.factor(valid.y$coupon2Used),
                    keep.forest=TRUE)
rf3 <- randomForest(x=train.x3, 
                    y=as.factor(train.y$coupon3Used), 
                    xtest=valid.x3,
                    ytest=as.factor(valid.y$coupon3Used),
                    keep.forest=TRUE)
rf1.pred <- predict(rf1, valid.x1, type="prob")
rf2.pred <- predict(rf2, valid.x2, type="prob")
rf3.pred <- predict(rf3, valid.x3, type="prob")
loss <- Loss_calculator(coupon1pred=rf1.pred[,2], 
                        coupon1true=valid.y$coupon1Used,
                        coupon2pred=rf2.pred[,2], 
                        coupon2true=valid.y$coupon2Used,
                        coupon3pred=rf3.pred[,2], 
                        coupon3true=valid.y$coupon3Used)
sum(loss)

v1 <- as.vector(rf1$importance)
names(v1) <- rownames(rf1$importance)
v1 <- sort(v1, decreasing=TRUE)
v2 <- as.vector(rf2$importance)
names(v2) <- rownames(rf2$importance)
v2 <- sort(v2, decreasing=TRUE)
v3 <- as.vector(rf3$importance)
names(v3) <- rownames(rf3$importance)
v3 <- sort(v3, decreasing=TRUE)
vv1 <- sub('(.*)_[0-9]', '\\1', names(v1))
vv2 <- sub('(.*)_[0-9]', '\\1', names(v2))
vv3 <- sub('(.*)_[0-9]', '\\1', names(v3))
v <- intersect(intersect(vv1[1:100], vv2[1:100]), vv3[1:100])
t1 <- which(sub('(.*)_[0-9]', '\\1', names(train.x1)) %in% v)
t2 <- which(sub('(.*)_[0-9]', '\\1', names(train.x2)) %in% v)
t3 <- which(sub('(.*)_[0-9]', '\\1', names(train.x3)) %in% v)

rf1.imp <- randomForest(x=train.x1[,t1], 
                    y=as.factor(train.y$coupon1Used), 
                    xtest=valid.x1[,t1],
                    ytest=as.factor(valid.y$coupon1Used),
                    keep.forest=TRUE)
rf2.imp <- randomForest(x=train.x2[,t2], 
                    y=as.factor(train.y$coupon2Used), 
                    xtest=valid.x2[,t2],
                    ytest=as.factor(valid.y$coupon2Used),
                    keep.forest=TRUE)
rf3.imp <- randomForest(x=train.x3[,t3], 
                    y=as.factor(train.y$coupon3Used), 
                    xtest=valid.x3[,t3],
                    ytest=as.factor(valid.y$coupon3Used),
                    keep.forest=TRUE)
rf1.imp.pred <- predict(rf1.imp, valid.x1[,t1], type="prob")
rf2.imp.pred <- predict(rf2.imp, valid.x2[,t2], type="prob")
rf3.imp.pred <- predict(rf3.imp, valid.x3[,t3], type="prob")

loss <- Loss_calculator(coupon1pred=rf1.imp.pred[,2], 
                        coupon1true=valid.y$coupon1Used,
                        coupon2pred=rf2.imp.pred[,2], 
                        coupon2true=valid.y$coupon2Used,
                        coupon3pred=rf3.imp.pred[,2], 
                        coupon3true=valid.y$coupon3Used)
sum(loss)

transform <- function(x) {
  x1 <- x
  t <- 0.4
  x1[x<=0.5] <- 2*t*x[x<=0.5]
  x1[x>0.5] <- 2*(1-t)*x[x>0.5]-2*(0.5-t)
  return(x1)
}

rf1.imp.trans <- transform(rf1.imp.pred[,2])
rf2.imp.trans <- transform(rf2.imp.pred[,2])
rf3.imp.trans <- transform(rf3.imp.pred[,2])
loss <- Loss_calculator(coupon1pred=rf1.imp.trans, 
                        coupon1true=valid.y$coupon1Used,
                        coupon2pred=rf2.imp.trans, 
                        coupon2true=valid.y$coupon2Used,
                        coupon3pred=rf3.imp.trans, 
                        coupon3true=valid.y$coupon3Used)
sum(loss)

rf1.trans <- transform(rf1.pred[,2])
rf2.trans <- transform(rf2.pred[,2])
rf3.trans <- transform(rf3.pred[,2])
loss <- Loss_calculator(coupon1pred=rf1.trans, 
                        coupon1true=valid.y$coupon1Used,
                        coupon2pred=rf2.trans, 
                        coupon2true=valid.y$coupon2Used,
                        coupon3pred=rf3.trans, 
                        coupon3true=valid.y$coupon3Used)
sum(loss)


cpn1 <- transform(result$predicted[result$couponcol==1])
cpn2 <- transform(result$predicted[result$couponcol==2])
cpn3 <- transform(result$predicted[result$couponcol==3])
loss <- Loss_calculator(coupon1pred=cpn1, 
                        coupon1true=result$couponUsed[result$couponcol==1],
                        coupon2pred=cpn2, 
                        coupon2true=result$couponUsed[result$couponcol==2],
                        coupon3pred=cpn3, 
                        coupon3true=result$couponUsed[result$couponcol==3])
sum(loss)
