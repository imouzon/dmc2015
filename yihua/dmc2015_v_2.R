#------------------------------**------------------------------# 
#
# DMC_2015: coupon feature (combined columns)
# dmc2015_v_2.R
# 
#------------------------------**------------------------------# 

library(ggplot2)
library(plyr)
library(lsr)
unlibrary(plyr)
library(lubridate)
library(dplyr)
library(fitdistrplus)

setwd('/Users/yihuali/Documents/dmc2015')

dat <- readRDS('./data/featureMatrix/HTVset1.rds')
H <- dat$H
T <- dat$T
V <- dat$V
C <- dat$C
H_melt <- stackCoupons2(H, idcols = c(1:4, 32:49))
T_melt <- stackCoupons2(T, idcols = c(1:4, 32:49))
V_melt <- stackCoupons2(V, idcols = c(1:4, 32:49))
C_melt <- stackCoupons2(C, idcols = c(1:4, 32:49))

Feature <- addFeatures_HTV(H_melt, T_melt, V_melt)

rf1 <- randomForest(x=Feature$T_melt[,c(34,36:494)], 
                    y=as.factor(Feature$T_melt$couponUsed), 
                    xtest=Feature$V_melt[,c(34,36:494)], 
                    ytest=as.factor(Feature$V_melt$couponUsed), 
                    ntree=500, mtry=200)
table(rf1$test$predicted, as.factor(Feature$V_melt$couponUsed))
(3169+58)/(3169+58+104+704)
varImpPlot(rf1)
