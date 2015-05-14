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

dat <- readRDS('./data/featureMatrix/HTVset3.rds')
H <- dat$H
T <- dat$T
V <- dat$V
C <- dat$C
H_melt <- stackCoupons2(H, idcols = c(1:4, 32:49))
T_melt <- stackCoupons2(T, idcols = c(1:4, 32:49))
V_melt <- stackCoupons2(V, idcols = c(1:4, 32:49))
C_melt <- stackCoupons2(C, idcols = c(1:4, 32:49))

Feature <- addFeatures_HTV(H_melt, T_melt, V_melt)

any(is.na(Feature$H_melt))
any(is.na(Feature$T_melt))
any(is.na(Feature$V_melt))
names(Feature$H_melt)[80]
Feature$H_melt <- Feature$H_melt[,-80]
Feature$T_melt <- Feature$T_melt[,-80]
Feature$V_melt <- Feature$V_melt[,-80]
N <- ncol(Feature$H_melt)
for (i in 1:N) {
  if (length(unique(Feature$H_melt[,i])) == 1 ||
        length(unique(Feature$H_melt[,i])) == 1 ||
        length(unique(Feature$H_melt[,i])) == 1) {
    Feature$H_melt <- Feature$H_melt[,-i]
    Feature$T_melt <- Feature$T_melt[,-i]
    Feature$V_melt <- Feature$V_melt[,-i]
  }
}

rf1 <- randomForest(x=Feature$T_melt[,c(34,36:404)], 
                    y=as.factor(Feature$T_melt$couponUsed), 
                    xtest=Feature$V_melt[,c(34,36:404)], 
                    ytest=as.factor(Feature$V_melt$couponUsed), 
                    ntree=500, mtry=120)
rf1$confusion
rf1$test$confusion
x <- table(as.factor(Feature$V_melt$couponUsed), rf1$test$predicted)
1 - sum(diag(x))/sum(x)

varImpPlot(rf1)
y = as.vector(rf1$importance)
names(y)=rownames(rf1$importance)
y <- sort(y, decreasing=TRUE)
head(y,100)

saveRDS(Feature, './yihua/HTVmelt3_Combn_UniqueUser.rds')
