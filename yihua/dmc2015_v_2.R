#------------------------------**------------------------------# 
#
# DMC_2015: coupon feature (combined columns)
# dmc2015_v_2.R
# 
#------------------------------**------------------------------# 

library(ggplot2)
library(lubridate)
library(dplyr)
library(fitdistrplus)
library(randomForest)

setwd('/Users/yihuali/Documents/dmc2015')

dat <- readRDS('./data/featureMatrix/HTVset2.rds')
H <- dat$H
T <- dat$T
V <- dat$V
C <- dat$C
H_melt <- stackCoupons2(H, idcols = c(1:4, 32:49))
T_melt <- stackCoupons2(T, idcols = c(1:4, 32:49))
V_melt <- stackCoupons2(V, idcols = c(1:4, 32:49))
C_melt <- stackCoupons2(C, idcols = c(1:4, 32:49))

Feature <- addFeatures_HTVC(H_melt, T_melt, V_melt, C_melt)

any(is.na(Feature$H_melt))
any(is.na(Feature$T_melt))
any(is.na(Feature$V_melt))
sum(is.na(Feature$C_melt))/nrow(Feature$C_melt)
names(Feature$H_melt)[80]
Feature$H_melt <- Feature$H_melt[,-80]
Feature$T_melt <- Feature$T_melt[,-80]
Feature$V_melt <- Feature$V_melt[,-80]
Feature$C_melt <- Feature$C_melt[,-80]
N <- ncol(Feature$H_melt)
col.remove <- c()
for (i in names(Feature$H_melt)[33:N]) {
  if (i %in% names(Feature$C_melt)) {
    if (length(unique(Feature$H_melt[,i]))==1 ||
          length(unique(Feature$T_melt[,i]))==1 ||
          length(unique(Feature$V_melt[,i]))==1 ||
          length(unique(Feature$C_melt[,i]))==1) {
      col.remove <- c(col.remove, i)
    }
  }
}
Feature1 <- Feature
remove.num <- which(names(Feature$H_melt) %in% col.remove)
Feature$H_melt <- Feature$H_melt[,-remove.num]
Feature$T_melt <- Feature$T_melt[,-remove.num]
Feature$V_melt <- Feature$V_melt[,-remove.num]
Feature$C_melt <- Feature$C_melt[,-remove.num]

saveRDS(Feature, './yihua/HTVCmelt2_Combn_UniqueUser.rds')

### read and random forest ###
Feature <- readRDS('./yihua/HTVmelt3_Combn_UniqueUser.rds')
rf1 <- randomForest(x=Feature$T_melt[,c(7,8,12,13,21,34,36:ncol(Feature$T_melt))], 
                    y=as.factor(Feature$T_melt$couponUsed), 
                    xtest=Feature$V_melt[,c(7,8,12,13,21,34,36:ncol(Feature$T_melt))], 
                    ytest=as.factor(Feature$V_melt$couponUsed), 
                    ntree=500, mtry=120, maxnodes=50,
                    keep.forest=TRUE)
rf.predicted <- predict(rf1, Feature$V_melt[,c(7,8,12,13,21,34,36:ncol(Feature$T_melt))], type="prob")
rf1
rf1$confusion
rf1$test$confusion
x <- table(as.factor(Feature$V_melt$couponUsed), rf1$test$predicted)
1 - sum(diag(x))/sum(x)

varImpPlot(rf1)
y = as.vector(rf1$importance)
names(y)=rownames(rf1$importance)
y <- sort(y, decreasing=TRUE)
head(y,100)

y1 <- names(y[1:100])
y2 <- names(y[1:100])
y3 <- names(y[1:100])

y <- intersect(y1,y2)
y <- intersect(y,y3)

length(grep('*nUser',y))-length(grep('*nUserUsed',y))
length(grep('*nUserUsed',y))
length(grep('*prob',y))
length(grep('*Twice',y))

# probability version
result <- data.frame(orderID=Feature$V_melt$orderID, 
                     couponUsed=Feature$V_melt$couponUsed, 
                     couponcol=rep(c(1,2,3),568), 
                     predicted=rf.predicted[,2])
# 0-1 version
result1 <- data.frame(orderID=Feature$V_melt$orderID, 
                     couponUsed=Feature$V_melt$couponUsed, 
                     couponcol=rep(c(1,2,3),568), 
                     predicted=rf.predicted[,2]>0.5)

roc <- ROC_curve(result$predicted, result$couponUsed)
plot(roc$x, roc$y, type='s')
auc <- AUC(result$predicted, result$couponUsed)

Loss_calculator(coupon1pred=result$predicted[result$couponcol==1], 
                coupon1true=result$couponUsed[result$couponcol==1],
                coupon2pred=result$predicted[result$couponcol==2], 
                coupon2true=result$couponUsed[result$couponcol==2],
                coupon3pred=result$predicted[result$couponcol==3], 
                coupon3true=result$couponUsed[result$couponcol==3])
Loss_calculator(coupon1pred=result1$predicted[result1$couponcol==1], 
                coupon1true=result1$couponUsed[result1$couponcol==1],
                coupon2pred=result1$predicted[result1$couponcol==2], 
                coupon2true=result1$couponUsed[result1$couponcol==2],
                coupon3pred=result1$predicted[result1$couponcol==3], 
                coupon3true=result1$couponUsed[result1$couponcol==3])

plot(result$predicted, col=result$couponUsed+2)
boxplot(result$predicted~as.factor(result$couponUsed))

# based on clustering results
Feature$T_melt$couponcol <- rep(c(1,2,3), nrow(Feature$T_melt)/3)
Feature$T_melt$cluster <- hc_s_result[hc_s_result$index=="T", 5]
Feature$V_melt$couponcol <- rep(c(1,2,3), nrow(Feature$V_melt)/3)
Feature$V_melt$cluster <- hc_s_result[hc_s_result$index=="V", 5]

nclust <- max(max(Feature$T_melt$cluster), max(Feature$V_melt$cluster))
test.error <- c()
v.tmp <- vector('list', nclust)
result <- vector('list', nclust)
for (i in 1:nclust) {
  train <- subset(Feature$T_melt, cluster==i)
  test <- subset(Feature$V_melt, cluster==i)
  if (nrow(train)!=0 && nrow(test)!=0) {
    
    rf1 <- randomForest(x=train[,c(7,8,12,13,21,34,36:(ncol(train)-2))], 
                        y=as.factor(train$couponUsed), 
                        xtest=test[,c(7,8,12,13,21,34,36:(ncol(train)-2))], 
                        ytest=as.factor(test$couponUsed), 
                        ntree=500, mtry=120, maxnodes=50,
                        keep.forest=TRUE)
    rf.pred.prob <- predict(rf1, test[,c(7,8,12,13,21,34,36:(ncol(train)-2))], type="prob")
    x <- table(as.factor(test$couponUsed), rf1$test$predicted)
    test.error[i] <- 1 - sum(diag(x))/sum(x)
    
    y.tmp = as.vector(rf1$importance)
    names(y.tmp)=rownames(rf1$importance)
    y.tmp <- sort(y.tmp, decreasing=TRUE)
    v.tmp[[i]] <- names(y.tmp[1:100])
    
    result[[i]] <- data.frame(orderID=test$orderID, 
                              couponUsed=test$couponUsed, 
                              couponcol=test$couponcol,
                              predicted=rf.pred.prob[,2], 
                              predicted1=rf1$test$predicted)
  }
}

combn.result <- rbind(result[[1]], result[[2]], result[[3]], result[[4]], result[[5]], result[[6]])
for(i in 1:6){
  cat(table(result[[i]]$couponUsed, result[[i]]$predicted1),"\n")
}
table(combn.result$couponUsed, combn.result$predicted1)


roc <- ROC_curve(result[[3]]$predicted, result[[3]]$couponUsed)
plot(roc$x, roc$y, type='s')
auc <- AUC(result[[3]]$predicted, result[[3]]$couponUsed)

Loss_calculator(coupon1pred=combn.result$predicted[combn.result$couponcol==1], 
                coupon1true=combn.result$couponUsed[combn.result$couponcol==1],
                coupon2pred=combn.result$predicted[combn.result$couponcol==2], 
                coupon2true=combn.result$couponUsed[combn.result$couponcol==2],
                coupon3pred=combn.result$predicted[combn.result$couponcol==3], 
                coupon3true=combn.result$couponUsed[combn.result$couponcol==3])
Loss_calculator(coupon1pred=0.2, coupon2pred=0.2, coupon3pred=0.2, 
                coupon1true=combn.result$couponUsed[combn.result$couponcol==1],
                coupon2true=combn.result$couponUsed[combn.result$couponcol==2],
                coupon3true=combn.result$couponUsed[combn.result$couponcol==3])
# Use just the important features
varImp <- intersect(intersect(intersect(v.tmp[[1]], v.tmp[[3]]), v.tmp[[5]]), v.tmp[[6]])

rf1 <- randomForest(x=Feature$T_melt[,varImp], 
                    y=as.factor(Feature$T_melt$couponUsed), 
                    xtest=Feature$V_melt[,varImp], 
                    ytest=as.factor(Feature$V_melt$couponUsed), 
                    ntree=200, mtry=16, maxnodes=50,
                    keep.forest=TRUE)
rf.predicted <- predict(rf1, Feature$V_melt[,varImp], type="prob")
rf1
rf1$confusion
rf1$test$confusion
x <- table(as.factor(Feature$V_melt$couponUsed), rf1$test$predicted)
1 - sum(diag(x))/sum(x)

# probability version
result <- data.frame(orderID=Feature$V_melt$orderID, 
                     couponUsed=Feature$V_melt$couponUsed, 
                     couponcol=rep(c(1,2,3),568), 
                     predicted=rf.predicted[,2],
                     predicted1=rf.predicted[,2]>0.5)
table(result$couponUsed, result$predicted1)

roc <- ROC_curve(result$predicted, result$couponUsed)
plot(roc$x, roc$y, type='s')
auc <- AUC(result$predicted, result$couponUsed)

Loss_calculator(coupon1pred=result$predicted[result$couponcol==1], 
                coupon1true=result$couponUsed[result$couponcol==1],
                coupon2pred=result$predicted[result$couponcol==2], 
                coupon2true=result$couponUsed[result$couponcol==2],
                coupon3pred=result$predicted[result$couponcol==3], 
                coupon3true=result$couponUsed[result$couponcol==3])
Loss_calculator(coupon1pred=result$predicted1[result$couponcol==1], 
                coupon1true=result$couponUsed[result$couponcol==1],
                coupon2pred=result$predicted1[result$couponcol==2], 
                coupon2true=result$couponUsed[result$couponcol==2],
                coupon3pred=result$predicted1[result$couponcol==3], 
                coupon3true=result$couponUsed[result$couponcol==3])

plot(result$predicted, col=result$couponUsed+2)

result$predicted[result$predicted>0.3] <- 0.5
Loss_calculator(coupon1pred=result$predicted[result$couponcol==1], 
                coupon1true=result$couponUsed[result$couponcol==1],
                coupon2pred=result$predicted[result$couponcol==2], 
                coupon2true=result$couponUsed[result$couponcol==2],
                coupon3pred=result$predicted[result$couponcol==3], 
                coupon3true=result$couponUsed[result$couponcol==3])

boxplot(result$predicted~as.factor(result$couponUsed))
