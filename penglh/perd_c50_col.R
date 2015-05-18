library(dplyr)
setwd("C:/Users/Liuhua/Dropbox/DMC2015")
source("./R/functions/classification_function.R")
source("./R/functions/Loss_caculator.R")
dat = readRDS("./R/data/featMat_based-on-HTVset1_LONG_ver0.3.rds")
# dat = readRDS("./R/data/featMat_based-on-HTVset3_LONG_ver0.3.rds")
rm(dat)

dat_tr_x <- dat$train$X
dat_tr_y <- dat$train$y
dat_te_x <- dat$validation$X
dat_te_y <- dat$validation$y

dat_tr_x$order_match_class <- as.numeric(dat_tr_x$order_match_class)
dat_te_x$order_match_class <- as.numeric(dat_te_x$order_match_class)

imp_c50_col_name <- readRDS("imp_c50_col_name_all.rds")
col_pred <- which(colnames(dat_tr_x)%in%imp_c50_col_name)

col1 <- which(dat_te_x$couponCol==1)
col2 <- which(dat_te_x$couponCol==2)
col3 <- which(dat_te_x$couponCol==3)

dat_x_tr <- dat_tr_x[,col_pred]
dat_x_te <- dat_te_x[,col_pred]

dat_y_tr <- as.factor(dat_tr_y$couponUsed)
dat_y_te <- as.factor(dat_te_y$couponUsed)


# random forest
library(randomForest)
rf1 <- randomForest(dat_x_tr,dat_y_tr,keep.forest=TRUE)
#pred.rf <- predict(rf1,dat_x_te)
pred.rf <- predict(rf1,dat_x_te,type="prob")[,2]
pred.rf[which(pred.rf<0.5)] = 0
pred.rf[which(pred.rf>=0.5)] = 1

1-mean(pred.rf==dat_y_te)
table(pred.rf,dat_y_te)
varImpPlot(rf1)

sum(Loss_calculator(pred.rf[col1],dat_te_y$couponUsed[col1],
                    pred.rf[col2],dat_te_y$couponUsed[col2],
                    pred.rf[col3],dat_te_y$couponUsed[col3]))

# 16329.25
# 6583.729 6537.597

# logistic
require(glmnet)
cv.lasso <- cv.glmnet(as.matrix(dat_x_tr),dat_y_tr,family="binomial",nfolds=3,alpha=1,standardize=FALSE)
pred.lasso <- predict(cv.lasso,as.matrix(dat_x_te),s=cv.lasso$lambda.min,type="response")[,1]
#pred <- as.numeric(pred.lasso)
pred.lasso[which(pred.lasso<0.5)] = 0
pred.lasso[which(pred.lasso>=0.5)] = 1

1-mean((as.numeric(dat_y_te)-1)==pred.lasso)
table(pred.lasso,as.numeric(dat_y_te))

sum(Loss_calculator(pred.lasso[col1],dat_te_y$couponUsed[col1],
                    pred.lasso[col2],dat_te_y$couponUsed[col2],
                    pred.lasso[col3],dat_te_y$couponUsed[col3]))

pred.lasso1 <- pred.lasso
pred.lasso1[pred.pca1<0.025] <- 0
pred.lasso1[pred.lasso>0.8] <- 1
pred.lasso1[pred.lasso>0.5 & pred.lasso<0.9] <- pred.lasso1[pred.lasso>0.5 & pred.lasso<0.9]+0.1#pred.lasso1[pred.lasso<0.6 & pred.lasso>0.4]+0.01
sum(Loss_calculator(pred.lasso1[col1],dat_te_y$couponUsed[col1],
                    pred.lasso1[col2],dat_te_y$couponUsed[col2],
                    pred.lasso1[col3],dat_te_y$couponUsed[col3]))

# 16191.19
# 6481.005 6432.31

# pca
col_pca1 <- which(apply(dat_x_tr,2,function(x) length(unique(x)))<5)
col_pca2 <- which(apply(dat_x_te,2,function(x) length(unique(x)))<5)
dat_x_te1 <- dat_x_te[,-c(col_pca1,col_pca2)]
dat_x_tr1 <- dat_x_tr[,-c(col_pca1,col_pca2)]

pred.pca <- classifierPCA(dat_x_te1,dat_x_tr1,dat_y_tr,K=45)
pred.pca1 <- pred.pca
pred.pca1[which(pred.pca1<0.5)] = 0
pred.pca1[which(pred.pca1>=0.5)] = 1
1-mean(pred.pca1==dat_y_te)
table(pred.pca1,dat_y_te)

# PCA
# K is the number of principal components used
classifierPCA <- function(xTest,xTrain,yTrain,K=5){ # yTrain is factor
  x1 <- scale(xTrain,center=TRUE,scale=TRUE)
  pcx <- prcomp(x1)
  m <- x1%*%pcx$rotation[,1:K]
  y1 <- as.numeric(yTrain)-1
  #  data <- list(x=m,y=y1)
  data <- data.frame(y1,m)
  colnames(data) <- c("y",paste0("x",1:K))
  glm.pcc <- glm(y~.,data=data,family=binomial(link=logit))
  x2 <- (scale(xTest,center=TRUE,scale=TRUE)%*%pcx$rotation)[,1:K]
  newdata <- data.frame(x2)
  colnames(newdata) <- paste0("x",1:K)
  pred <- predict(glm.pcc,newdata,type="response")
  pred_tr <- predict(glm.pcc,data,type="response")
  return(list("pred_train"=pred_tr,"pred_test"=pred))
}

sum(Loss_calculator(pred.pca1[col1],dat_te_y$couponUsed[col1],
                    pred.pca1[col2],dat_te_y$couponUsed[col2],
                    pred.pca1[col3],dat_te_y$couponUsed[col3]))
# 16178.35
# 6432.791 6431.743

pred.pca2 <- pred.pca1
pred.pca2[pred.lasso>0.8] <- 1
pred.pca2[pred.pca1>0.5 & pred.pca1<0.7] <- pred.pca2[pred.pca1>0.5 & pred.pca1<0.7]+0.1
pred.pca2[pred.pca1<0.5] <- pred.pca2[pred.pca1<0.5]-0.1#pred.lasso1[pred.lasso<0.6 & pred.lasso>0.4]+0.01
sum(Loss_calculator(pred.pca2[col1],dat_te_y$couponUsed[col1],
                    pred.pca2[col2],dat_te_y$couponUsed[col2],
                    pred.pca2[col3],dat_te_y$couponUsed[col3]))

# c5.0
library(C50)
c50 <- C5.0(dat_x_tr,dat_y_tr,trials=20,rules=TRUE)
pred.c50 <- predict(c50,dat_x_te,type="class")
1-mean(pred.c50==dat_y_te)
table(pred.c50,dat_y_te)
pred.c50 <- predict(c50,dat_x_te,type="prob")[,2]
sum(Loss_calculator(pred.c50[col1],dat_te_y$couponUsed[col1],
                    pred.c50[col2],dat_te_y$couponUsed[col2],
                    pred.c50[col3],dat_te_y$couponUsed[col3]))
# 16807.47
# 6717.322 6748.012

pred.ada <- readRDS("./R/data/ada_c50_pred.rds")

sum(Loss_calculator(pred.ada[col1],dat_te_y$couponUsed[col1],
                    pred.ada[col2],dat_te_y$couponUsed[col2],
                    pred.ada[col3],dat_te_y$couponUsed[col3]))

# weight
weight <- seq(0.1,0.9,by=0.1)
sapply(weight,function(x) weighted_prob(pred.lasso,pred.c50,x,dat_te_x,as.numeric(dat_y_te)-1))
sapply(weight,function(x) weighted_prob(pred.lasso,pred.pca1,x,dat_te_x,as.numeric(dat_y_te)-1))
sapply(weight,function(x) weighted_prob(pred.pca1,pred.c50,x,dat_te_x,as.numeric(dat_y_te)-1))
sapply(weight,function(x) weighted_prob(pred.rf,pred.lasso,x,dat_te_x,as.numeric(dat_y_te)-1))
sapply(weight,function(x) weighted_prob(pred.rf,pred.c50,x,dat_te_x,as.numeric(dat_y_te)-1))
sapply(weight,function(x) weighted_prob(pred.rf,pred.pca1,x,dat_te_x,as.numeric(dat_y_te)-1))
sapply(weight,function(x) weighted_prob(pred.ada,pred.rf,x,dat_te_x,as.numeric(dat_y_te)-1))
sapply(weight,function(x) weighted_prob(pred.ada,pred.lasso,x,dat_te_x,as.numeric(dat_y_te)-1))
sapply(weight,function(x) weighted_prob(pred.ada,pred.pca1,x,dat_te_x,as.numeric(dat_y_te)-1))
sapply(weight,function(x) weighted_prob(pred.ada,pred.c50,x,dat_te_x,as.numeric(dat_y_te)-1))

# 0.4*pred.rf+0.6*pred.lasso  16051.68



plot(pred.lasso[1:200],col=1,pch=15+as.numeric(dat_y_te))
points(pred.pca1[1:200],col=2,pch=15+as.numeric(dat_y_te))
points(pred.rf[1:200],col=3,pch=15+as.numeric(dat_y_te))
points(pred.c50[1:200],col=4,pch=15+as.numeric(dat_y_te))
