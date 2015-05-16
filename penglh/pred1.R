library(dplyr)
setwd("C:/Users/Liuhua/Dropbox/DMC2015")
source("./R/functions/classification_function.R")
source("./R/functions/Loss_caculator.R")
dat = readRDS("./R/data/featMat_based-on-HTVset1_LONG_ver0.3.rds")

dat_tr_x <- dat$train$X
dat_tr_y <- dat$train$y
dat_te_x <- dat$validation$X
dat_te_y <- dat$validation$y

dat_tr_x$order_match_class <- as.numeric(dat_tr_x$order_match_class)
dat_te_x$order_match_class <- as.numeric(dat_te_x$order_match_class)


# col_del_name <- readRDS("col_del_name.rds")
# col_del <- which(colnames(dat_tr_x)%in%col_del_name)
# col_pred <- c(1:ncol(dat_tr_x))[-col_del]

imp_rf_col <- readRDS("imp_rf_col.rds")
col_pred_name <- as.character(imp_rf_col$col_name[1:100])
col_pred <- which(colnames(dat_tr_x)%in%col_pred_name)

col1 <- which(dat_te_x$couponCol==1)
col2 <- which(dat_te_x$couponCol==2)
col3 <- which(dat_te_x$couponCol==3)

dat_x_tr <- dat_tr_x[,col_pred]
dat_x_te <- dat_te_x[,col_pred]

dat_y_tr <- as.factor(dat_tr_y$couponUsed)
dat_y_te <- as.factor(dat_te_y$couponUsed)

library(randomForest)
rf1 <- randomForest(dat_x_tr,dat_y_tr,replace=TRUE,ntree=1000,maxnodes=50)
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
# 18780.03

imp_rf_col_names <- attr(rf1$importance,"dimnames")[[1]][which(rank(rf1$importance)>268)]
imp_rf_col <- data.frame("importance"=rf1$importance,"col_name"=attr(rf1$importance,"dimnames")[[1]])
row.names(imp_rf_col) <- NULL

imp_rf_col <- imp_rf_col %>% arrange(-MeanDecreaseGini)

saveRDS(imp_rf_col,"imp_rf_col.rds")
imp_rf_col <- readRDS("imp_rf_col.rds")
par(mar=c(3,25,3,3))
plot(imp_rf_col$MeanDecreaseGini[1:50],c(50:1),yaxt="n",ylab="",xlab="importance")
axis(2, at=c(50:1), labels=imp_rf_col$col_name[1:50],las=2)


# logistic
require(glmnet)
cv.lasso <- cv.glmnet(as.matrix(dat_x_tr),dat_y_tr,family="binomial",nfolds=3,alpha=1,standardize=FALSE)
pred.lasso <- predict(cv.lasso,as.matrix(dat_x_te),s=cv.lasso$lambda.min,type="response")
#pred <- as.numeric(pred.lasso)
pred.lasso[which(pred.lasso<0.5)] = 0
pred.lasso[which(pred.lasso>=0.5)] = 1

1-mean((as.numeric(dat_y_te)-1)==pred.lasso)
table(pred.lasso,as.numeric(dat_y_te))

saveRDS(list("lasso.model"=cv.lasso,"lasso.pred"=pred.lasso,"lasso.loss"=16387.9),
        file="lasso_pred1.rds")

imp_lasso_col_name <- col_pred_name[which(coef(cv.lasso,s=cv.lasso$lambda.min)>0.0000000000001)]
which(imp_rf_col$col_name%in%imp_lasso_col_name)

saveRDS(imp_lasso_col_name,"imp_lasso_col_name.rds")

sum(Loss_calculator(pred.lasso[col1],dat_te_y$couponUsed[col1],
                    pred.lasso[col2],dat_te_y$couponUsed[col2],
                    pred.lasso[col3],dat_te_y$couponUsed[col3]))
# 16529.49

# pca
pred.pca <- classifierPCA(dat_x_te,dat_x_tr,dat_y_tr,K=5)
pred.pca1 <- pred.pca
pred.pca1[which(pred.pca1<0.5)] = 0
pred.pca1[which(pred.pca1>=0.5)] = 1
1-mean(pred.pca1==dat_y_te)
table(pred.pca1,dat_y_te)

sum(Loss_calculator(pred.pca1[col1],dat_te_y$couponUsed[col1],
                    pred.pca1[col2],dat_te_y$couponUsed[col2],
                    pred.pca1[col3],dat_te_y$couponUsed[col3]))
# 16750.30

plot(pred.lasso,pred.pca1)

pred.mean <- (pred.lasso+pred.pca1)/2
sum(Loss_calculator(pred.mean[col1],dat_te_y$couponUsed[col1],
                    pred.mean[col2],dat_te_y$couponUsed[col2],
                    pred.mean[col3],dat_te_y$couponUsed[col3]))

# spca
pred.spca <- classifierSPAPCA(dat_x_te,dat_x_tr,dat_y_tr,K=10,para=rep(50,10))
1-mean(pred.spca==dat_y_te)
table(pred.spca,dat_y_te)


# spls
pred.spls <- classifierSPLS(dat_x_te,dat_x_tr,dat_y_tr)
1-mean(pred.spls==dat_y_te)
table(pred.spls,dat_y_te)


# nsc
pred.nsc <- classifierNSC(dat_x_te,dat_x_tr,dat_y_tr)
1-mean(pred.nsc==dat_y_te)
table(pred.nsc,dat_y_te)


# c5.0
library(C50)
c50 <- C5.0(dat_x_tr,dat_y_tr,trials=10,rules=TRUE)
pred.c50 <- predict(c50,dat_x_te,type="class")
1-mean(pred.c50==dat_y_te)
table(pred.c50,dat_y_te)
pred.c50 <- predict(c50,dat_x_te,type="prob")[,2]
sum(Loss_calculator(pred.c50[col1],dat_te_y$couponUsed[col1],
                    pred.c50[col2],dat_te_y$couponUsed[col2],
                    pred.c50[col3],dat_te_y$couponUsed[col3]))
# 17207.14

imp_c50_col <- C5imp(c50,pct = TRUE)

imp_c50_col_name <- rownames(imp_c50_col)[1:142]
which(imp_rf_col$col_name%in%imp_c50_col_name)
saveRDS(imp_c50_col_name,"imp_c50_col_name.rds")

plot(pred.lasso,pred.c50,col=dat_y_te)




sum(Loss_calculator(pred.nsc[col1],dat_te_y$couponUsed[col1],
                    pred.nsc[col2],dat_te_y$couponUsed[col2],
                    pred.nsc[col3],dat_te_y$couponUsed[col3]))

sum(Loss_calculator(rep(0.2,1345),dat_te_y$couponUsed[col1],
                    rep(0.2,1345),dat_te_y$couponUsed[col2],
                    rep(0.2,1345),dat_te_y$couponUsed[col3]))
