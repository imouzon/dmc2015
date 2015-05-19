#------------------------------**------------------------------# 
#
# DMC_2015: coupon feature (combined columns)
# dmc2015_v_6_twostage.R
# 
#------------------------------**------------------------------# 

library(ggplot2)
library(lubridate)
library(dplyr)
library(fitdistrplus)
library(randomForest)

setwd('/Users/yihuali/Documents/dmc2015')

dat <- readRDS('./data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.3.rds')
train.x <- dat$train$X
train.y <- dat$train$y
class.x <- dat$class$X
class.y <- dat$class$y
valid.x <- dat$validation$X
valid.y <- dat$validation$y

feature.318 <- readRDS('./penglh/imp_rf_col.rds')

sum((pred.c50<0.6) + (pred.c50>0.4))
dat_x_tmp <- dat_x_te[(pred.c50<0.6 && pred.c50>0.4),]
dat_y_tmp <- dat_y_te[(pred.c50<0.6 && pred.c50>0.4)]
dat_x_tr2 <- dat_x_tmp[1:700,]
dat_y_tr2 <- dat_y_tmp[1:700]
dat_x_te2 <- dat_x_tmp[-(1:700),]
dat_y_te2 <- dat_y_tmp[-(1:700)]

col_pca1 <- which(apply(dat_x_tr2,2,function(x) length(unique(x)))<5)
col_pca2 <- which(apply(dat_x_te2,2,function(x) length(unique(x)))<5)
dat_x_te2 <- dat_x_te2[,-c(col_pca1,col_pca2)]
dat_x_tr2 <- dat_x_tr2[,-c(col_pca1,col_pca2)]

pred.pca <- classifierPCA(dat_x_te2, dat_x_tr2, dat_y_tr2, K=45)

sum((pred.pca-as.numeric(dat_y_te2)+1)^2)
tmp <- pred.c50[pred.c50<0.1]
sum((tmp[-(1:700)]-as.numeric(dat_y_te2)+1)^2)
