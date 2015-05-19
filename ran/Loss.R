source("//Users/Ran/Google Drive/ISU/dmc2015/yihua/Loss_caculator.R")
source("//Users/Ran/Google Drive/ISU/dmc2015/weicheng/savePredictions.R")
library(e1071)

# features selected by correlation
imp_corr1 <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/penglh/imp_set1/imp_corr_col_name.rds")
imp_corr3 <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/penglh/imp_set3/imp_rf_SET3.rds")
# features selected by random forest
imp_rf1 <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/penglh/imp_set1/imp_rf150_col_name.rds")
imp_rf3 <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/penglh/imp_set3/imp_rf_SET3.rds")
# features selected by lasso
imp_lasso1 <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/penglh/imp_set1/imp_lasso_col_name.rds")
imp_lasso3 <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/penglh/imp_set3/imp_lasso_col_name.rds")
# features selected by C5.0
imp_c501 <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/penglh/imp_set1/imp_c50_col_name.rds")
imp_c503 <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/penglh/imp_set3/imp_c50_col_name_set3.rds")
# features selected by adaboost
imp_ada1 <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/penglh/imp_set1/imp_gbm_col_name.rds")
imp_ada3 <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/penglh/imp_set3/imp_gbm_set3.rds")
# features selected by conditional random forest
imp_crf1 <- as.character(readRDS("//Users/Ran/Google Drive/ISU/dmc2015/penglh/imp_set1/imp_crf_col_name.rds")$var)
imp_crf3 <- as.character(readRDS("//Users/Ran/Google Drive/ISU/dmc2015/penglh/imp_set3/imp_crf_col_name.rds")$var)

# features
dat1 = readRDS("//Users/Ran/Google Drive/ISU/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.8.rds")
dat3 = readRDS("//Users/Ran/Google Drive/ISU/dmc2015/data/featureMatrix/featMat_based-on-HTVset3_LONG_ver0.6.rds")

svm_pred <- function(dat, col_pred_name, method, file){
  dat_tr_x <- dat$train$X
  dat_tr_y <- dat$train$y
  dat_te_x <- dat$validation$X
  dat_te_y <- dat$validation$y
  dat_cl_x <- dat$class$X
  dat_cl_y <- dat$class$y
  
  col_pred <- which(colnames(dat_tr_x)%in%col_pred_name)
  
  col1 <- which(dat_te_x$couponCol==1)
  col2 <- which(dat_te_x$couponCol==2)
  col3 <- which(dat_te_x$couponCol==3)
  
  dat_x_tr <- dat_tr_x[,col_pred]
  dat_x_te <- dat_te_x[,col_pred]
  dat_x_cl <- dat_cl_x[,col_pred]
  a <- as.numeric(which(apply(dat_x_tr, 2, function(x){length(unique(as.factor(x))) > 1})))
  dat_x_tr <- dat_x_tr[, a]
  dat_x_te <- dat_x_te[, a]
  dat_x_cl <- dat_x_cl[, a]
  
  for (i in 1:length(col_pred)){
    dat_x_tr[, i] <- as.numeric(dat_x_tr[, i])
    dat_x_te[, i] <- as.numeric(dat_x_te[, i])
    dat_x_cl[, i] <- as.numeric(dat_x_cl[, i])
  }
  
  dat_y_tr <- as.factor(dat_tr_y$couponUsed)
  dat_y_te <- as.factor(dat_te_y$couponUsed)
  
  ### SVM ###
  
  # radial kernel with default gamma, training dataset only
  svmfit_radial_T <- svm(x = dat_x_tr, y = dat_y_tr, 
                         kernel = "radial", probability = TRUE)
  # validation error
  validation <- predict(svmfit_radial_T, dat_x_te, probability = TRUE)
  class.T <- predict(svmfit_radial_T, dat_x_cl, probability = TRUE)
  
  prob_radial <- attr(validation, "probabilities")[, 2]
  loss_te <- sum(Loss_calculator(prob_radial[col1],dat_te_y$couponUsed[col1],
                                 prob_radial[col2],dat_te_y$couponUsed[col2],
                                 prob_radial[col3],dat_te_y$couponUsed[col3]))
  prob_class.T <- attr(class.T, "probabilities")[, 2]
  
  # training + validation dataset 
  svmfit_radial_TV <- svm(x = rbind(dat_x_tr, dat_x_te), y = as.factor(c(dat_y_tr, dat_y_te) - 1), 
                          kernel = "radial", probability = TRUE)
  class.TV <- predict(svmfit_radial_TV, dat_x_cl, probability = TRUE)
  prob_class.TV <- attr(class.TV, "probabilities")[, 2]
  
  # save file
  savePred(dat, prob_radial, prob_class.T, prob_class.TV, loss_te, method, file)
}

dat <- dat1
col_pred_name <- imp_c501
file <- "//Users/Ran/Google Drive/ISU/dmc2015/ran/SVM_c50_set1.rds"

svm_pred(dat, col_pred_name, "SVM", file)
