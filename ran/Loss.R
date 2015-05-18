source("//Users/Ran/Google Drive/ISU/dmc2015/yihua/Loss_caculator.R")
library(e1071)

# features selected by random forest
imp_rf1 <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/penglh/imp_rf_SET3.rds")
imp_rf2 <- imp_rf1[1:100]
# features selected by lasso
imp_lasso <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/penglh/imp_lasso_col_name_set3.rds")
# features selected by C5.0
imp_c50 <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/penglh/imp_c50_col_name_set3.rds")
# features selected by adaboost
imp_ada <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/penglh/imp_ada.rds")
imp_ada <- names(imp_ada)
# features selected by correlation
imp_corr <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/penglh/imp_corr_v4.rds")

# features
dat = readRDS("//Users/Ran/Google Drive/ISU/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.4.rds")

dat_tr_x <- dat$train$X
dat_tr_y <- dat$train$y
dat_te_x <- dat$validation$X
dat_te_y <- dat$validation$y

dat_tr_x$order_match_class <- as.numeric(dat_tr_x$order_match_class)
dat_te_x$order_match_class <- as.numeric(dat_te_x$order_match_class)

col_pred_name <- imp_corr
col_pred <- which(colnames(dat_tr_x)%in%col_pred_name)

col1 <- which(dat_te_x$couponCol==1)
col2 <- which(dat_te_x$couponCol==2)
col3 <- which(dat_te_x$couponCol==3)

dat_x_tr <- dat_tr_x[,col_pred]
dat_x_te <- dat_te_x[,col_pred]
a <- as.numeric(which(apply(dat_x_tr, 2, function(x){length(unique(as.factor(x))) > 1})))
dat_x_tr <- dat_x_tr[, a]
dat_x_te <- dat_x_te[, a]

dat_y_tr <- as.factor(dat_tr_y$couponUsed)
dat_y_te <- as.factor(dat_te_y$couponUsed)


### SVM ###

# polynomial kernel with default order 3
svmfit_polynomial <- svm(x = dat_x_tr, y = dat_y_tr, 
                         kernel = "polynomial", probability = TRUE)
# validation error
pred_polynomial <- predict(svmfit_polynomial, dat_x_te, probability = TRUE)
table_polynomial <- table(pred_polynomial, dat_y_te)
1 - sum(diag(table_polynomial)) / sum(table_polynomial)  # 0.1911
# probability loss
prob_polynomial <- attr(pred_polynomial, "probabilities")[, 2]
sum(Loss_calculator(prob_polynomial[col1],dat_te_y$couponUsed[col1],
                    prob_polynomial[col2],dat_te_y$couponUsed[col2],
                    prob_polynomial[col3],dat_te_y$couponUsed[col3]))

# radial kernel with default gamma
svmfit_radial <- svm(x = dat_x_tr, y = dat_y_tr, 
                     kernel = "radial", probability = TRUE)
# validation error
pred_radial <- predict(svmfit_radial, dat_x_te, probability = TRUE)
table_radial <- table(pred_radial, dat_y_te)
1 - sum(diag(table_radial)) / sum(table_radial)  # 0.1854
# probability loss
prob_radial <- attr(pred_radial, "probabilities")[, 2]
sum(Loss_calculator(prob_radial[col1],dat_te_y$couponUsed[col1],
                    prob_radial[col2],dat_te_y$couponUsed[col2],
                    prob_radial[col3],dat_te_y$couponUsed[col3]))

# radial kernel with half default gamma
svmfit_radial0.5 <- svm(x = dat_x_tr, y = dat_y_tr, 
                        kernel = "radial", probability = TRUE, gamma = svmfit_radial$gamma/2)
# validation error
pred_radial0.5 <- predict(svmfit_radial0.5, dat_x_te, probability = TRUE)
table_radial0.5 <- table(pred_radial0.5, dat_y_te)
1 - sum(diag(table_radial0.5)) / sum(table_radial0.5)  # 0.1854
# probability loss
prob_radial0.5 <- attr(pred_radial0.5, "probabilities")[, 2]
sum(Loss_calculator(prob_radial0.5[col1],dat_te_y$couponUsed[col1],
                    prob_radial0.5[col2],dat_te_y$couponUsed[col2],
                    prob_radial0.5[col3],dat_te_y$couponUsed[col3]))

# radial kernel with double default gamma
svmfit_radial2 <- svm(x = dat_x_tr, y = dat_y_tr, 
                      kernel = "radial", probability = TRUE, gamma = svmfit_radial$gamma * 2)
# validation error
pred_radial2 <- predict(svmfit_radial2, dat_x_te, probability = TRUE)
table_radial2 <- table(pred_radial2, dat_y_te)
1 - sum(diag(table_radial2)) / sum(table_radial2)  # 0.1854
# probability loss
prob_radial2 <- attr(pred_radial2, "probabilities")[, 2]
sum(Loss_calculator(prob_radial2[col1],dat_te_y$couponUsed[col1],
                    prob_radial2[col2],dat_te_y$couponUsed[col2],
                    prob_radial2[col3],dat_te_y$couponUsed[col3]))

