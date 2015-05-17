### adaboost long ver0.3 ###
d = readRDS("../data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.3.rds")
imp_rf = readRDS("../penglh/imp_rf_col.rds")
imp_rf$col_name = as.character(imp_rf$col_name)

T.X = d$train$X[, imp_rf$col_name]
T.y = d$train$y
V.X = d$validation$X[, imp_rf$col_name]
V.y = d$validation$y

dat = cbind(T.X, couponUsed = as.factor(T.y$couponUsed))
dat.val = cbind(V.X, couponUsed = as.factor(V.y$couponUsed))
library(gbm)
dmc.adaboost <- gbm(couponUsed~., data=dat, distribution="adaboost", n.trees=5000)
dmc.adaboost.pred <- predict(dmc.adaboost, newdata = dat.val, type="response", n.trees=5000)
