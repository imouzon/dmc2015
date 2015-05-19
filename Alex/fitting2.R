library(dplyr)
library(gbm)
library(e1071)
library(party)

setwd("~/Documents/dmc2015")
#set1
a = readRDS("/Users/MKULTRA/Documents/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.3.rds")
a_wide = readRDS("/Users/MKULTRA/Documents/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_WIDE_ver0.3.rds")

#set2
b = readRDS("/Users/MKULTRA/Documents/dmc2015/data/featureMatrix/featMat_based-on-HTVset2_LONG_ver0.3.rds")
b_wide = readRDS("/Users/MKULTRA/Documents/dmc2015/data/featureMatrix/featMat_based-on-HTVset2_WIDE_ver0.3.rds")

#set3
c = readRDS("/Users/MKULTRA/Documents/dmc2015/data/featureMatrix/featMat_based-on-HTVset3_LONG_ver0.3.rds")
c_wide = readRDS("/Users/MKULTRA/Documents/dmc2015/data/featureMatrix/featMat_based-on-HTVset3_WIDE_ver0.3.rds")

#returns the training set from the RDS file
get_set = function(dat, columns) {
	trx = dat$train$X
	try = dat$train$y

	trx = trx %>% select(-firstTimeCoup1Rec, -firstTimeCoup2Rec, -firstTimeCoup3Rec)
	if(all(columns == -1)) trx = trx
	else trx = trx[, columns]
	full = data.frame(couponUsed = try$couponUsed, trx)

	full
}

#returns the validation set from the RDS file
pred_set = function(dat, columns) {
	vx = dat$validation$X
	vy = dat$validation$y

	vx = vx %>% select(-firstTimeCoup1Rec, -firstTimeCoup2Rec, -firstTimeCoup3Rec)
	if(all(columns == -1)) vx = vx
	else vx = vx[, columns]

	full = data.frame(couponUsed = vy$couponUsed, vx)
}

#simple function wrapper for fitting a model
fit_data = function(dat, fn, formula, ...) {
	fn(formula, data = dat, ...)
}

#ian's function to calculate loss
lossFunMethod = function(yval,Xval,method,...) {
  hatmat = as.matrix(matrix(predict(method,newdata = Xval,type="response",...),ncol=3,byrow=TRUE))
  ymat = matrix(yval,ncol=3,byrow=TRUE)

  error = colSums((ymat - hatmat)^2)
  wt = colMeans(ymat)^2
  cat("The coupon error is:  ",sum(error/wt),"\n")
  cat("By column error:      ",error,"\n")
  cat("By column weight:     ",error,"\n\n")
  return(sum(error/wt))
}


run_set3_trials = function(method, colset = 1, ...) {
	bf1 = readRDS("./penglh/imp_rf_SET3.rds")
	bf2 = readRDS("./penglh/imp_lasso_col_name_set3.rds")
	bf3 = readRDS("./penglh/imp_c50_col_name_set3.rds")
	master_set = bf1 %>% intersect(bf2) %>% intersect(bf3)

	if(colset == 1) set = bf1
	else if(colset == 2) set = bf2
	else if(colset == 3) set = bf3
	else if(colset == 4) set = -1
	else set = master_set

	df = get_set(c, set)
	df2 = pred_set(c, set)

	m = fit_data(df, method, couponUsed~., ...)
	p = predict(object = m, newdata = df2, type = "response", ...)
	lossFunMethod(df2$couponUsed, df2[,-1], m, ...)

	return(m)
}
a=run_set3_trials(gbm, colset = 1, n.trees = 1000, distribution="adaboost")
a=run_set3_trials(gbm, colset = 2, n.trees = 10000, distribution="adaboost")
a=run_set3_trials(gbm, colset = 3, n.trees = 10000, distribution="adaboost") ###
a=run_set3_trials(gbm, colset = 4, n.trees = 10000, distribution="adaboost")
a=run_set3_trials(gbm, colset = 1, n.trees = 10000)
a=run_set3_trials(gbm, colset = 2, n.trees = 10000)
a=run_set3_trials(gbm, colset = 3, n.trees = 10000)
a=run_set3_trials(gbm, colset = 4, n.trees = 10000)


save_set3_trial = function(method, colset = 1, ...) {
	bf1 = readRDS("./penglh/imp_rf_SET3.rds")
	bf2 = readRDS("./penglh/imp_lasso_col_name_set3.rds")
	bf3 = readRDS("./penglh/imp_c50_col_name_set3.rds")
	master_set = bf1 %>% intersect(bf2) %>% intersect(bf3)

	if(colset == 1) set = bf1
	else if(colset == 2) set = bf2
	else if(colset == 3) set = bf3
	else if(colset == 4) set = -1
	else set = master_set

	df = get_set(c, set)
	df2 = pred_set(c, set)

	m = fit_data(df, method, couponUsed~., ...) #model with training
	#validation stuff
	v_p = predict(object = m, newdata = df2, type = "response", ...)
	v_p = data.frame(orderID = c$validation$X$orderID,
		couponID = rep(1:3, 945),
		prediction = v_p)
	#error stuff
	err = lossFunMethod(df2$couponUsed, df2[,-1], m, ...)

	#class prediction stuff
	orderID_class = c$class$X$orderID
	couponID_class = c$class$X$couponID
	df3 = rbind(c$train$X, c$validation$X)[,set]
	df3$couponUsed = c(c$train$y$couponUsed, c$validation$y$couponUsed)
	m_v = fit_data(df3, method, couponUsed~., ...) #model with training and validation
	predT = predict(m, c$class$X, type = "response", ...)
	predTV = predict(m_v, c$class$X, type = "response", ...)


	return(list(validation = v_p, err = err, class = data.frame(orderID = orderID_class,
																couponID = rep(1:3, 669),
																adaboost.pred.T = predT,
																adaboost.pred.TV = predTV)))
}
adaboost_c5.0_set3_long=save_set3_trial(gbm, colset = 3, n.trees = 10000, distribution="adaboost") ###
saveRDS(adaboost_c5.0_set3_long, "./predictions/adaboost_c5.0_set3_long_0.3.rds")

adaboost_rf_set3_long=save_set3_trial(gbm, colset = 1, n.trees = 10000, distribution="adaboost") 
saveRDS(adaboost_rf_lasso_set3_long, "./predictions/adaboost_rf_lasso_set3_long_0.3.rds")

adaboost_37col_lasso_set3_long=save_set3_trial(gbm, colset = 2, n.trees = 10000, distribution="adaboost")
saveRDS(adaboost_37col_lasso_set3_long, "./predictions/adaboost_37col_lasso_set3_long_0.3.rds")



