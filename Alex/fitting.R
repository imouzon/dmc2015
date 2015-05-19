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


run_set1_trials = function(method, colset = 1, ...) {
	#bf1 = readRDS("./penglh/imp_rf_col.rds")$col_name
	#bf2 = readRDS("./penglh/imp_lasso_col_name.rds")
	#bf3 = readRDS("./penglh/imp_c50_col_name.rds")
	#bf4 = names(readRDS("./penglh/imp_ada.rds"))
	bf1 = readRDS("penglh/imp_set1_v4/imp_rf_set1_v4.rds")$col_name
	bf2 = readRDS("penglh/imp_set1_v4/imp_lasso_set1_v4.rds")
	bf3 = rownames(readRDS("penglh/imp_set1_v4/imp_c50_set1_v4.rds"))
	bf4 = readRDS("weicheng/data/imp_gbm_set1.rds")
	bf5 = readRDS("penglh/imp_set1_v4/imp_corr_v4.rds")
	master_set = bf1 %>% 
		intersect(bf2) %>% 
		intersect(bf3) %>% 
		intersect(bf4) %>% 
		intersect(bf4)

	if(colset == 1) set = bf1
	else if(colset == 2) set = bf2
	else if(colset == 3) set = bf3
	else if(colset == 4) set = bf4
	else if(colset == 5) set = bf5
	else if(colset == 6) set = -1
	else set = master_set

	df = get_set(a, set)
	df2 = pred_set(a, set)

	m = fit_data(df, method, couponUsed~., ...)
	p = predict(object = m, newdata = df2, type = "response", ...)
	lossFunMethod(df2$couponUsed, df2[,-1], m, ...)

	return(m)
}
run_set1_trials(gbm, colset = 1, n.trees = 5000, distribution="adaboost")
run_set1_trials(gbm, colset = 2, n.trees = 5000, distribution="adaboost")
run_set1_trials(gbm, colset = 3, n.trees = 5000, distribution="adaboost")
run_set1_trials(gbm, colset = 4, n.trees = 5000, distribution="adaboost")
run_set1_trials(gbm, colset = 5, n.trees = 5000, distribution="adaboost")
run_set1_trials(gbm, colset = 6, n.trees = 5000, distribution="adaboost")
run_set1_trials(gbm, colset = 7, n.trees = 5000, distribution="adaboost")



run_set1_trials(gbm, colset = 1, n.trees = 5000)
run_set1_trials(gbm, colset = 2, n.trees = 5000)
run_set1_trials(gbm, colset = 3, n.trees = 5000)
run_set1_trials(gbm, colset = 4, n.trees = 5000)
run_set1_trials(gbm, colset = 5, n.trees = 5000)


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
run_set3_trials(gbm, colset = 1, n.trees = 5000, distribution="adaboost")
run_set3_trials(gbm, colset = 2, n.trees = 5000, distribution="adaboost")
run_set3_trials(gbm, colset = 3, n.trees = 5000, distribution="adaboost")
run_set3_trials(gbm, colset = 4, n.trees = 500, distribution="adaboost")
run_set3_trials(gbm, colset = 1, n.trees = 5000)
run_set3_trials(gbm, colset = 2, n.trees = 5000)
run_set3_trials(gbm, colset = 3, n.trees = 5000)
run_set3_trials(gbm, colset = 4, n.trees = 5000)
run_set3_trials(cforest, colset = 4, control = cforest_unbiased(mtry = 10, ntree = 200))

                

run_set2_trials = function(method, colset = 1, ...) {
	bf1 = readRDS("./alex/important_features_set2_long_adaboost.rds")
	bf1 = bf1[bf1 > 0]
	bf1 = names(bf1)
	if(colset == 1) set = -1
	if(colset == 2) set = bf1

	df = get_set(b, set)
	df2 = pred_set(b, set)

	m = fit_data(df, method, couponUsed~., ...)
	p = predict(object = m, newdata = df2, type = "response", ...)
	lossFunMethod(df2$couponUsed, df2[,-1], m, ...)

	return(m)
}
#run_set2_trials(gbm, colset = 1, n.trees = 10000, distribution = "adaboost")



#cforests
#set1
df1 = get_set(a, -1)
df2 = pred_set(a, -1)
aa = cforest(couponUsed~., data = df1, control = cforest_unbiased(mtry = 10, ntree = 200))
lossFunMethod(df2$couponUsed, df2[,-1], aa)
cforest_imp = varimp(aa)

#set2
df1 = get_set(b, -1)
df2 = pred_set(b, -1)
bb = cforest(couponUsed~., data = df1, control = cforest_unbiased(mtry = 10, ntree = 200))
lossFunMethod(df2$couponUsed, df2[,-1], bb)
cforest_imp_b = varimp(bb)

#set3
df1 = get_set(c, -1)
df2 = pred_set(c, -1)
cc = cforest(couponUsed~., data = df1, control = cforest_unbiased(mtry = 10, ntree = 200))
lossFunMethod(df2$couponUsed, df2[,-1], cc)
cforest_imp_c = varimp(cc)


#save these:
#saveRDS(cforest_imp, "important_features_set1_long_cforest.rds")
#saveRDS(cforest_imp_b, "important_features_set2_long_cforest.rds")
#saveRDS(cforest_imp_c, "important_features_set3_long_cforest.rds")


bf = names(tail(sort(cforest_imp_c), 30))
df = get_set(c, bf)
df2 = pred_set(c, bf)
m = cforest(couponUsed~., data = df, control = cforest_unbiased(mtry = 10, ntree = 200))
lossFunMethod(df2$couponUsed, df2[,-1], m)



