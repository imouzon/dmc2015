get_set = function(dat, columns) {
	trx = dat$train$X
	try = dat$train$y

	trx = trx %>% select(-firstTimeCoup1Rec, -firstTimeCoup2Rec, -firstTimeCoup3Rec)
	trx = trx[, columns]
	full = data.frame(couponUsed = try$couponUsed, trx)

	full
}

pred_set = function(dat, columns) {
	vx = dat$validation$X
	vy = dat$validation$y

	vx = vx %>% select(-firstTimeCoup1Rec, -firstTimeCoup2Rec, -firstTimeCoup3Rec)
	vx = vx[, columns]

	full = data.frame(couponUsed = vy$couponUsed, vx)
}

run_set1_trials = function(method, ...) {
	a = readRDS("/Users/MKULTRA/Documents/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.3.rds")
	set = readRDS("/Users/MKULTRA/Documents/dmc2015/penglh/imp_rf_col.rds")$col_name
	df = get_set(a, set)
	df2 = pred_set(a, set)

	m = fit_data(df, method, couponUsed~., ...)
	p = predict(object = m, newdata = df2, type = "response", ...)
	lossFunMethod(df2$couponUsed, df2[,-1], m, ...)

	return(p)
}
run_set1_trials(gbm, colset = 1, n.trees = 500, distribution="adaboost")
