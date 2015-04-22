#--------------------------------------**--------------------------------------#
#  File Name: load_data_from_raw.R
#  Purpose:
#
#  Creation Date: 15-04-2015
#  Last Modified: Wed Apr 15 13:54:39 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#

#training set ("historical data")
trn = read.delim("../data/raw_data/DMC_2015_orders_train.txt", stringsAsFactors = FALSE, sep = "|", quote = "")

#test set ("future data")
tst = read.delim("../data/raw_data/DMC_2015_orders_class.txt", stringsAsFactors = FALSE, sep = "|", quote = "")

source("~/dmc2015/ian/R/TimeFeatures.R")

#Whatever you do to the training set
trn = TimeFeatures(trn,"orderTime")
trn = TimeFeatures(trn,"couponsReceived")

#try if you can to do the same to the test set
tst = TimeFeatures(tst,"orderTime")
tst = TimeFeatures(tst,"couponsReceived")

#add batch information
source("~/dmc2015/ian/R/GetBatchInfo.R")
batchres = GetBatchInfo("2015-01-06 00:00:00")
trn = batchres$train
tst = batchres$test

trn$TimeBtwnSentRec = difftime(trn$couponsReceived,trn$couponsSent,units='hours')
trn$TimeBtwnRecExpire = difftime(trn$couponsExpire,trn$couponsReceived,units='hours')
trn$TimeBtwnRecOrder = difftime(trn$orderTime,trn$couponsReceived,units='hours')
trn$TimeBtwnOrderExpire = difftime(trn$couponsExpire,trn$orderTime,units='hours')

tst$TimeBtwnSentRec = difftime(tst$couponsReceived,tst$couponsSent,units='hours')
tst$TimeBtwnRecExpire = difftime(tst$couponsExpire,tst$couponsReceived,units='hours')
tst$TimeBtwnRecOrder = difftime(tst$orderTime,tst$couponsReceived,units='hours')
tst$TimeBtwnOrderExpire = difftime(tst$couponsExpire,tst$orderTime,units='hours')
