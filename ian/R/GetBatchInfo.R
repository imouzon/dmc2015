#--------------------------------------**--------------------------------------#
#  File Name:
#  Purpose:
#
#  Creation Date: 12-04-2015
#  Last Modified: Wed Apr 22 17:16:26 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#

GetBatchInfo = function(initial_batch.ymd_hms, nbatch=10, weeks2expire = 1, train=trn, test=tst,unts,keep.units=FALSE){
   # start sending coupons: initial_batch.ymd_hms = "2015-01-06 1:00:00"
   #                        initial_batch.ymd_hms = "2015-01-03 1:00:00"
   # How many batches are there?  nbatch = 10
   # how long are coupons valid?  weeks2expire = 1
   # what is the training set?  train = trn
   # what is the test set?  test = tst

   require(lubridate)
   require(ggplot2)
   require(plyr)

   stopifnot(is.POSIXct(ymd_hms(initial_batch.ymd_hms)) & is.POSIXt(ymd_hms(initial_batch.ymd_hms)))

   # expiration date one week after coupons are sent
   couponLengthValid= weeks(weeks2expire)

   # batches go out when the last batch expires
   weeksbtwnbatches = weeks2expire

   #coupons start on 
   batch.start = ymd_hms(initial_batch.ymd_hms)

   # make data frame
   couponBatches = data.frame(
      'couponsSent' = batch.start + (0:(nbatch-1))*couponLengthValid,
      'couponsExpire' = batch.start + 1:nbatch*couponLengthValid,
      'batch' = factor(1:nbatch))

   couponBatches
   #create time interval
   couponBatches$validInterval = with(couponBatches,interval(couponsSent, couponsExpire))

   #give the training set batchID
   train$batchID = 0 
   train$couponsExpire = batch.start + years(1)
   train$couponsSent = batch.start
   for(i in 1:nbatch){
      orderinbatch= train$orderTime %within% couponBatches$validInterval[i] & train$couponsReceived %within% couponBatches$validInterval[i]
      couponinbatch= train$couponsReceived %within% couponBatches$validInterval[i]
      if(sum(orderinbatch) > 0) train$batchID[orderinbatch] = couponBatches$batch[i]
      if(sum(couponinbatch) > 0){
         train$couponsSent[couponinbatch] = couponBatches$couponsSent[i]
         train$couponsExpire[couponinbatch] = couponBatches$couponsExpire[i]
      }
   }
   train$batchID = as.factor(train$batchID)
   train$dataset = "train"

   test$batchID = 0 
   test$couponsExpire = batch.start + years(1)
   test$couponsSent = batch.start
   for(i in 1:nbatch){
      orderinbatch= test$orderTime %within% couponBatches$validInterval[i] & test$couponsReceived %within% couponBatches$validInterval[i]
      couponinbatch= test$couponsReceived %within% couponBatches$validInterval[i]
      if(sum(orderinbatch) > 0) test$batchID[orderinbatch] = couponBatches$batch[i]
      if(sum(couponinbatch) > 0){
         test$couponsSent[couponinbatch] = couponBatches$couponsSent[i]
         test$couponsExpire[couponinbatch] = couponBatches$couponsExpire[i]
      }
   }
   test$batchID = as.factor(test$batchID)
   test$dataset = "test"

   #create the plots
   batch.invalid = data.frame('batch.violation' = c(train$batchID == 0, test$batchID == 0))

   #plots help us make sure that the batches make sense
   p1 = ggplot() + geom_point(data=cbind(rbind(train,test),batch.invalid),aes(x=couponsReceived,y=orderTime,shape=dataset,size=batch.violation))

   p2 = ggplot() +geom_rect(data=couponBatches, aes(xmin=couponsSent, xmax=couponsSent+weeks(1), ymin=couponsSent, ymax=couponsExpire, fill = batch),alpha=I(.4)) + geom_point(data=cbind(rbind(train,test),batch.invalid),aes(x=couponsReceived,y=orderTime,shape=dataset),size=I(.9)) 

   p3 = ggplot() + geom_point(data=cbind(rbind(train,test),batch.invalid),aes(x=couponsReceived,y=orderTime,color=batchID),size=I(.9))

   train = train[,-which(names(train) == "dataset")]
   test = test[,-which(names(test) == "dataset")]

   #add custom variables
   train$TimeBtwnSentRec = difftime(train$couponsReceived,train$couponsSent,units=unts)
   train$TimeBtwnRecExpire = difftime(train$couponsExpire,train$couponsReceived,units=unts)
   train$TimeBtwnRecOrder = difftime(train$orderTime,train$couponsReceived,units=unts)
   train$TimeBtwnOrderExpire = difftime(train$couponsExpire,train$orderTime,units=unts)

   test$TimeBtwnSentRec = difftime(test$couponsReceived,test$couponsSent,units=unts)
   test$TimeBtwnRecExpire = difftime(test$couponsExpire,test$couponsReceived,units=unts)
   test$TimeBtwnRecOrder = difftime(test$orderTime,test$couponsReceived,units=unts)
   test$TimeBtwnOrderExpire = difftime(test$couponsExpire,test$orderTime,units=unts)

   if(!keep.units){
      train$TimeBtwnSentRec = as.numeric(train$TimeBtwnSentRec)
      train$TimeBtwnRecExpire = as.numeric(train$TimeBtwnRecExpire)
      train$TimeBtwnRecOrder = as.numeric(train$TimeBtwnRecOrder)
      train$TimeBtwnOrderExpire = as.numeric(train$TimeBtwnOrderExpire)

      test$TimeBtwnSentRec = as.numeric(test$TimeBtwnSentRec)
      test$TimeBtwnRecExpire = as.numeric(test$TimeBtwnRecExpire)
      test$TimeBtwnRecOrder = as.numeric(test$TimeBtwnRecOrder)
      test$TimeBtwnOrderExpire = as.numeric(test$TimeBtwnOrderExpire)
   }

   results = list('train' = train, 'test' = test, 'plots' = list(p1,p2,p3))

   return(results)
}
