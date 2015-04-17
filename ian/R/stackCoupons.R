#--------------------------------------**--------------------------------------#
#  File Name:
#  Purpose:
#
#  Creation Date: 15-04-2015
#  Last Modified: Wed Apr 15 13:51:32 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)

stackCoupons = function(train,test,idcols = NULL){
   #coupon rows have 1, 2, or 3 in column name
   cpn.i = lapply(1:3, function(i) which(grepl(i,names(train))))

   #if we don't specify, all non-coupon columns are id columns
   if(is.null(idcols)) idcols = (1:ncol(train))[-unlist(cpn.i)]

   #print messages so that it is obvious if there is a column problem
   m1 = "using columns:\n\tidcols\nas id columns"
   m2 = "using columns:\n\tcpncols\nas measure columns"
   message(gsub("idcols",paste(names(train)[idcols],collapse=',\n\t'),m1))
   message(gsub("cpncols",paste(names(train)[unlist(cpn.i)],collapse=',\n\t'),m2))

   #identify observations as coming from training or test set
   train$dsn = "train"
   test$dsn = "test"

   #add that dataset identifier to the id columns
   idcols = c(idcols,which(names(train) == "dsn"))

   cpnisolate = function(i){
      ret = rbind(train,test)[,c(idcols,cpn.i[[i]])]
      ret$couponCol = i
      names(ret) = gsub(i,'',names(ret))
      return(ret)
   }

   d.stack = do.call("rbind", lapply(1:3, cpnisolate))

   d = list("train" = d.stack[which(d.stack$dsn == "train"),-which(names(d.stack) == "dsn")],
            "test" = d.stack[which(d.stack$dsn == "test"),-which(names(d.stack) == "dsn")],
            "combined" = d.stack)

   return(d)
}

