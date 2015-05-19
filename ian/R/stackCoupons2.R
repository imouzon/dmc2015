#--------------------------------------**--------------------------------------#
#  File Name:
#  Purpose:
#
#  Creation Date: 15-04-2015
#  Last Modified: Mon May 18 16:52:03 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)

stackCoupons2 = function(dsn,idcols = NULL){
   #coupon rows have 1, 2, or 3 in column name
   #if we don't specify, all non-coupon columns are id columns
   cpn.i = lapply(1:3, function(i) which(grepl(i,names(dsn)) & !(1:ncol(dsn) %in% idcols)))
   if(is.null(idcols)) idcols = (1:ncol(dsn))[-unlist(cpn.i)]

   #print messages so that it is obvious if there is a column problem
   m1 = "using the following as id:\n\tidcols\n"
   m2 = "using the following as measure columns:\n\tcpncols\n"
   message(gsub("idcols",paste(names(dsn)[idcols],collapse=',\n\t'),m1))
   message(gsub("cpncols",paste(names(dsn)[unlist(cpn.i)],collapse=',\n\t'),m2))

   cpnisolate = function(i){
      ret = dsn[,c(idcols,cpn.i[[i]])]
      names(ret) = c(names(dsn)[idcols],gsub(i,'',names(dsn)[cpn.i[[i]]]))
      ret$couponCol = i
      return(ret)
   }

   d.stack = do.call("rbind", lapply(1:3, cpnisolate))
   d.stack = d.stack %>% arrange(orderID, couponCol)

   return(d.stack)
}
