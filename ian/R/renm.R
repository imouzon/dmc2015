#--------------------------------------**--------------------------------------#
#  File Name:
#  Purpose:
#
#  Creation Date: 29-04-2015
#  Last Modified: Wed Apr 29 13:37:21 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#

renm = function(dsn,colnum=NULL,newname=NULL){
   if(is.null(newname)) newname=colnum; colnum=NULL
   if (is.null(colnum)) colnum = 1:ncol(dsn)
   names(dsn)[colnum] = newname
   return(dsn)
}

