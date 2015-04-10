#--------------------------------------**--------------------------------------#
#  File Name: scratch.r
#  Purpose:
#
#  Creation Date: 07-04-2015
#  Last Modified: Tue Apr  7 17:08:55 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#

d = read.delim("../dmc2013/data/transact_train.txt", header = TRUE, na.strings="?",sep = "|", quote = "")
d = read.delim("../dmc2014/data/orders_train.txt", header = TRUE, na.strings="",sep = ";", quote = "")
head(d)
