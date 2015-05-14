#--------------------------------------**--------------------------------------#
#  File Name:
#  Purpose:
#
#  Creation Date: 14-05-2015
#  Last Modified: Thu May 14 11:26:19 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#

setwd("~/dmc2015/features/feature_files/")
files = list.files("./set1",full.names=TRUE)
dsn = readRDS(files[1])
dsnf = do.call("rbind",dsn)

saveRDS(dsnf,"clean_coupons_used.rds")
