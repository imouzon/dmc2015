#--------------------------------------**--------------------------------------#
#  File Name:
#  Purpose:
#
#  Creation Date: 22-04-2015
#  Last Modified: Wed Apr 22 17:43:10 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#

#training set ("historical data")
trn.raw = read.delim("../data/raw_data/DMC_2015_orders_train.txt", stringsAsFactors = FALSE, sep = "|", quote = "")
trn = read.csv("../data/clean_data/train_simple_name.csv", stringsAsFactors = FALSE,na.strings="")

#test set ("future data")
tst.raw = read.delim("../data/raw_data/DMC_2015_orders_class.txt", stringsAsFactors = FALSE, sep = "|", quote = "")
tst = read.csv("../data/clean_data/test_simple_name.csv", stringsAsFactors = FALSE,na.strings="")
