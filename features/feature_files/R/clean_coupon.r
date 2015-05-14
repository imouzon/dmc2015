#--------------------------------------**--------------------------------------#
#  File Name:
#  Purpose:
#
#  Creation Date: 14-05-2015
#  Last Modified: Thu May 14 17:04:21 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#

feature_root = "~/dmc2015/features/feature_files/"
setwd(feature_root)
source("./universal/combine_universal_features.R")
uf = combine_universal_features = function(read_dir = "./universal/individual", write_dir = "./universal/combined", universal_clean = "../../data/clean_data/universalCleanData.rds", exclude = c("basePrice_price_ratio.rds"), joinby = "orderID")

## Making Set 1:
# combine_set_individual("set1",feature_root,"wide","orderID")
# combine_set_individual("set1",feature_root,"long",c("orderID","couponCol"))
w1 = readRDS("./set1/set1Combined_wide.rds")
l1 = readRDS("./set1/set1Combined_long.rds")

## Making Set 2:
# combine_set_individual("set2",feature_root,"wide","orderID")
# combine_set_individual("set2",feature_root,"long",c("orderID","couponCol"))
w2 = readRDS("./set2/set2Combined_wide.rds")
l2 = readRDS("./set2/set2Combined_long.rds")

## Making Set 3:
# combine_set_individual("set3",feature_root,"wide","orderID")
# combine_set_individual("set3",feature_root,"long",c("orderID","couponCol"))
w3 = readRDS("./set3/set3Combined_wide.rds")
l3 = readRDS("./set3/set3Combined_long.rds")



source("./R/makeWideFeatureMatrix.r")

#####
makeWideFeatureMatrix(w1,"set1")
makeWideFeatureMatrix(w2,"set2")
makeWideFeatureMatrix(w3,"set3")


##### make long sets
