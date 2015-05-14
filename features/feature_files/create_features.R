#--------------------------------------**--------------------------------------#
#  File Name: create_features.R
#  Purpose:
#
#  Creation Date: 14-05-2015
#  Last Modified: Thu May 14 12:53:52 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#


set = "set1"
vers = "1"

setwd("~/dmc2015/features/feature_files/")

library(dplyr)

univ_features = readRDS("./universal/combined/universalFeaturesCombined.rds")

set_features = readRDS(paste0("./R/",set,"Combined.rds"))

features_full = univ_features %>% left_join(set_features,by="orderID")

features_trim = features_full[,!(grepl("ntimes", names(features_full)) | grepl("timesNotUsed_", names(features_full)) | grepl("timesUsedUsed_", names(features_full)))]

features_trim = features_trim[, 

names(features_trim)[2:49]




HTV = readRDS(paste0("~/dmc2015/data/featureMatrix/HTV",set,".rds"))
trn = features_trim[which(features_trim$orderID %in% HTV$T$orderIDs),]
val = features_trim[which(features_trim$orderID %in% HTV$V$orderIDs),]
cls = features_trim[which(features_trim$orderID %in% HTV$C$orderIDs),]
