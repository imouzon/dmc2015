#--------------------------------------**--------------------------------------#
#  File Name:
#  Purpose:
#
#  Creation Date: 14-05-2015
#  Last Modified: Fri May 15 09:18:34 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#

library(dplyr)
feature_root = "~/dmc2015/features/feature_files/"
setwd(feature_root)
source("./universal/combine_universal_features.R")
uf = combine_universal_features(read_dir = "./universal/individual", write_dir = "./universal/combined", universal_clean = "../../data/clean_data/universalCleanData.rds", exclude = c("basePrice_price_ratio.rds"), joinby = "orderID")
names(uf) = gsub("cos.sim","cosSim",names(uf))

## Making Set 1:
# set="set1"
# feature_dir = "~/dmc2015/features/feature_files/"
# type = "wide"
# joinby = "orderID"
source("./R/combine_set.R")
combine_set_individual("set1",feature_root,"wide","orderID")
combine_set_individual("set1",feature_root,"long",c("orderID","couponCol"))
w1 = readRDS("./set1/set1Combined_wide.rds")
l1 = readRDS("./set1/set1Combined_long.rds")

## Making Set 2:
combine_set_individual("set2",feature_root,"wide","orderID")
combine_set_individual("set2",feature_root,"long",c("orderID","couponCol"))
w2 = readRDS("./set2/set2Combined_wide.rds")
l2 = readRDS("./set2/set2Combined_long.rds")

## Making Set 3:
combine_set_individual("set3",feature_root,"wide","orderID")
combine_set_individual("set3",feature_root,"long",c("orderID","couponCol"))
w3 = readRDS("./set3/set3Combined_wide.rds")
l3 = readRDS("./set3/set3Combined_long.rds")


#####
source("./R/makeWideFeatureMatrix.r")
makeWideFeatureMatrix(w1,"set1")
makeWideFeatureMatrix(w2,"set2")
makeWideFeatureMatrix(w3,"set3")


##### make long sets (THIS PART IS GROSS AND HAS SMALL DETAILS)
##### make long sets (THIS PART IS GROSS AND HAS SMALL DETAILS)
##### make long sets (THIS PART IS GROSS AND HAS SMALL DETAILS)
forcoupon1 = names(uf)[grepl("1",names(uf)) | grepl("12",names(uf)) | grepl("13",names(uf))]
uf1 = uf[,c("orderID",forcoupon1)] 
uf1 = uf1[,-(32:42)]
names(uf1) = gsub("12","AB",names(uf1))
names(uf1) = gsub("13","AC",names(uf1))
names(uf1) = gsub("1","",names(uf1))
names(uf1) = gsub("2","",names(uf1))
uf1$couponCol = 1


forcoupon2 = names(uf)[grepl("2",names(uf)) | grepl("12",names(uf)) | grepl("23",names(uf))]
uf2 = uf[,c("orderID",forcoupon2)] 
uf2 = uf2[,-c(11,13,14,16)]
uf2 = uf2[,-(32:42)]
names(uf2) = gsub("12","AB",names(uf2))
names(uf2) = gsub("23","AC",names(uf2))
names(uf2) = gsub("2","",names(uf2))
uf2$couponCol = 2


forcoupon3 = names(uf)[grepl("3",names(uf)) | grepl("13",names(uf)) | grepl("23",names(uf))];
uf3 = uf[,c("orderID",forcoupon3)] 
uf3 = uf3[,-(32:42)]
names(uf3) = gsub("13","AB",names(uf3))
names(uf3) = gsub("23","AC",names(uf3))
names(uf3) = gsub("3","",names(uf3))
names(uf3) = gsub("2","",names(uf3))
uf3$couponCol = 3

ul = uf[,c(1:4,32:49)] %>% left_join(uf1 %>% rbind(uf2) %>% rbind(uf3),by="orderID") %>%
   arrange(orderID,couponCol)


##### make long sets (ITS OVER NOW)
##### make long sets (ITS OVER NOW)
##### make long sets (ITS OVER NOW)


#make long
source("./R/makeLongFeatureMatrix.r")
makeLongFeatureMatrix(l1,ul,"set1")
makeLongFeatureMatrix(l2,ul,"set2")
makeLongFeatureMatrix(l3,ul,"set3")
