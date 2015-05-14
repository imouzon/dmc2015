#--------------------------------------**--------------------------------------#
#  File Name:
#  Purpose:
#
#  Creation Date: 14-05-2015
#  Last Modified: Thu May 14 15:51:48 2015
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
source("./R/combine_set.R")

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


## Working with wide versions
dropcols = c("orderTime",
         "userID",
         "couponsReceived",
         "couponID1",
         "couponID2",
         "couponID3",
         "couponsReceivedDate",
         "orderTimeDate",
         "batchID",
         "couponsExpire",
         "couponsSent")

f1 = uf[,!(names(uf) %in% dropcols)] %>% left_join(w1,by="orderID") %>% arrange(orderID)
f1 = f1[,!(grepl("ntimes_",names(f1)) | grepl("timesNotUsed_",names(f1)) | grepl("timesUsed_",names(f1)))]

HTV1 = readRDS("~/dmc2015/data/featureMatrix/HTVset1.rds")

#train
Xtrn = HTV1$T %>% select(orderID) %>% left_join(f1,by="orderID") %>% arrange(orderID)
trn = list("X" = Xtrn %>% select(-orderID,-coupon1Used,-coupon2Used,-coupon3Used,-basketValue),
           "y" = Xtrn %>% select(orderID,coupon1Used, coupon2Used, coupon3Used, basketValue))

#val
Xval = HTV1$V %>% select(orderID) %>% left_join(f1,by="orderID") %>% arrange(orderID)
val = list("X" = Xval %>% select(-orderID,-coupon1Used,-coupon2Used,-coupon3Used,-basketValue),
           "y" = Xval %>% select(orderID,coupon1Used, coupon2Used, coupon3Used, basketValue))

#class
Xcls = HTV1$C %>% select(orderID) %>% left_join(f1,by="orderID") %>% arrange(orderID)
cls = list("X" = Xcls %>% select(-orderID,-coupon1Used,-coupon2Used,-coupon3Used,-basketValue),
           "y" = Xcls %>% select(orderID,coupon1Used, coupon2Used, coupon3Used, basketValue))

Fmat1 = list("train" = trn, "class" = cls, "validation" = val)
saveRDS(Fmat1,file="~/dmc2015/data/featureMatrix/featMat_based-on-HTV1_ver0.rds")
