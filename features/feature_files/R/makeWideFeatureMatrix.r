#--------------------------------------**--------------------------------------#
#  File Name: makeWideFeatureMatrix.r
#  Purpose:
#
#  Creation Date: 14-05-2015
#  Last Modified: Thu May 14 17:47:29 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#
makeWideFeatureMatrix = function(w,set){
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

   f1 = uf[,!(names(uf) %in% dropcols)] %>% left_join(w,by="orderID") %>% arrange(orderID)
   f1 = f1[,!(grepl("ntimes_",names(f1)) | grepl("timesNotUsed_",names(f1)) | grepl("timesUsed_",names(f1)))]

   #check for names repeats
   data.frame(table(names(f1))) %>% arrange(Freq) %>% head

   #check for identical columns:
   nfeat = ncol(f1)
   identMat = sapply(1:nfeat,function(i) sapply(1:nfeat, function(j) identical(f1[,i],f1[,j])))
   vars = data.frame(which(identMat,arr.ind=TRUE)) %>% filter(row < col) %>% mutate(var1 = names(f1)[row], var2 = names(f1)[col]) 
   f1 = f1[,which(!(names(f1) %in% vars[,4]))]

   #convert characters 
   chars = names(f1)[sapply(1:ncol(f1), function(i) is.character(f1[,i]))]
   for(x in chars) f1[,x] = as.factor(f1[,x])

   factors = names(f1)[sapply(1:ncol(f1), function(i) is.factor(f1[,i]))]

   HTV = readRDS(paste0("~/dmc2015/data/featureMatrix/HTV",set,".rds"))

   #train
   Xtrn = HTV$T %>% select(orderID) %>% left_join(f1,by="orderID") %>% arrange(orderID)
   trn = list("X" = Xtrn %>% select(-orderID,-coupon1Used,-coupon2Used,-coupon3Used,-basketValue),
              "y" = Xtrn %>% select(orderID,coupon1Used, coupon2Used, coupon3Used, basketValue))

   #val
   Xval = HTV$V %>% select(orderID) %>% left_join(f1,by="orderID") %>% arrange(orderID)
   val = list("X" = Xval %>% select(-orderID,-coupon1Used,-coupon2Used,-coupon3Used,-basketValue),
              "y" = Xval %>% select(orderID,coupon1Used, coupon2Used, coupon3Used, basketValue))

   #class
   Xcls = HTV$C %>% select(orderID) %>% left_join(f1,by="orderID") %>% arrange(orderID)
   cls = list("X" = Xcls %>% select(-orderID,-coupon1Used,-coupon2Used,-coupon3Used,-basketValue),
              "y" = Xcls %>% select(orderID,coupon1Used, coupon2Used, coupon3Used, basketValue))

   Fmat1 = list("train" = trn, "class" = cls, "validation" = val)
   saveRDS(Fmat1,file=paste0("~/dmc2015/data/featureMatrix/featMat_based-on-HTV",set,"_WIDE_ver0.rds"))
}

