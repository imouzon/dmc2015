#--------------------------------------**--------------------------------------#
#  File Name: new.r
#  Purpose:
#
#  Creation Date: 21-04-2015
#  Last Modified: Tue Apr 21 14:41:34 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#

trn$is555 = trn$basketValue > 555

trn$catIDs1main = sapply(1:nrow(trn), function(i) strsplit(trn$categoryIDs1[i],",")[[1]][1])
trn$catIDs2main = sapply(1:nrow(trn), function(i) strsplit(trn$categoryIDs2[i],",")[[1]][1])
trn$catIDs3main = sapply(1:nrow(trn), function(i) strsplit(trn$categoryIDs3[i],",")[[1]][1])

trn$catIDsTotalCoup = trn$coupon1Used + trn$coupon2Used + trn$coupon3Used

trn$relCat = sapply(1:nrow(trn), function(i) paste(trn[i, c("catIDs1main","catIDs2main","catIDs3main")[sapply(c("coupon1Used","coupon2Used","coupon3Used"), function(x) trn[i,x]==1)]],collapse=":"))
qplot(x=relCat,y=basketValue, ylim=c(0,10000), color=is555, data=trn,geom='boxplot',fill=as.factor(catIDsTotalCoup))
