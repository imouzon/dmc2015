#--------------------------------------**--------------------------------------#
#  File Name: universal_clean_data.r
#  Purpose:
#
#  Creation Date: 18-05-2015
#  Last Modified: Mon May 18 13:03:17 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#
getpests = function(alpha.est){
   beta.est = 4*alpha.est 
   HTVset1 = readRDS("~/dmc2015/data/featureMatrix/HTVset1.rds")
   p1 = (sum(HTVset1$H$coupon1Used) + alpha.est)/(nrow(HTVset1$H) + alpha.est + beta.est)
   p2 = (sum(HTVset1$H$coupon2Used) + alpha.est)/(nrow(HTVset1$H) + alpha.est + beta.est)
   p3 = (sum(HTVset1$H$coupon3Used) + alpha.est)/(nrow(HTVset1$H) + alpha.est + beta.est)
   return(c(p1,p2,p3))
}

universal_clean_data = function(path2ucd = "~/dmc2015/data/clean_data/universalCleanData.rds") {
   require(dplyr)
   d = readRDS(path2ucd)

   source("~/dmc2015/ian/R/clean_factor.r")
   d = clean_factor(d,"couponID","cpn")
   d = clean_factor(d,"brand","brand")
   d = clean_factor(d,"productGroup","prod")
   d = clean_factor(d,"categoryIDs")
   d$ShopFast = 1*(d$TimeBtwnRecOrder < 28)
   d$EarlyRec = 1*(d$TimeBtwnSentRec < 8)

   d$Shop60 = floor(d$orderTimeTime)
   d$Shop30 = floor(d$orderTimeTime * 60/30)*30/60
   d$Shop15 = floor(d$orderTimeTime * 60/15)*15/60

   d$RecExpire60 = floor(d$TimeBtwnSentRec)
   d$RecOrder60 = floor(d$TimeBtwnRecOrder)
   d$OrderExpire60 = floor(d$TimeBtwnOrderExpire)

   d$basePrice_price_ratio1 = d$basePrice1/d$price1
   d$basePrice_price_ratio2 = d$basePrice2/d$price2
   d$basePrice_price_ratio3 = d$basePrice3/d$price3

   d = d[,c(1,3,2,43,45,4,44,46:49,33:37,38:42,50:57,5:12,58,13:20,59,21:28,60,29:32)]

   d1 = d[,c(1:29,60,30:38,57)]
   names(d1)[31:40] = gsub("1","",names(d1)[31:40])
   d1$couponCol = 1

   d2 = d[,c(1:29,60,39:47,58)]
   names(d2)[31:40] = gsub("2","",names(d2)[31:40])
   d2$couponCol = 2

   d3 = d[,c(1:29,60,48:56,59)]
   names(d3)[31:40] = gsub("3","",names(d3)[31:40])
   d3$couponCol = 3

   dm = rbind(d1,d2) %>% rbind(d3)
   
   source("~/dmc2015/ian/r/splitColumn.R")
   dmc = splitColumn(dm,"categoryIDs","orderID",splitby=":") 
   dmc = dmc[,-which(names(dmc) == "categoryIDs")]
   dmc = clean_factor(dmc,"categoryIDs",scrape_off="cat")

   ## add this grouping to the data
   d = d %>% mutate(basketGroup = (1*(basketValue > 185) + 1*(basketValue > 137) + 1*(basketValue > 92)))
   dm = dm %>% mutate(basketGroup = (1*(basketValue > 185) + 1*(basketValue > 137) + 1*(basketValue > 92)))

   alpha.est = 24
   p = getpests(alpha.est)

   return(list("d" = d,"dm" = dm, "dmc" = dmc))
}
