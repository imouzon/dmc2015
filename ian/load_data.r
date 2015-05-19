#--------------------------------------**--------------------------------------#
#  File Name: load_data.r
#  Purpose:
#
#  Creation Date: 17-05-2015
#  Last Modified: Tue May 19 00:16:31 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#
d = readRDS("~/dmc2015/data/clean_data/universalCleanData.rds")

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

source("~/dmc2015/ian/r/stackCoupons2.R")
dm = stackCoupons2(d,idcols = c(1:4,32:57)) 

source("~/dmc2015/ian/r/splitColumn.R")
dmc = splitColumn(dm,"categoryIDs","orderID",splitby=":") 
dmc = dmc[,-which(names(dmc) == "categoryIDs")]
dmc = clean_factor(dmc,"categoryIDs",scrape_off="cat")
dmcs = dm %>% 
   left_join(dmc %>%
         select(orderID,couponCol,categoryIDs1, categoryIDs2, categoryIDs3, categoryIDs4, categoryIDs5) %>%
         gather(tmp,categoryID,-orderID,-couponCol) %>%
         mutate(categoryEntry = gsub("categoryIDs","",tmp)) %>%
         select(orderID,couponCol,categoryEntry,categoryID) %>%
         arrange(orderID,couponCol,categoryEntry) %>%
         filter(!is.na(categoryID)),
      by = c("orderID","couponCol")) %>%
   arrange(orderID,couponCol,categoryEntry)

## add this grouping to the data
d = d %>% mutate(basketGroup = (1*(basketValue > 185) + 1*(basketValue > 137) + 1*(basketValue > 92)))
dm = dm %>% mutate(basketGroup = (1*(basketValue > 185) + 1*(basketValue > 137) + 1*(basketValue > 92)))

getpests = function(alpha.est){
   beta.est = 4*alpha.est 
   HTVset1 = readRDS("~/dmc2015/data/featureMatrix/HTVset1.rds")
   p1 = (sum(HTVset1$H$coupon1Used) + alpha.est)/(nrow(HTVset1$H) + alpha.est + beta.est)
   p2 = (sum(HTVset1$H$coupon2Used) + alpha.est)/(nrow(HTVset1$H) + alpha.est + beta.est)
   p3 = (sum(HTVset1$H$coupon3Used) + alpha.est)/(nrow(HTVset1$H) + alpha.est + beta.est)
   return(c(p1,p2,p3))
}

p = getpests(24)
