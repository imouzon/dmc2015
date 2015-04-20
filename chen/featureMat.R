getwd()
setwd("/Users/chenhua/Desktop/isu/dmc/data/raw_data")
library(ggplot2)
library(reshape2)
train <- read.table("DMC_2015_orders_train.txt", header = TRUE, sep = "|")
test <- read.table("DMC_2015_orders_class.txt",header =TRUE, sep = "|")

train$premInd <- train$premCount <-train$premiumProduct1+train$premiumProduct2+train$premiumProduct3
train[train$premInd >0, ]$premInd <- 1

train$coupnIndicator <- train$coupUsedCount <- train$coupon1Used+train$coupon2Used+train$coupon3Used
train[train$coupnIndicator > 0, ]$coupnIndicator <- 1


####  Calculating % of coupon used for non-premium and premium product, respectively
## Non-premium product
sum(train[train$premInd == 0, ]$coupnIndicator == 1)/dim(train[train$premInd == 0, ])[1]

## Premium product
sum(train[train$premInd == 1, ]$coupnIndicator == 1)/dim(train[train$premInd == 1, ])[1]

## Comparison between non-premium product and premium product
ggplot(data = train, aes(x = factor(premInd), fill = factor(coupnIndicator)) ) +
  geom_bar(position="dodge") + 
  scale_x_discrete(breaks=c("0", "1"),
                   labels=c("Non-premium product", "Premium product"))
# stat="identity",

####  Conclusion: Non-premium products tend to have higher redemption rate.


####  Within premium product
##  1 for 1 coupon product is premium product
##  2 for 2 coupon products are premium products
##  3 for 3 coupon products are premium products
ggplot(data = train[train$premInd == 1, ], 
       aes(x = factor(premCount), fill = factor(coupnIndicator))) + geom_bar(position = "dodge") + 
  scale_x_discrete(breaks=c("1", "2", "3"),
                   labels=c("1 premium product", "2 premium products", "3 premium products"))

# % of they redeem at least one of the coupon is 0.3643018
sum(train[train$premInd ==1 & train$premCount == 1,]$coupnIndicator == 1)/
  dim(train[train$premInd ==1 & train$premCount == 1,])[1]

# % of they redeem at least one of the coupon is 0.3984476
sum(train[train$premInd ==1 & train$premCount == 2,]$coupnIndicator == 1)/
  dim(train[train$premInd ==1 & train$premCount == 2,])[1]

# % of they redeem at least one of the coupon is 0.4703557
sum(train[train$premInd ==1 & train$premCount == 3,]$coupnIndicator == 1)/
  dim(train[train$premInd ==1 & train$premCount == 3,])[1]

####  Conclusion: the more premium product, the more likely they redeem the coupon.
####  Further investigation: check each one of the coupon combination

##  To Be Continued...


####  Within only 1 coupon product is premium product
oneCpnProdIsPrem_dat <- train[train$premInd == 1 & train$premCount == 1, ]

oneCpnProdIsPrem_dat$premProdClass <- apply(cbind(oneCpnProdIsPrem_dat[,'premiumProduct1'], oneCpnProdIsPrem_dat[,'premiumProduct2'], 
                                                  oneCpnProdIsPrem_dat[,'premiumProduct3']), 1, which.max)

ggplot(data = oneCpnProdIsPrem_dat, aes(x = factor(premProdClass),fill = factor(coupnIndicator))) +
  geom_bar(position = "dodge") + 
  scale_x_discrete(breaks=c("1", "2", "3"),
                  labels=c("1st Coupon Premium", "2nd Coupon Premium", "3rd Coupon Premium"))

# % of they redeem the coupon for 1st coupon product is premium product 0.3732809
sum(train[train$premInd == 1 & train$premCount == 1 & train$premiumProduct1 == 1, ]$coupnIndicator == 1)/
  dim(train[train$premInd == 1 & train$premCount == 1 & train$premiumProduct1 == 1, ])[1]

# % of they redeem the coupon for 2nd coupon product is premium product 0.3837638
sum(train[train$premInd == 1 & train$premCount == 1 & train$premiumProduct2 == 1, ]$coupnIndicator == 1)/
  dim(train[train$premInd == 1 & train$premCount == 1 & train$premiumProduct2 == 1, ])[1]

# % of they redeem the coupon for 3rd coupon product is premium product 0.3434483
sum(train[train$premInd == 1 & train$premCount == 1 & train$premiumProduct3 == 1, ]$coupnIndicator == 1)/
  dim(train[train$premInd == 1 & train$premCount == 1 & train$premiumProduct3 == 1, ])[1]



######################################################
########  --------  premiumProduct  --------  ########
######################################################

################################################################
######  Only one product out of three is Premium product  ######
######  n = 1776 obs                                      ######
################################################################
dim(train[train$premInd == 1 & train$premCount == 1, ])

## Only first coupon product is a premium product
train[train$premInd == 1 & train$premCount == 1 &
        train$premiumProduct1 == 1, ]
cpnProdPremium_1 <- as.numeric(train$premInd == 1 & train$premCount == 1 &
                                 train$premiumProduct1 == 1)

## Only second coupon product is a premium product
train[train$premInd == 1 & train$premCount == 1 &
        train$premiumProduct2 == 1, ]
cpnProdPremium_2 <- as.numeric(train$premInd == 1 & train$premCount == 1 &
                                 train$premiumProduct2 == 1)

## Only third coupon product is a premium product
train[train$premInd == 1 & train$premCount == 1 &
        train$premiumProduct3 == 1, ]
cpnProdPremium_3 <- as.numeric(train$premInd == 1 & train$premCount == 1 &
                                 train$premiumProduct3 == 1)

#############################################################
######  Two product out of three are Premium products  ######
######  n = 773 obs                                    ######
#############################################################
dim(train[train$premInd == 1 & train$premCount == 2, ])
## First and Second coupon products are premium products
train[train$premInd == 1 & train$premCount == 2 &
        train$premiumProduct1 == 1 & train$premiumProduct2 == 1, ]
cpnProdPremium_12 <- as.numeric(train$premInd == 1 & train$premCount == 2 &
                                  train$premiumProduct1 == 1 & train$premiumProduct2 == 1)

## First and Third coupon products are premium products
train[train$premInd == 1 & train$premCount == 2 &
        train$premiumProduct1 == 1 & train$premiumProduct3 == 1, ]
cpnProdPremium_13 <- as.numeric(train$premInd == 1 & train$premCount == 2 &
                                  train$premiumProduct1 == 1 & train$premiumProduct3 == 1)

## Second and Third coupon products are premium products
train[train$premInd == 1 & train$premCount == 2 &
        train$premiumProduct2 == 1 & train$premiumProduct3 == 1, ]
cpnProdPremium_23 <- as.numeric(train$premInd == 1 & train$premCount == 2 &
                                  train$premiumProduct2 == 1 & train$premiumProduct3 == 1)


#######################################################
######  All three products are Premium products  ######
######  n = 253 obs                              ######
#######################################################
dim(train[train$premInd == 1 & train$premCount == 3, ])
cpnProdPremium_123 <- as.numeric(train$premInd == 1 & train$premCount == 3)


basketValue <- train$basketValue
coupon1Used <- train$coupon1Used
coupon2Used <- train$coupon2Used
coupon3Used <- train$coupon3Used
premCount <- train$premCount
premIndicator <- train$premInd



###########################################
####  ----  Non-premium product  ----  ####
###########################################

# sum(train$premInd == 0)
# train$premInd == 0
# train[train$premInd == 0,]$brand1
t <- train[train$premInd == 0,]




featureMatr <- data.frame(basketValue,coupon1Used,coupon2Used,coupon3Used,premCount,
                          premIndicator,cpnProdPremium_1,cpnProdPremium_2,cpnProdPremium_3,
                          cpnProdPremium_12,cpnProdPremium_13,cpnProdPremium_23,cpnProdPremium_123
)

