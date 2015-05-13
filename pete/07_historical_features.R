# ============================================================================
# Author: Evan P. Walsh
# Contact: epwalsh@iastate.edu
# Date: May 5, 2015
#
# Purpose: To create features based on the historical sections of the 
# the data set. The only features that need to be explicitly created from the 
# historical set are features that involve the response. Out of that class 
# of features, I am mainly responsible for creating the log-likelihood
# statistics for predicting coupon usage.
#
# ============================================================================

# Set 1: Random sampling on coupons
s1 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/HTVset1.rds")
# Set 2: Random sampling on users
s2 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/HTVset2.rds")
# Set 3: Chronological by batches - historical = 1:7, train = 8, test = 9
s3 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/HTVset3.rds")


# Source likelihood functions
source("06_likelihood.R")

# Create likelihood features for H1 train, test, and classification
# First melt data
d <- data.frame(orderID = rep(s1$H$orderID, 3),
                brand = c(as.character(s1$H$brand1), 
                          as.character(s1$H$brand2), 
                          as.character(s1$H$brand3)),
                reward = c(as.character(s1$H$reward1),
                           as.character(s1$H$reward2),
                           as.character(s1$H$reward3)),
                coupUsed = c(s1$H$coupon1Used, s1$H$coupon2Used, s1$H$coupon3Used),
                stringsAsFactors = F)
d <- d[order(d$orderID),]

s1$T$brand1_ll <- compute_ll(d$brand,
                             d$coupUsed,
                             as.character(s1$T$brand1))
s1$T$brand2_ll <- compute_ll(d$brand,
                             d$coupUsed,
                             as.character(s1$T$brand2))
s1$T$brand3_ll <- compute_ll(d$brand,
                             d$coupUsed,
                             as.character(s1$T$brand3))
s1$T$brand_rew_1 <- compute_ll_2w(d$brand, d$reward, d$coupUsed,
                                 as.character(s1$T$brand1),
                                 as.character(s1$T$reward1))
