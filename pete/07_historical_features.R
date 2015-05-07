# ============================================================================
# Author: Evan P. Walsh
# Contact: epwalsh@iastate.edu
# Date: 05-05-2015
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
s1$H$coup1CatLL <- compute_ll(s1$H$categoryIDs1, s1$H$coupon1Used)
s1$H$coup2CatLL <- compute_ll(s1$H$categoryIDs2, s1$H$coupon2Used)
s1$H$coup3CatLL <- compute_ll(s1$H$categoryIDs3, s1$H$coupon3Used)
