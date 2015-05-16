# ============================================================================
# File Name: 11_var_imp.R
#
# Author: Evan P. Walsh
# Contact: epwalsh@iastate.edu
#
# Creation Date: 15-05-2015
# Last Modified: Fri May 15 22:58:35 2015
#
# Purpose: Measure variable importance with regard to conditional random
# forests. To do this we will randomly group variables and fit conditional
# random forests with each group of variable and record the importance of 
# every variable.
#
# ============================================================================

# Util functions
# ----------------------------------------------------------------------------
getGroupSize <- function(n_groups, n) {
  stopifnot(n_groups <= n)
  sizes <- rep(NA, n_groups)
  count <- n
  for (i in 1:n_groups) {
    groups <- n_groups - i + 1
    sizes[i] <- floor(count / groups)
    count <- count - sizes[i]
  }
  return(sizes)
}

source("~/GitHub/dmc2015/pete/10_roc.R")
# ----------------------------------------------------------------------------

library(party)

h1 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.rds")
h12 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.2.rds")
# We will take random samples of 10/11 variables and fit a conditional random 
# forest and record the importance of each variable. We then repeat this 
# process on a new disjoint random set of predictors. This is repeated until
# all predictors are used.
vars <- names(h1$train$X[sapply(h1$train$X, is.numeric)])
sizes <- getGroupSize(57, length(vars))
importance <- data.frame(var = NULL, imp = NULL)

for (s in sizes) {
  samp <- sample(vars[!(vars %in% importance$var)], s)
  train <- cbind(couponUsed = h1$train$y$couponUsed, h1$train$X[samp])
  cf <- cforest(couponUsed~., data = train,
                control = cforest_unbiased(mtry = 3, ntree = 50))
  vImp <- varimp(cf)
  temp <- data.frame(var = names(vImp), imp = as.numeric(vImp))
  importance <- rbind(importance, temp)
}

importance$imp <- importance$imp * 10000
importance <- importance[order(importance$imp, decreasing = T),]
rownames(importance) <- 1:nrow(importance)
saveRDS(importance, "~/GitHub/dmc2015/pete/predictions/importance.rds")
importance <- readRDS("~/GitHub/dmc2015/pete/predictions/importance.rds")

# How do naive estimators compare to non-naive?
mean(importance$imp[grep("naive", importance$var)])
mean(importance$imp[grep("est", importance$var)])

importance$var <- as.character(importance$var)

# Test model with top 10 predictors 
vars <- importance$var[c(1:7,9:11)]
train <- cbind(couponUsed = h1$train$y$couponUsed, 
               h1$train$X[vars])
cf <- cforest(couponUsed~., data = train,
              control = cforest_unbiased(mtry = 3, ntree = 50))

h1_v <- cbind(couponUsed = h1$validation$y$couponUsed, 
              h1$validation$X[vars])
m1_v_p <- predict(cf, newdata = h1_v)

roc(h1_v_p, h1_v$couponUsed)

library(randomForest)
rf <- randomForest(couponUsed~., data = train,
                   mtry = 3, ntree = 50)
h1_v_p2 <- predict(rf, newdata = h1_v)
roc(h1_v_p2, h1_v$couponUsed)
