# ============================================================================
# File Name: 11_var_imp.R
#
# Author: Evan P. Walsh
# Contact: epwalsh@iastate.edu
#
# Creation Date: 15-05-2015
# Last Modified: Tue May 19 00:35:06 2015
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

library(party)
source("~/GitHub/dmc2015/pete/10_roc.R")
# ----------------------------------------------------------------------------

# Classification 
# Var importance on new feature matrix with 1000+ vars for classification.
# Only do this for numeric variables. Factors will take too long.
# ============================================================================
h1 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.8.rds")
vars <- names(h1$train$X[sapply(h1$train$X, is.numeric)])
vars <- vars[vars != "orderID"]
sizes <- getGroupSize(100, length(vars))
importance <- data.frame(var = NULL, imp = NULL)

for (s in sizes) {
  samp <- sample(vars[!(vars %in% importance$var)], s)
  train <- cbind(couponUsed = h1$train$y$couponUsed, h1$train$X[samp])
  cf <- cforest(couponUsed~., data = train,
                control = cforest_unbiased(mtry = 3, ntree = 50))
  vImp <- varimp(cf)
  temp <- data.frame(var = names(vImp), imp = as.numeric(vImp))
  importance <- rbind(importance, temp)
  saveRDS(importance, "~/GitHub/dmc2015/pete/predictions/importance_H1_0.8.rds")
  cat(nrow(importance))
  cat("\n")
}

importance$imp <- importance$imp * 10000
importance <- importance[order(importance$imp, decreasing = T),]
rownames(importance) <- 1:nrow(importance)
saveRDS(importance, "~/GitHub/dmc2015/pete/predictions/importance_H1_0.8.rds")


h3 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset3_LONG_ver0.5.rds")
vars <- names(h3$train$X[sapply(h3$train$X, is.numeric)])
vars <- vars[vars != "orderID"]
sizes <- getGroupSize(70, length(vars))
importance <- data.frame(var = NULL, imp = NULL)

for (s in sizes) {
  samp <- sample(vars[!(vars %in% importance$var)], s)
  train <- cbind(couponUsed = h3$train$y$couponUsed, h3$train$X[samp])
  cf <- cforest(couponUsed~., data = train,
                control = cforest_unbiased(mtry = 3, ntree = 50))
  vImp <- varimp(cf)
  temp <- data.frame(var = names(vImp), imp = as.numeric(vImp))
  importance <- rbind(importance, temp)
  saveRDS(importance, "~/GitHub/dmc2015/pete/predictions/importance_H3_0.5.rds")
  cat(nrow(importance))
  cat("\n")
}

importance$imp <- importance$imp * 10000
importance <- importance[order(importance$imp, decreasing = T),]
rownames(importance) <- 1:nrow(importance)
saveRDS(importance, "~/GitHub/dmc2015/pete/predictions/importance_H3_0.5.rds")

# Merge importance measures from all three historical sets
imp1 <- readRDS("~/GitHub/dmc2015/pete/predictions/importance_H1_0.5.rds")
imp3 <- readRDS("~/GitHub/dmc2015/pete/predictions/importance_H3_0.5.rds")

names(imp1)[2] <- "h1_imp"
names(imp3)[2] <- "h3_imp"

imp <- merge(imp1, imp3, all = T)

imp <- imp[order(imp$h1_imp, decreasing = T),]
rownames(imp) <- 1:nrow(imp)
saveRDS(imp, "~/GitHub/dmc2015/pete/predictions/importance_0.4.rds")

# Regression
# Var importance on new feature matrix for regression (basketValue).
# Only do this for numeric variables. Factors will take too long.
# ============================================================================
h1 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_WIDE_ver0.5.rds")
vars <- names(h1$train$X[sapply(h1$train$X, is.numeric)])
vars <- vars[vars != "orderID"]
sizes <- getGroupSize(200, length(vars))
importance <- data.frame(var = NULL, imp = NULL)

for (s in sizes) {
  samp <- sample(vars[!(vars %in% importance$var)], s)
  train <- cbind(basketValue = h1$train$y$basketValue, h1$train$X[samp])
  cf <- cforest(basketValue~., data = train,
                control = cforest_unbiased(mtry = 5, ntree = 50))
  vImp <- varimp(cf)
  temp <- data.frame(var = names(vImp), imp = as.numeric(vImp))
  importance <- rbind(importance, temp)
  saveRDS(importance, "~/GitHub/dmc2015/pete/predictions/importance_H1_0.5_BV.rds")
  cat(nrow(importance))
  cat("\n")
}

importance <- importance[order(importance$imp, decreasing = T),]
rownames(importance) <- 1:nrow(importance)
saveRDS(importance, "~/GitHub/dmc2015/pete/predictions/importance_H1_0.5_BV.rds")


h3 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset3_WIDE_ver0.5.rds")
vars <- names(h3$train$X[sapply(h3$train$X, is.numeric)])
vars <- vars[vars != "orderID"]
sizes <- getGroupSize(200, length(vars))
importance <- data.frame(var = NULL, imp = NULL)

for (s in sizes) {
  samp <- sample(vars[!(vars %in% importance$var)], s)
  train <- cbind(basketValue = h3$train$y$basketValue, h3$train$X[samp])
  cf <- cforest(basketValue~., data = train,
                control = cforest_unbiased(mtry = 3, ntree = 50))
  vImp <- varimp(cf)
  temp <- data.frame(var = names(vImp), imp = as.numeric(vImp))
  importance <- rbind(importance, temp)
  saveRDS(importance, "~/GitHub/dmc2015/pete/predictions/importance_H3_0.5_BV.rds")
  cat(nrow(importance))
  cat("\n")
}

importance <- importance[order(importance$imp, decreasing = T),]
rownames(importance) <- 1:nrow(importance)
saveRDS(importance, "~/GitHub/dmc2015/pete/predictions/importance_H3_0.5_BV.rds")

# Merge importance measures from all three historical sets
imp1 <- readRDS("~/GitHub/dmc2015/pete/predictions/importance_H1_0.3_reg.rds")
imp3 <- readRDS("~/GitHub/dmc2015/pete/predictions/importance_H3_0.3_reg.rds")

names(imp1)[2] <- "h1_imp"
names(imp3)[2] <- "h3_imp"

imp <- merge(imp1, imp3, all = T)

imp <- imp[order(imp$h1_imp, decreasing = T),]
rownames(imp) <- 1:nrow(imp)
saveRDS(imp, "~/GitHub/dmc2015/pete/predictions/importance_BV.rds")

# Penglh basketValue feature matrix 
h1 <- readRDS("~/GitHub/dmc2015/penglh/basketValue_est/features_HTVset1_basketValue_est.rds")
