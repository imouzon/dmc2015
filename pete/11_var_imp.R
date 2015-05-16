# ============================================================================
# File Name: 11_var_imp.R
#
# Author: Evan P. Walsh
# Contact: epwalsh@iastate.edu
#
# Creation Date: 15-05-2015
# Last Modified: Sat May 16 17:42:21 2015
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
# How does this compare to RF?
library(randomForest)
rf <- randomForest(couponUsed~., data = train, mtry = 3, ntree = 50)
h1_v_p2 <- predict(rf, newdata = h1_v)
roc(h1_v_p2, h1_v$couponUsed)

# Next step: var importance on new feature matrix with 1000+ vars.
# Only do this for numeric variables. Factors will take too long.
# ============================================================================
h1 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.3.rds")
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
  saveRDS(importance, "~/GitHub/dmc2015/pete/predictions/importance_H1_0.3.rds")
  cat(nrow(importance))
  cat("\n")
}

importance$imp <- importance$imp * 10000
importance <- importance[order(importance$imp, decreasing = T),]
rownames(importance) <- 1:nrow(importance)
saveRDS(importance, "~/GitHub/dmc2015/pete/predictions/importance_H1_0.3.rds")

importance <- readRDS("~/GitHub/dmc2015/pete/predictions/importance_H1_0.3.rds")

h2 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset2_LONG_ver0.3.rds")
vars <- names(h2$train$X[sapply(h2$train$X, is.numeric)])
vars <- vars[vars != "orderID"]
sizes <- getGroupSize(100, length(vars))
importance <- data.frame(var = NULL, imp = NULL)

for (s in sizes) {
  samp <- sample(vars[!(vars %in% importance$var)], s)
  train <- cbind(couponUsed = h2$train$y$couponUsed, h2$train$X[samp])
  cf <- cforest(couponUsed~., data = train,
                control = cforest_unbiased(mtry = 3, ntree = 50))
  vImp <- varimp(cf)
  temp <- data.frame(var = names(vImp), imp = as.numeric(vImp))
  importance <- rbind(importance, temp)
  saveRDS(importance, "~/GitHub/dmc2015/pete/predictions/importance_H2_0.3.rds")
  cat(nrow(importance))
  cat("\n")
}

importance$imp <- importance$imp * 10000
importance <- importance[order(importance$imp, decreasing = T),]
rownames(importance) <- 1:nrow(importance)
saveRDS(importance, "~/GitHub/dmc2015/pete/predictions/importance_H2_0.3.rds")

h3 <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_based-on-HTVset3_LONG_ver0.3.rds")
vars <- names(h3$train$X[sapply(h3$train$X, is.numeric)])
vars <- vars[vars != "orderID"]
sizes <- getGroupSize(93, length(vars))
importance <- data.frame(var = NULL, imp = NULL)

for (s in sizes) {
  samp <- sample(vars[!(vars %in% importance$var)], s)
  train <- cbind(couponUsed = h3$train$y$couponUsed, h3$train$X[samp])
  cf <- cforest(couponUsed~., data = train,
                control = cforest_unbiased(mtry = 3, ntree = 50))
  vImp <- varimp(cf)
  temp <- data.frame(var = names(vImp), imp = as.numeric(vImp))
  importance <- rbind(importance, temp)
  saveRDS(importance, "~/GitHub/dmc2015/pete/predictions/importance_H3_0.3.rds")
  cat(nrow(importance))
  cat("\n")
}

importance$imp <- importance$imp * 10000
importance <- importance[order(importance$imp, decreasing = T),]
rownames(importance) <- 1:nrow(importance)
saveRDS(importance, "~/GitHub/dmc2015/pete/predictions/importance_H3_0.3.rds")

# Merge importance measures from all three historical sets
imp1 <- readRDS("~/GitHub/dmc2015/pete/predictions/importance_H1_0.3.rds")
imp2 <- readRDS("~/GitHub/dmc2015/pete/predictions/importance_H2_0.3.rds")
imp3 <- readRDS("~/GitHub/dmc2015/pete/predictions/importance_H3_0.3.rds")

names(imp1)[2] <- "h1_imp"
names(imp2)[2] <- "h2_imp"
names(imp3)[2] <- "h3_imp"

imp <- merge(imp1, imp2, all = T)
imp <- merge(imp, imp3, all = T)

imp$m_imp <- rowMeans(imp[,2:4], na.rm=T)

imp <- imp[order(imp$m_imp, decreasing = T),]
rownames(imp) <- 1:nrow(imp)
saveRDS(imp, "~/GitHub/dmc2015/pete/predictions/importance.rds")
