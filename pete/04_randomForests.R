# Fit random forest to most current version of feature matrix

# Read in data as RDS. This file includes class and training
featureFiles <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_v3.0.rds")
# Note that train consists of an X data frame and a Y data frame 
train <- featureFiles$train

# Some columns have been added in twice for some reason
# index <- grep(".*\\.y", names(train$X))
# names(train$X)[index]
# train$X <- train$X[,-index]
# index <- grep(".*\\.x", names(train$X))
# names(train$X)[index] <- gsub(".x", "", names(train$X)[index]) 

set.seed(1)
samp <- sample(1:nrow(train), 500, replace = F)

d <- train[samp,-c(12, 20, 28)]

library(randomForest)

# Predict basketValue

# Exclude variables with too many levels:
# couponsReceived, orderTime, userID, productGroup1, ca

rf_basketValue <- randomForest(basketValue~., data = d, importance = T)
importance(rf_basketValue)

train[train == ""]
