# Fit random forest to most current version of feature matrix

# dataObject <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_v1.1.rds")
# train <- dataObject$train

train <- read.csv("~/GitHub/dmc2015/data/featureMatrix/train_ver1.1.csv", stringsAsFactors = T)

set.seed(1)
samp <- sample(1:nrow(train), 500, replace = F)
# How do we handle factors with more than 53 levels?
d <- train[samp,-c(12, 20, 28)]

library(randomForest)

# Predict basketValue

# Exclude variables with too many levels:
# couponsReceived, orderTime, userID, productGroup1, ca

rf_basketValue <- randomForest(basketValue~., data = d, importance = T)
importance(rf_basketValue)

train[train == ""]
