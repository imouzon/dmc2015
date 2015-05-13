# Read in latest feature matrix 
class <- read.csv("~/GitHub/dmc2015/data/featureMatrix/class_ver2.0.csv")
train <- read.csv("~/GitHub/dmc2015/data/featureMatrix/train_ver2.0.csv")
# ... or read in long form data
class <- read.csv("~/GitHub/dmc2015/data/clean_data/melted_test_simple_name.csv")
train <- read.csv("~/GitHub/dmc2015/data/clean_data/melted_train_simple_name.csv")
# Combine rows of train and test set
d <- rbind(train[, -c(29:32)], class[, -c(29:32)])
# ... or read in rds format
dataObjects <- readRDS("~/GitHub/dmc2015/data/featureMatrix/featMat_v2.0.rds")
train <- dataObjects$train
class <- dataObjects$class

# Premium products
# ===================================================================
train %>% ggplot(aes(x = factor(premiumProduct), y = basketValue)) + 
  geom_point() + geom_jitter()
# Note: all of the huge basketValue's are associated with non-premium products

train %>% group_by(premiumProduct) %>% 
  summarize(average = mean(basketValue),
            median = median(basketValue), sd = sd(basketValue))

train %>% group_by(premiumProduct, couponUsed) %>%
  summarize(mean = mean(basketValue),
            median = median(basketValue),
            sd = sd(basketValue))
# Note: the highest basketValue's are all from people who did received
# coupons for non-premium products and who did not use the coupons

train %>% group_by(premiumProduct) %>%
  summarize(propUsed = sum(couponUsed) / length(couponUsed),
            nUsed = sum(couponUsed),
            n = length(couponUsed))
# There is not much of a difference in coupon usage between premium and
# non-premium products

# Can we cluster coupon categoryIDs by the proportion of times the 
# corresponding coupon was used and the corresponding basketValue?
# 31 unique coupon IDs
d <- d[1:nrow(train),] # only want the training portion of data
coupIDs <- matrix(rep(NA, 5 * 31), nrow = 31)
for (i in 1:31) {
  # number of times coupon category ID seen
  coupIDs[i,1] <- sum(d[,14+i])   
  # proportion of times coupon used with that category id
  coupIDs[i,2] <- mean(d$couponUsed[d[,14+i] == 1])
  # mean basketValue associated with that category id
  coupIDs[i,3] <- mean(d$basketValue[d[,14+i] == 1])
  # number of times associated with premiumProduct
  coupIDs[i,4] <- sum(d$premiumProduct[d[,14+i] == 1])
  # proportion of times associated with premiumProduct
  coupIDs[i,5] <- mean(d$premiumProduct[d[14+i] == 1])
}
colnames(coupIDs) <- c("count", "propUsed", "mBasketVal", "nPrem", "propPrem")
coupIDs <- data.frame(coupIDs)
coupIDs <- cbind(names(d)[15:45], coupIDs)
names(coupIDs)[1] <- "catID"

qplot(propUsed, mBasketVal, data = coupIDs, geom = "point", 
      size = count, colour = propPrem)
qplot(propUsed, mBasketVal, data = coupIDs, geom = "point",
      size = propPrem)
qplot(propUsed, propPrem, data = coupIDs, goem = "point")
qplot(propPrem, mBasketVal, data = coupIDs, geom = "point")
# Note: there is a strong association between the proportion of times 
# that a categoryID is associated with a premium product and the basketValue

# Coupons categoryIDs
# ===================================================================
d <- rbind(train, class)

d$categoryIDs <- as.character(d$categoryIDs)

catIDs <- sapply(d$categoryIDs, strsplit, split = ":")

outCats <- matrix(rep(NA, 5*nrow(d)), ncol = 5)

for (i in 1:nrow(d)) {
  cats <- unlist(catIDs[[i]])
  for (j in 1:length(cats)) {
    outCats[i,j] <- cats[j]
  }
}

colnames(outCats) <- c("cat1", "cat2", "cat3", "cat4", "cat5")
d <- cbind(d, outCats)
d$cat1 <- as.character(d$cat1)
d$cat2 <- as.character(d$cat2)
d$cat3 <- as.character(d$cat3)
d$cat4 <- as.character(d$cat4)
d$cat5 <- as.character(d$cat5)
catM <- melt(d, id = c("orderID", "couponCol"), 16:20)
catI <- dcast(catM, orderID + couponCol ~ value, length, fill = 0)

d <- d[, -c(16:20)]

d <- cbind(d, catI[,-c(1:2)])
d <- d[, -ncol(d)]
# We now have indicator columns for all 31 categories

