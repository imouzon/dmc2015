# Code to compute estimated log-likelihood ratios for categorical 
# variables and binary response. The second function is two calculate the 
# log-likelihood ratios for an interaction between two categorical variables.
# This is based on the work by Cory Lanker.

compute_ll <- function(x, y, epsilon = 0.5) {
  # Takes a categorical predictor variable, x, and a binary response
  # variable, y, and produces a vector of the estimated log-likelihood
  # statistics. An epsilon > 0 is added to the numerator and denominator
  # to prevent numerical instability.
  stopifnot(all(y %in% c(0,1)))
  levs <- unique(x)
  lls <- sapply(levs, FUN = function(levs) {
                  log((sum(x == levs & y == 1) + epsilon) / 
                      (sum(x == levs & y == 0) + epsilon))
                })
  res <- lls[x]
  return(res)
}

# Testing code
# --------------------------------------------------
x <- sample(c("red", "blue"), 10, replace = T)
y <- rbinom(10, 1, 0.7)

data.frame(x, y, compute_ll(x, y))
# --------------------------------------------------


compute_ll_2w <- function(x1, x2, y, epsilon = 0.5) {
  # Takes 2 categorical predictor variables, x1 and x2, and a binary response
  # variable, y, and produces a vector of the estimated log-likelihood
  # statistics for all combinations between the levels of each predictor. 
  # An epsilon > 0 is added to the numerator and denominator to prevent 
  # numerical instability.
  stopifnot(all(y %in% c(0,1)))
  levs_1 <- unique(x1)
  levs_2 <- unique(x2)
  lls <- sapply(levs_2, FUN = function(levs_2) {
                  sapply(levs_1, FUN = function(levs_1) {
                           log((sum(x1 == levs_1 & x2 == levs_2 & y == 1) + 0.5) / 
                               (sum(x1 == levs_1 & x2 == levs_2 & y == 0) + 0.5))
                    })
                })
  index <- 1:length(x1)
  res <- sapply(index, FUN = function(index) lls[x1[index], x2[index]])
  return(res)
}

# Testing code
# --------------------------------------------------
x1 <- sample(c("red", "blue"), 10, replace = T)
x2 <- sample(c("square", "triangle"), 10, replace = T)
y <- rbinom(10, 1, 0.5)

data.frame(x1, x2, y, compute_ll_2w(x1, x2, y))
# --------------------------------------------------

# Try on training data 
data <- readRDS("~/GitHub/dmc2015/data/clean_data/clean_data_v1.rds")
train <- data$train 

# Takes less than a second
train$llBrand1 <- compute_ll(train$brand1, train$coupon1Used)
# Takes about 10 seconds
train$llBrand1Prod1 <- compute_ll_2w(train$brand1, 
                                     train$productGroup1, 
                                     train$coupon1Used)
