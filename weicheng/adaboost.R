library(adabag)
data("iris")
train <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))

## Boosting
## ========
iris.adaboost <- boosting(Species ~ ., data = iris[train, ], mfinal =
                              10, control = rpart.control(maxdepth = 1))

iris.adaboost

## Variables relative importance for boosting in the iris example
barplot(iris.adaboost$imp[order(iris.adaboost$imp, decreasing = TRUE)],
        ylim = c(0, 100), main = "Variables Relative Importance",
        col = "lightblue")

## the confusion matrix for the training set
table(iris.adaboost$class, iris$Species[train],
      dnn = c("Predicted Class", "Observed Class"))
## Error
1 - sum(iris.adaboost$class == iris$Species[train]) /
    length(iris$Species[train])

## Predict
iris.predboosting <- predict.boosting(iris.adaboost,
                                      newdata = iris[-train, ])
iris.predboosting

## cross validation
iris.boostcv <- boosting.cv(Species ~ ., v = 10, data = iris, mfinal = 10,
                            control = rpart.control(maxdepth = 1))
iris.boostcv

## Bagging (Have problems with this function.)
## ========
dat = iris[train, ]
iris.bagging <- bagging(Species ~ ., data = dat, mfinal = 10,
                        control = rpart.control(maxdepth = 1))
iris.bagging

barplot(iris.bagging$imp[order(iris.bagging$imp, decreasing = TRUE)],
        ylim = c(0, 100), main = "Variables Relative Importance",
        col = "lightblue")

table(iris.bagging$class, iris$Species[train],
      dnn = c("Predicted Class", "Observed Class"))

1 - sum(iris.bagging$class == iris$Species[train]) /
    length(iris$Species[train])


## multiclass example
data("Vehicle")
l <- length(Vehicle[ ,1])
sub <- sample(1:l, 2 * l/3)
maxdepth <- 5
mfinal <- 50
cntrl <- rpart.control(maxdepth = 5, minsplit = 0, cp = -1)

Vehicle.bagging <- bagging(Class ~ ., data = Vehicle[sub, ],
                           mfinal = mfinal, control = cntrl)
1 - sum(Vehicle.bagging$class == Vehicle$Class[sub]) /
    length(Vehicle$Class[sub])

Vehicle.predbagging <- predict.bagging(Vehicle.bagging,
                                       newdata = Vehicle[-sub, ])
Vehicle.predbagging$confusion
Vehicle.predbagging$error


Vehicle.adaboost <- boosting(Class ~., data = Vehicle[sub, ],
                             mfinal = mfinal, coeflearn = "Freund", boos = TRUE, control = cntrl)
1 - sum(Vehicle.adaboost$class == Vehicle$Class[sub])/
    length(Vehicle$Class[sub])
Vehicle.adaboost.pred <- predict.boosting(Vehicle.adaboost,
                                          newdata = Vehicle[-sub,])
Vehicle.adaboost.pred$confusion
Vehicle.adaboost.pred$error


Vehicle.SAMME <- boosting(Class ~ ., data = Vehicle[sub, ],
                          mfinal = mfinal, coeflearn = "Zhu", boos = TRUE, control = cntrl)
1 - sum(Vehicle.SAMME$class == Vehicle$Class[sub]) /
    length(Vehicle$Class[sub])

Vehicle.SAMME.pred <- predict.boosting(Vehicle.SAMME,
                                       newdata = Vehicle[-sub, ])
Vehicle.SAMME.pred$confusion
Vehicle.SAMME.pred$error

## Margins for bagging in the Vehicle data.
margins.train <- margins(Vehicle.bagging, Vehicle[sub, ])[[1]]
margins.test <- margins(Vehicle.bagging.pred, Vehicle[-sub, ])[[1]]
plot(sort(margins.train), (1:length(margins.train)) /
          length(margins.train), type = "l", xlim = c(-1,1),
      main = "Margin cumulative distribution graph", xlab = "m",
      ylab = "% observations", col = "blue3", lty = 2, lwd = 2)
abline(v = 0, col = "red", lty = 2, lwd = 2)
lines(sort(margins.test), (1:length(margins.test)) / length(margins.test),
      type = "l", cex = .5, col = "green", lwd = 2)
legend("topleft", c("test", "train"), col = c("green", "blue3"),
       lty = 1:2, lwd = 2)


## Margins for AdaBoost.M1 in the Vehicle data.
margins.train <- margins(Vehicle.adaboost, Vehicle[sub, ])[[1]]
margins.test <- margins(Vehicle.adaboost.pred, Vehicle[-sub, ])[[1]]
plot(sort(margins.train), (1:length(margins.train)) /
          length(margins.train), type = "l", xlim = c(-1,1),
      main = "Margin cumulative distribution graph", xlab = "m",
      ylab = "% observations", col = "blue3", lty = 2, lwd = 2)
abline(v = 0, col = "red", lty = 2, lwd = 2)
lines(sort(margins.test), (1:length(margins.test)) / length(margins.test),
      type = "l", cex = .5, col = "green", lwd = 2)
legend("topleft", c("test", "train"), col = c("green", "blue3"),
       lty = 1:2, lwd = 2)

## Margins for SAMME in the Vehicle data.
margins.train <- margins(Vehicle.SAMME, Vehicle[sub, ])[[1]]
margins.test <- margins(Vehicle.SAMME.pred, Vehicle[-sub, ])[[1]]
plot(sort(margins.train), (1:length(margins.train)) /
          length(margins.train), type = "l", xlim = c(-1,1),
      main = "Margin cumulative distribution graph", xlab = "m",
      ylab = "% observations", col = "blue3", lty = 2, lwd = 2)
abline(v = 0, col = "red", lty = 2, lwd = 2)
lines(sort(margins.test), (1:length(margins.test)) / length(margins.test),
      type = "l", cex = .5, col = "green", lwd = 2)
legend("topleft", c("test", "train"), col = c("green", "blue3"),
       lty = 1:2, lwd = 2);


#################
#################
dset1 = readRDS("../data/featureMatrix/HTVset1.rds")

H = dset1$H
V = dset1$V
T = dset1$T
C = dset1$C


T1 = T[, c(2,4,6,7,8,9,10,11,29,33,34,35,36,37,38,39,40,41,42, 43:49)]
#T1 = T1[,c(3,4,5,6,9)]
T1$coupon1Used = as.factor(T1$coupon1Used)

V1 = V[, c(2,4,6,7,8,9,10,11,29,33,34,35,36,37,38,39,40,41,42, 43:49)]
#V1 = V1[,c(3,4,5,6,9)]
V1$coupon1Used = as.factor(V1$coupon1Used)

dmc.adaboost <- boosting(coupon1Used ~., data = T1, mfinal = 10)

1 - sum(dmc.adaboost$class == T1$coupon1Used)/
    length(T1$coupon1Used)

dmc.adaboost.pred <- predict.boosting(dmc.adaboost, newdata = V1)

dmc.adaboost.pred$confusion
dmc.adaboost.pred$error


dmc = readRDS("../data/featureMatrix/featMat_v3.0.rds")

cUsed = readRDS("./feature/couponUsed.rds")
T2 = cbind(T1, cUsed$T)
V2 = cbind(V1, cUsed$V)

## H1 = H[, c(2,4,6,7,8,9,10,11,29,33,34,35,36,37,38,39,40,41,42, 43:49)]
##H2 = cbind(H1, trn[,-c(1:11, 15:23)])
##dmc.adaboost <- boosting(coupon1Used ~., data = H2, mfinal = 10)


dmc.adaboost <- boosting(coupon1Used ~., data = T2, mfinal = 10)

table(dmc.adaboost$class, T2$coupon1Used,
      dnn = c("Predicted Class", "Observed Class"))

1 - sum(dmc.adaboost$class == T2$coupon1Used)/
    length(T2$coupon1Used)

dmc.adaboost.pred <- predict.boosting(dmc.adaboost, newdata = V2)

dmc.adaboost.pred$confusion
dmc.adaboost.pred$error


dmc.SAMME <- boosting(coupon1Used ~ ., data = T2, mfinal = 10,
                      coeflearn = "Zhu", boos = TRUE)
1 - sum(dmc.SAMME$class == T2$coupon1Used) /
    length(T2$coupon1Used)

dmc.SAMME.pred <- predict.boosting(dmc.SAMME,
                                       newdata = V2)
dmc.SAMME.pred$confusion
dmc.SAMME.pred$error



dat = readRDS("./data/dat_1111.rds")
str(dat)
trt = dat$trt

col_pred <-
    c(which(names(trt)%in%c("couponsReceivedTime","couponsReceivedDoW","batchID",
                            "TimeBtwnRecOrder", "price","basePrice","reward","premiumProduct", "couponCol","ratio_bp_p", "couponUsed"))
     ,which(grepl("prob",names(trt))))

trt1 = trt[, col_pred]
trt1$couponUsed = as.factor(trt1$couponUsed)
tst = dat$tst
tst1 = tst[, col_pred]
tst1$couponUsed = as.factor(tst1$couponUsed)

cntrl <- rpart.control(maxdepth = 5, minsplit = 0, cp = -1)
dmc.adaboost <- boosting( couponUsed~., data = trt1, control = cntrl)

table(dmc.adaboost$class, trt1$couponUsed,
      dnn = c("Predicted Class", "Observed Class"))

1 - sum(dmc.adaboost$class == trt1$couponUsed)/
    length(trt1$couponUsed)

dmc.adaboost.pred <- predict.boosting(dmc.adaboost, newdata = tst1)
dmc.adaboost.pred$confusion
dmc.adaboost.pred$error

dmc.SAMME <- boosting(couponUsed~., data = trt1, coeflearn = "Zhu", boos = TRUE)
table(dmc.SAMME$class, trt1$couponUsed,
      dnn = c("Predicted Class", "Observed Class"))
1 - sum(dmc.SAMME$class == trt1$couponUsed)/
    length(trt1$couponUsed)

dmc.SAMME.pred <- predict.boosting(dmc.SAMME, newdata = tst1)
dmc.SAMME.pred$confusion
dmc.SAMME.pred$error
