library(C50)
library(dplyr)

setwd("/Users/chenhua/Desktop/dmc2015/data/featureMatrix")

featMat_HTV1_l = readRDS("featMat_based-on-HTVset1_LONG_ver0.2.rds")
# featMat_HTV1_w = readRDS("featMat_based-on-HTVset1_WIDE_ver0.rds")
# featMat_HTV2_l = readRDS("featMat_based-on-HTVset2_LONG_ver0.rds")
# featMat_HTV2_w = readRDS("featMat_based-on-HTVset2_WIDE_ver0.rds")
# featMat_HTV3_l = readRDS("featMat_based-on-HTVset3_LONG_ver0.rds")
# featMat_HTV3_w = readRDS("featMat_based-on-HTVset3_WIDE_ver0.rds")

X1.l = featMat_HTV1_l$train$X
y1.l = featMat_HTV1_l$train$y

name_llr_est = grep("llr_est", names(X1.l))
name_llr_naive = grep("llr_naive", names(X1.l))

X1_no_llr = X1.l[, -c(name_llr_est, name_llr_naive)]
X1 = X1_no_llr[, !names(X1_no_llr) %in% c("orderID","couponCol","firstTimeCoup1Rec",
                                          "firstTimeCoup2Rec","firstTimeCoup3Rec")]
y1 = featMat_HTV1_l$train$y


X1_est = cbind(X1, X1.l[ ,name_llr_est])
X1_naive = cbind(X1, X1.l[ ,name_llr_naive])

trn1_est = cbind(y1, X1_est)
trn1_naive = cbind(y1, X1_naive)
trn1 = cbind(y1, X1.l[, !names(X1.l) %in% c("orderID","couponCol","firstTimeCoup1Rec",
                                            "firstTimeCoup2Rec","firstTimeCoup3Rec")])

trn1_est$couponUsed = as.factor(trn1_est$couponUsed)
trn1_naive$couponUsed = as.factor(trn1_naive$couponUsed)
trn1$couponUsed = as.factor(trn1$couponUsed)

mod1.est = C5.0(x = trn1_est[,-c(1:3)], y = trn1_est[,2], rules = TRUE, trials = 20)
c5_imp1_est = C5imp(mod1.est, pcd = TRUE)
C50_imp268 = rownames(c5_imp1_est)[c5_imp1_est>0]
saveRDS(C50_imp268, file = "C50_imp268.rds")


mod1.naive = C5.0(x = trn1_naive[,-c(1:3)], y = trn1_naive[,2], rules = TRUE, trials = 20)
c5_imp1_naive = C5imp(mod1.naive, pcd = TRUE)

mod1 = C5.0(x = trn1[, -c(1:3)], y = trn1[, 2], rules = TRUE, trials = 20)
c5_imp1 = C5imp(mod1, pcd = TRUE)
C50_imp320 = rownames(c5_imp1)[c5_imp1>0]
saveRDS(C50_imp320, file = "C50_imp320.rds")


#############  -------  Testing set  -------  #############

ytst1 = featMat_HTV1_l$validation$y$couponUsed

###################
####  LLR est  ####
###################
tst1_est = featMat_HTV1_l$validation$X %>% 
  select(-c(orderID, couponCol, firstTimeCoup1Rec,
            firstTimeCoup2Rec, firstTimeCoup3Rec), -starts_with("llr_naive"))

#####################
####  LLR naive  ####
#####################
tst1_naive = featMat_HTV1_l$validation$X %>% 
  select(-c(orderID, couponCol, firstTimeCoup1Rec,
            firstTimeCoup2Rec, firstTimeCoup3Rec), -starts_with("llr_est"))

#########################################
####  Combined both LLR est & naive  ####
#########################################

tst1 = featMat_HTV1_l$validation$X %>% 
  select(-c(orderID, couponCol, firstTimeCoup1Rec,
            firstTimeCoup2Rec, firstTimeCoup3Rec))


#############  -------  Prediction  -------  #############
ypred1.est = predict(mod1.est, tst1_est, type = "prob")
ypred1.naive = predict(mod1.naive, tst1_naive, type = "prob")
ypred1 = predict(mod1, tst1, type = "prob")

#############  -------  Loss function  -------  #############
lossFun = function(couponTrue, couponPredict){
  n = length(couponTrue)
  idx1 = seq(1,n, by=3)
  idx2 = seq(2,n, by=3)
  idx3 = seq(3,n, by=3)
  coupon1pred = couponPredict[idx1]
  coupon2pred = couponPredict[idx2]
  coupon3pred = couponPredict[idx3]
  coupon1true = couponTrue[idx1]
  coupon2true = couponTrue[idx2]
  coupon3true = couponTrue[idx3]
  loss1 <- sum((coupon1pred-coupon1true)^2)/(mean(coupon1true))^2
  loss2 <- sum((coupon2pred-coupon2true)^2)/(mean(coupon2true))^2
  loss3 <- sum((coupon3pred-coupon3true)^2)/(mean(coupon3true))^2
  return(c(loss1, loss2, loss3, sum(loss1+loss2+loss3)))
}

lossFun(couponTrue = ytst1, couponPredict = ypred1.est[,2])
lossFun(couponTrue = ytst1, couponPredict = ypred1.naive[,2])
lossFun(couponTrue = ytst1, couponPredict = ypred1[,2])


###############   ------  Wide dataset  ------  ###############
setwd("/Users/chenhua/Desktop/dmc2015/data/featureMatrix")

feat1.wide = readRDS("featMat_based-on-HTVset1_WIDE_ver0.3.rds")
tX1 = feat1.wide$train$X
ty1 = feat1.wide$train$y %>% 
  mutate(couponUsed = paste0(coupon1Used, coupon2Used, coupon3Used))

vX1 = feat1.wide$validation$X
vY1 = feat1.wide$validation$y

ftd_names = Xw1[, grep("ftd", names(Xw1))] %>% names
z_names = Xw1[, grep("_z", names(Xw1))] %>% names
weicheng_names = names(Xw1)[grep("weicheng", names(Xw1))]
llr_1way_names = Xw1[, grep("llr", names(Xw1))] %>% 
  select(-matches("X", ignore.case = FALSE)) %>% names
names_select = c(ftd_names, z_names, weicheng_names, llr_1way_names)

# Training dataset and testing predictors
trn = cbind(yw1[,c(1,6)], Xw1[, names_select])
trn$couponUsed = as.factor(trn$couponUsed)
testX = vX1[, c("orderID", names_select)]

mod1_comb.w = C5.0(x = trn[,-c(1:2)], y = trn[,2], rules = TRUE, trials = 100)
# summary(mod1_comb.w)

pred_cpnComb = predict(mod1_comb.w, testX[,-1], type = "prob")

cpSelectProb = as.data.frame(pred_cpnComb) %>% mutate(
  coupon1Prob = pred_cpnComb[,5]+pred_cpnComb[,6]+pred_cpnComb[,7]+pred_cpnComb[,8],
  coupon2Prob = pred_cpnComb[,c(3)]+pred_cpnComb[,4]+pred_cpnComb[,7]+pred_cpnComb[,8],
  coupon3Prob = pred_cpnComb[,2]+pred_cpnComb[,4]+pred_cpnComb[,6]+pred_cpnComb[,8]
  ) %>% select(coupon1Prob, coupon2Prob, coupon3Prob) %>%
  cbind(testX$orderID)

coupon1Pred = cpSelectProb$coupon1Prob
coupon2Pred = cpSelectProb$coupon2Prob
coupon3Pred = cpSelectProb$coupon3Prob	

coupon1Used = vY1$coupon1Used
coupon2Used = vY1$coupon2Used
coupon3Used = vY1$coupon3Used

sum((coupon1Used - coupon1Pred)^2)/(mean(coupon1Used))^2 +
sum((coupon2Used - coupon2Pred)^2)/(mean(coupon2Used))^2 +
sum((coupon3Used - coupon3Pred)^2)/(mean(coupon3Used))^2




