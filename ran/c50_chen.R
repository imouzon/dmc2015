###############   ------  Wide dataset  ------  ###############

feat1.wide = readRDS("//Users/Ran/Google Drive/ISU/dmc2015/data/featureMatrix/featMat_based-on-HTVset1_WIDE_ver0.3.rds")
tX1 = feat1.wide$train$X
ty1 = feat1.wide$train$y %>% 
  mutate(couponUsed = paste0(coupon1Used, coupon2Used, coupon3Used))

vX1 = feat1.wide$validation$X
vY1 = feat1.wide$validation$y

pif_names = tX1[, grep("pif", names(tX1))] %>% names
invmax_names = tX1[, grep("invmax", names(tX1))] %>% names
orig_names = tX1[, grep("orig", names(tX1))] %>% names
weicheng_names = names(tX1)[grep("wei", names(tX1))]
llr_1way_names = tX1[, grep("llr", names(tX1))] %>% 
  select(-matches("X", ignore.case = FALSE)) %>% names
llr_est_names = llr_1way_names[grep("llr_est", llr_1way_names)]

names_select = c(pif_names, invmax_names, orig_names, weicheng_names, llr_est_names)
sub_est = tX1[, names(tX1) %in% names_select]

# Training dataset and testing predictors
trn = cbind(ty1, sub_est)
trn$couponUsed = as.factor(trn$couponUsed)
mod1_comb.w = C5.0(x = trn[,-c(1:6)], y = trn[,6], rules = TRUE, trials = 100)
summary(mod1_comb.w)

testX = vX1[, c("orderID", names_select)]
pred_cpnComb = predict(mod1_comb.w, testX[,-1], type = "prob")

# weight
HTV1 <- readRDS("//Users/Ran/Google Drive/ISU/dmc2015/data/featureMatrix/HTVset1.rds")
H1 <- HTV1$H
cpnUsed <- H1[, 29:31] %>% mutate(couponUsed = paste0(coupon1Used, coupon2Used, coupon3Used))
prop <- summary(as.factor(cpnUsed$couponUsed))
ww <- as.numeric(prop)/sum(prop)

pred_cpnComb_w = t(t(pred_cpnComb) * ww)
pred_cpnComb_scale = pred_cpnComb_w / rowSums(pred_cpnComb_w)

cpSelectProb = as.data.frame(pred_cpnComb) %>% mutate(
  coupon1Prob = pred_cpnComb[,5]+pred_cpnComb[,6]+pred_cpnComb[,7]+pred_cpnComb[,8],
  coupon2Prob = pred_cpnComb[,3]+pred_cpnComb[,4]+pred_cpnComb[,7]+pred_cpnComb[,8],
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

a <- data.frame(coupon1Pred, coupon2Pred, coupon3Pred, coupon1Used, coupon2Used, coupon3Used)


