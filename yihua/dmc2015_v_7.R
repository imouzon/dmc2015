#------------------------------**------------------------------# 
#
# DMC_2015: group users
# dmc2015_v_7.R
# 
#------------------------------**------------------------------# 

setwd('/Users/yihuali/Documents/dmc2015')

dat <- readRDS('./data/featureMatrix/HTVset3.rds')
dat <- readRDS('./data/clean_data/clean_data_v1.rds')
train <- dat$train
class <- dat$class

train$user_group <- as.numeric(cut(train$TimeBtwnRecOrder, 
                                   breaks=seq(0,154,by=1)))  
train$user_group <- as.numeric(cut(train$TimeBtwnSentRec, 
                                   breaks=seq(0,167,by=1)))  
train$user_group <- as.numeric(cut(train$TimeBtwnRecExpire, 
                                   breaks=seq(0,168,by=1)))  
train$user_group <- as.numeric(cut(train$TimeBtwnOrderExpire, 
                                   breaks=seq(0,168,by=1)))

group_bv <- train %>% group_by(user_group) %>% 
  summarize(mean.bv=mean(basketValue))
plot(group_bv$mean.bv)
