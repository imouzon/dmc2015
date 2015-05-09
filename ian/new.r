   raw.trn = read.csv("~/dmc2015/data/clean_data/train_simple_name.csv")
head(raw.trn)

# dplyr
##  select
trn.simp = raw.trn %>% select(couponID1,userID,coupon1Used,price1,basePrice1)

## filter: choose rows for a given condition
which(trn.simp$userID == "user1")

trn.simp %>% filter(userID == "user1")

##group_by and summarize
trn.simp %>% group_by(userID) %>% summarize(mean_bprice = mean(basePrice1))

##group_by and mutate
trn.simp %>% group_by(userID) %>% mutate(mean_bprice = mean(basePrice1)) %>% data.frame %>% head(10)

##mutate alone
trn.simp %>% mutate(cheapInd = 1*(price1 < 20)) %>% head

## arrange to order columns
trn.simp %>% mutate(cheapInd = 1*(price1 < 20)) %>% arrange(userID) %>% head
trn.simp %>% mutate(cheapInd = 1*(price1 < 20)) %>% arrange(userID,couponID1) %>% head

## joins 
# left_join(d1,d2,by=c("orderID","userID") 
# this keeps ALL rows from d1 and adds columns from d2 if the corresponding rows in d2
# have matching orderID and userID in d1.
# full_join is like merge with all = TRUE
# right_join = merge with all.y = TRUE

# gather = data long and skinny
trn.simp %>% head
trn.simp %>% gather(varname,values,-couponID1,-userID) %>% arrange(userID,couponID1) %>% head

# spread to make data wider
trn.simp[1:20,] %>% select(userID, couponID1, coupon1Used) %>% spread(couponID1,coupon1Used)



