# pre-clustering

# number of things that could be customized are:
# 1. col: selected which columns to be used for clustering
#         (need to be numeric)
# 2. number of clusters (memb_s <- cutree(hc.s, k=??))
#         (want to make sure no clusters contain only validation set)

col <- c("TimeBtwnSentRec", "TimeBtwnRecExpire", 
         "TimeBtwnRecOrder", "TimeBtwnOrderExpire", 
         "ratio_bp_p", "orderID", "couponUsed")
train <- Feature$T_melt[,col]; train$index <- "T"
valid <- Feature$V_melt[,col]; valid$index <- "V"
c.dat <- rbind(train, valid)
dm_s <- dist(as.matrix(c.dat[,1:(ncol(c.dat)-3)]))
hc_s <- hclust(dm_s)
memb_s <- cutree(hc_s, k=6)
plot(memb_s, col=c.dat$couponUsed+2)
hc_s_result <- data.frame(orderID=c.dat$orderID, 
                          couponUsed=c.dat$couponUsed, 
                          couponcol=rep(c(1,2,3),1513),
                          index=c.dat$index,
                          cluster=memb_s)
hc_s_prob <- hc_s_result %>% group_by(cluster,index) %>%
  summarize(number=n(), Used=sum(couponUsed), 
            NotUsed=n()-sum(couponUsed),
            prob=sum(couponUsed)/n())

