head(d)
#remove classification rows
hcd = d %>% 
   filter(!is.na(basketValue)) %>% 
   filter(orderID != 2363) %>%
   select(orderID,
          orderTimeDoW,orderTimeWeekend,orderTimeFriSat, orderTimeTime,
          couponsReceivedDoW,couponsReceivedWeekend,couponsReceivedFriSat, couponsReceivedTime,
          TimeBtwnSentRec,
          TimeBtwnRecExpire,
          TimeBtwnRecOrder,
          TimeBtwnOrderExpire,
          reward1,reward2,reward3,
          basketValue) %>% 
          filter(!is.na(basketValue)) %>% 
          filter(orderID != 2363)


CVkset <- function (dsn,k) {
   rowselect = sample(1:nrow(dsn))[1:nrow(dsn)]
   strt=seq(1,nrow(dsn), round(nrow(dsn)/k))
   strt = c(strt,nrow(dsn))
   CVsets = lapply(1:k,function(i) list(predict = dsn[rowselect[strt[i]:(strt[i+1]-1)] ,], learnon = dsn[-rowselect[strt[i]:(strt[i+1]-1)] ,]))
   names(CVsets) = paste0("CV",1:k)
   return(CVsets)
}

#get cluster using centroid
CVclusterChoose = function(nclusters_max,N_CV=10,method="centroid",nclusters_min=2){
   setCV = CVkset(hcd,N_CV)

   CVerror = matrix(0,N_CV,nclusters_max-nclusters_min + 1)

   for(i in 1:N_CV){
      trni = setCV[[paste0("CV",i)]]$learnon
      clsi = setCV[[paste0("CV",i)]]$predict

      #create super groups
      top100 = trni %>% arrange(-basketValue) %>% select(orderID) %>% head(100)
      top500 = trni %>% arrange(-basketValue) %>% select(orderID) %>% head(500)

      trni$superK = 0
      trni$superK[which(trni$orderID %in% top500$orderID)] = 1
      trni$superK[which(trni$orderID %in% top100$orderID)] = 2

      trni$basketValue[which(trni$superK == 2)] = mean(trni$basketValue[which(trni$superK == 2)])
      trni$basketValue[which(trni$superK == 1)] = mean(trni$basketValue[which(trni$superK == 1)])

      #cluster
      hcd_clst = hclust(dist(trni$basketValue))

      covars_col = 2:16
      resp_col = 19

      for(nclusters in nclusters_min:nclusters_max){
         trni$k_centroid = factor(cutree(hcd_clst,k=nclusters))
                   
         X = trni[,covars_col]
         y = trni[,resp_col]
         rft = randomForest(X,y)
         clsi$k_centroid = predict(rft,clsi[,covars_col])

         predVals = trni %>% group_by(k_centroid) %>% summarize(predVal = mean(basketValue))
         predMat = clsi %>% left_join(predVals,by="k_centroid") %>% mutate(obsError = (basketValue - predVal)^2)

         qplot(basketValue, obsError,data=predMat, color=k_centroid)
         predMat %>% select(orderID,basketValue,predVal,obsError) %>% arrange(-obsError) %>% head

         CVerror[i,nclusters-1] = sum(predMat$obsError)
      }
   }
   return(colMeans(CVerror))
}



######
# USING 
#    K_CV = 10
#    kclust = 2:30
#    centroid linkage
hclust_centroid = CVclusterChoose(20,N_CV=10,method="centroid")

qplot(2:20,hclust_centroid)

