#####################
#Request from:
#https://github.com/imouzon/dmc2015/issues/2
#####################

library(lubridate)
library(dplyr)

dataset = readRDS("../../data/clean_data/universalCleanData.rds")
dataset$orderTime = ymd_hms(dataset$orderTime)
dataset$couponsReceived = ymd_hms(dataset$couponsReceived)
dataset_new = dataset[,c(1:4, 33, 43)]

#this function will be helpful later
#aka I can't get anonymouse functions to work inside mutate
time_between_visits = function(times) {
  c(NA, as.duration(diff(times, lag = 1, differences = 1)))/360
}

#1: user number of times visited total
#2: enumerate the total visits
#3: time between visits (hours)
#4: average time between visits (hours)  -- NaN if they only visit once
#5: variance of time between visits (hours) -- NA if they only visit once
#6: median time between visits (hours) -- NA if they only visit once
#7: max time between visits (hours) -- NA if they only visit once
#8: min time between visits (hours) -- NA if they only visit once
#9: Average time between receiving coupon and ordering
#10: Average time of day (in terms of hour) of order
dataset_new = dataset_new %>% 
  group_by(userID) %>% 
  mutate(tot_visits = n(),
         tot_visitOrder = rank(orderTime),
         time_lastVisit = time_between_visits(orderTime),
         avgTime_visit = mean(time_lastVisit, na.rm=T),
         var_visit = var(time_lastVisit, na.rm=T),
         medianTime_visit = median(time_lastVisit, na.rm=T),
         maxTime_visit = max(time_lastVisit, na.rm=T),
         minTime_visit = min(time_lastVisit, na.rm=T),
         avgTime_recd_order = mean(
           as.duration(orderTime - couponsReceived)/360),
         avgTime_order = mean(hour(orderTime))
         )

dataset_new = dataset_new %>% 
	group_by(userID, batchID) %>%
	mutate(batch_visits = n(),
		   batch_visitOrder = rank(orderTime)
		   )

