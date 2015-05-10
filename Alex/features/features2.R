#####################
# History dependent features for Set1, Set2, Set3
# Basket Value features for set1, set2, set
#####################

set1 = readRDS("../../data/featureMatrix/HTVset1.rds")
set2 = readRDS("../../data/featureMatrix/HTVset2.rds")
set3 = readRDS("../../data/featureMatrix/HTVset3.rds")

h1 = set1$H
h2 = set2$H
h3 = set3$H

# By UserID
h1_new = h1 %>% 
group_by(userID) %>% 
mutate(	mean_basketVal_byUser = mean(basketValue),
	   	med_basketVal_byUser = median(basketValue),
		) %>%


