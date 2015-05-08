td = read.csv("../data/raw_data/DMC_2015_orders_train.txt", sep = "|", stringsAsFactors=F)

#count IDs: there might be a comma separated list for different item categories


ID_Count = function(col) {
	L = list()
	s = strsplit(col, ",")

	unlist(lapply(s, function(x) x))
}

cat_ID = c(ID_Count(td$categoryIDs1), ID_Count(td$categoryIDs2), ID_Count(td$categoryIDs3))
group_ID = c(ID_Count(td$productGroup1), ID_Count(td$productGroup2), ID_Count(td$productGroup3))

#number of product categories
length(table(cat_ID))

#number of product IDs
length(table(group_ID))



# the price associated with the couponIDs is always the same
td %>% group_by(couponID2) %>% summarize(m_price2 = mean(price2), v_price1 = var(price2), n = n())


# one user shows up 30 times in the dataset
td %>% group_by(userID) %>% summarise(n = n())
td %>% filter(userID == "2bab1752b217fdd3704199dead8fa372")

#for a particular user, he might receive a set of coupons and order multiple times before receiving another set.
user1 = td %>% filter(userID == "2bab1752b217fdd3704199dead8fa372")
user1 %>% group_by(couponsReceived) %>% summarise(c1 = sum(coupon1Used), c2 = sum(coupon2Used), c3 = sum(coupon3Used))
user1[12:15,]




#which pairs of coupons appear together
coupon = td %>% select(couponID1, couponID2, couponID3)
L = list()
key1 = paste0(coupon$couponID1, ",", coupon$couponID2)
key2 = paste0(coupon$couponID1, ",", coupon$couponID3)
key3 = paste0(coupon$couponID2, ",", coupon$couponID3)

for(i in 1:nrow(coupon)) {
	if(is.null(L[[key1[i]]])) {
		L[[key1[i]]] = 1
	} else {
		L[[key1[i]]] = L[[key1[i]]] + 1
	}

	if(is.null(L[[key2[i]]])) {
		L[[key2[i]]] = 1
	} else {
		L[[key2[i]]] = L[[key2[i]]] + 1
	}

	if(is.null(L[[key3[i]]])) {
		L[[key3[i]]] = 1
	} else {
		L[[key3[i]]] = L[[key3[i]]] + 1
	}	
}

val = unlist(L)
df = data.frame(do.call(rbind, lapply(strsplit(names(val), ","), function(x) c(x[1], x[2]))))
names(df) = c("ID1", "ID2")
df$num = val

p = ggplot(df, aes(y=ID1,x=ID2))
p = p + geom_tile(aes(fill=num)) + scale_fill_gradient(low="white", high="darkblue") + xlab("") + ylab("")



M = list()
uid = paste0(td$productGroup1)
for(i in 1:nrow(td)) {


}


