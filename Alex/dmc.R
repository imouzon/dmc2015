#count IDs: there might be a comma separated list for different item categories


ID_Count = function(col) {
	L = list()
	s = strsplit(col, ",")

	unlist(lapply(s, function(x) x))
}

cat_ID = c(ID_Count(td$categoryIDs1), ID_Count(td$categoryIDs2), ID_Count(td$categoryIDs3))
prod_ID = c(ID_Count(td$productGroup1), ID_Count(td$productGroup2), ID_Count(td$productGroup3))

#number of product categories
length(table(cat_ID))

#number of product IDs
length(table(prod_ID))



