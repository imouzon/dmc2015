library(dplyr)

files = list.files(paste0(getwd(), "/", "set1"), full.names=TRUE)
files = files[grepl("wide", files) | !grepl("llr", files)]
#replace couponUsed.rds with a reformatted version
files = files[-1]
all_files = lapply(files, readRDS)


data = data.frame(orderID = all_files[[1]]$orderID)
for(i in 1:length(all_files)) {
	print(paste0("Processing: ", files[i]))
	data = data %>% full_join(all_files[[i]], by = "orderID")
}

#add back in processed couponUsed.rds
couponUsed2 = readRDS("./clean_coupons_used.rds")
data = data %>% left_join(couponUsed2, by = "orderID")



#let's write this out!
saveRDS(data, "./R/set1Combined.rds")