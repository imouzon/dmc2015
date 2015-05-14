
feature_root = "~/dmc2015/features/feature_files/"
set = "set1"
vers = "1"

library(dplyr)
llr_filen = list.files(paste0(feature_dir,set,"/individual/llrs"), pattern="wide")
set_filen = list.files(paste0(feature_dir,set,"/individual"), pattern=".rds")

#llr files are flat and have all order IDs
llr_sets = lapply(llr_filen, readRDS)


data = data.frame(orderID = llr_sets[[1]]$orderID)
for(i in 1:length(llr_filen)) {
	message("Processing: ", llr_filen[i])
	data = data %>% full_join(llr_sets[[i]], by = "orderID")
}

#add back in processed couponUsed.rds
couponUsed2 = readRDS("./clean_coupons_used.rds")
data = data %>% left_join(couponUsed2, by = "orderID")



#let's write this out!
saveRDS(data, "./R/set1Combined.rds")
