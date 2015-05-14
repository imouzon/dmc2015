library(dplyr)

files = list.files(paste0(getwd(), "/", "set3"), full.names=TRUE)
files = files[grepl("wide", files) | !grepl("llr", files)]
all_files = lapply(files, readRDS)

data = data.frame(orderID = all_files[[1]]$orderID)
for(i in 1:length(all_files)) {
	print(paste0("Processing: ", files[i]))
	data = data %>% full_join(all_files[[i]], by = "orderID")
}

#let's write this out!
saveRDS(data, "./R/set3Combined.rds")
