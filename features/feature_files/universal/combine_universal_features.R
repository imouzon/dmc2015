##########
#' combines RDS files for universal features
##########
#' Requires RDS files to have the same number of rows as 
#' the universalCleanData.  Must have an "orderID" column.
#' Each individual RDS file should not have any duplicated 
#' columns with the universalCleanData except for orderID.
#'

combine_universal_features = function(read_dir = "./individual", 
	write_dir = "./combined",
	universal_clean = "../../../data/clean_data/universalCleanData.rds",
	exclude = c("basePrice_price_ratio.rds"),
   joinby = "orderID") {

	#original dataset
	universalCleanData = readRDS(universal_clean)
	n_obs = nrow(universalCleanData)

	#read in files
	file_names = list.files(read_dir)[!(list.files(read_dir) %in% exclude)]
	file_names = paste(read_dir, file_names, sep = "/")
	files_list = lapply(file_names, readRDS)

	#make sure the dimensions are the same
	if(any(sapply(files_list, nrow) != n_obs)) {
		mismatched_files = file_names[sapply(files_list, nrow) != n_obs]
		mismatched_files = paste(mismatched_files, collapse = ", ")
		stop(paste0("These files don't match in dimension to universal features: \n", mismatched_files))
	}

	data = universalCleanData
	for(i in 1:length(files_list)) {
		print(paste0("Merging: ", file_names[[i]]))
      data.i = files_list[[i]]
      data.i = data.i[, which(!(names(data.i) %in% names(data)) | names(data.i) %in% joinby)]
		data.i = data %>% full_join(data.i, by = joinby)
      if(nrow(data.i) > nrow(data)){
         message("Not using ",files_list[[i]],": too many matches")
      }else{
         data = data.i
      }
	}

	return(data)
}




#' This particular file needs additional processing to make it mergable
fix_basePrice_ratio = function(file_path = "./individual/basePrice_price_ratio.rds") {
	d = readRDS(file_path)

	d2 = d %>% 
	group_by(orderID) %>% 
	summarize(bPr2pr_ratio1 = mean(bPr2pr_ratio1, na.rm=T),
		bPr2pr_ratio2 = mean(bPr2pr_ratio2, na.rm=T),
		bPr2pr_ratio3 = mean(bPr2pr_ratio3, na.rm=T),
		bPr2pr_approx_ratio1 = mean(bPr2pr_approx_ratio1, na.rm=T),
		bPr2pr_approx_ratio2 = mean(bPr2pr_approx_ratio2, na.rm=T),
		bPr2pr_approx_ratio3 = mean(bPr2pr_approx_ratio3, na.rm=T)
		)

	saveRDS(d2, file = "./individual/basePrice_price_ratio2.rds")
}
