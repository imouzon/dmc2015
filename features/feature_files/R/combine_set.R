combine_set_individual = function(set,feature_dir,type,joinby){
   library(dplyr)
   llr_filenames = list.files(paste0(feature_dir,set,"/individual/llrs"), full.names=TRUE,pattern=type)
   set_filenames = list.files(paste0(feature_dir,set,"/individual"), full.names=TRUE,pattern=c(".rds"))

   #llr files are flat and have all order IDs
   llr_sets = lapply(llr_filenames, readRDS)

   dsn = llr_sets[[1]]
   message("Processing: LLRs")
   for(i in 2:length(llr_filenames)) {
      if(i %in% seq(10,length(llr_filenames),10)) 
      dsn = dsn %>% left_join(llr_sets[[i]], by = joinby)
   }

   #add back in processed couponUsed.rds
   for(x in set_filenames){
      dsn.i = readRDS(x)
      if(sum(joinby %in% names(dsn.i)) != length(joinby)){
         message("File ",x," will not be used (not enough columns to join on)")
      }else{
         dupcols = which(names(dsn.i) %in% names(dsn) & !(names(dsn.i) %in% joinby))
         if(length(dupcols) + length(joinby) == ncol(dsn.i)){
            message("File ",x," will not be used (repeat features)")
         }else{
            if(length(dupcols) > 0) dsn.i = dsn.i[,-dupcols]

            if(nrow(dsn.i) > nrow(dsn)){
               message("File ",x," will not be used (too many rows)")
            }else{
               dsn.i = dsn %>% left_join(dsn.i,by=joinby)
            }

            if(nrow(dsn.i) > nrow(dsn)){
               message("File ",x," will not be used (too many matching joins)")
            }else{
               dsn = dsn.i
            }
         }
      }
   }

   #let's write this out!
   saveRDS(dsn, paste0(feature_root,"/",set,"/",set,"Combined_wide.rds"))

   #return(dsn)
}
