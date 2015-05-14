combine_set_individual = function(type,set,joinby)
   library(dplyr)
   llr_filenames = list.files(paste0(feature_dir,set,"/individual/llrs"), full.names=TRUE,pattern=type)
   set_filenames = list.files(paste0(feature_dir,set,"/individual"), full.names=TRUE,pattern=c(".rds",type))


   #llr files are flat and have all order IDs
   llr_sets = lapply(llr_filenames, readRDS)

   dsn = data.frame(orderID = llr_sets[[1]]$orderID)
   for(i in 1:length(llr_filenames)) {
      if(i %in% seq(10,length(llr_filenames),10)) message("Processing: ", llr_filenames[i])
      dsn = dsn %>% full_join(llr_sets[[i]], by = joinby)
   }

   #add back in processed couponUsed.rds
   for(x in set_filenames){
      dsn.i = readRDS(x)

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

   #let's write this out!
   saveRDS(dsn, paste0(feature_root,"/R/",set,"Combined.rds"))
}
