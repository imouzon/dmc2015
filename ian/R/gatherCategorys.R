#--------------------------------------**--------------------------------------#
#  File Name:
#  Purpose:
#
#  Creation Date: 13-05-2015
#  Last Modified: Wed May 13 18:06:15 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#

splitColumn = function(dsn,varn,orderby,splitby=","){
   library(sqldf)
   res_d = dsn[,c(orderby,varn)] %>% 
      mutate(parts = strsplit(as.character(dsn[,varn]),splitby)) %>%
      group_by_(varn) %>%
      do(data.frame({
         idx = 1:length(.$parts[[1]])
         lst = lapply(idx, function(x) .$parts[[1]][x])
         names(lst) = lapply(idx,function(x) paste0(varn,x))
           (lst)
         }, stringsAsFactors=FALSE)
      )
   res_d = dsn %>% left_join(res_d,by=varn) %>% arrange_(orderby)
   sqldf(gsub("varn",varn,gsub("orderby",paste(orderby,collapse=", "),"select a.*, b.* from dsn as a left join res_d as b on a.varn = b.varn order by orderby")))
   res_d = res_d[,-(ncol(dsn) + 1)]
   return(res_d)
}
gatherCategorys = function(dsn){
   dsnc = splitColumn(dsn,"categoryIDs","orderID",splitby=":") 
   dsnc = dsnc[,-which(names(dsnc) == "categoryIDs")]
   dsnc = clean_factor(dsnc,"categoryIDs",scrape_off="cat")
   dsnc = dsn %>% 
      left_join(dsnc %>%
            select(orderID,couponCol,categoryIDs1, categoryIDs2, categoryIDs3, categoryIDs4, categoryIDs5) %>%
            gather(tmp,categoryID,-orderID,-couponCol) %>%
            mutate(categoryEntry = as.numeric(gsub("categoryIDs","",tmp))) %>%
            select(orderID,couponCol,categoryEntry,categoryID) %>%
            arrange(orderID,couponCol,categoryEntry) 

   if(drop.na) dsnc = 
            filter(!is.na(categoryID)),
         by = c("orderID","couponCol")) %>%
      arrange(orderID,couponCol,categoryEntry)

