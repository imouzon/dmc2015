#--------------------------------------**--------------------------------------#
#  File Name:
#  Purpose:
#
#  Creation Date: 29-04-2015
#  Last Modified: Wed May 13 15:54:59 2015
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
   res_d = sqldf(gsub("varn",varn,gsub("orderby",paste(orderby,collapse=", "),"select a.*, b.* from dsn as a left join res_d as b on a.varn = b.varn order by orderby")))
   res_d = res_d[,-(ncol(dsn) + 1)]
   return(res_d)
}
