#--------------------------------------**--------------------------------------#
#  File Name:
#  Purpose:
#
#  Creation Date: 12-05-2015
#  Last Modified: Tue May 12 19:39:33 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#

clean_factor = function(dsn,rootnm,scrape_off=NULL,NAreplace=NULL){
   colstoclean = (1:ncol(dsn))[grepl(rootnm,names(dsn))]
   for(i in colstoclean) dsn[,i] = as.character(dsn[,i])

   if(!is.null(NAreplace)) for(i in colstoclean) dsn[is.na(dsn[,i]),i] = NAreplace

   lvls = unique(as.vector(sapply(colstoclean, function(i) dsn[,i])))
   if(!is.null(scrape_off)) lvls = as.numeric(gsub(scrape_off,'',lvls))

   lvls = lvls[order(lvls)]
   if(!is.null(scrape_off)) lvls = paste0(scrape_off,lvls)

   for(i in colstoclean) dsn[,i] = factor(dsn[,i],levels = lvls)

   return(dsn)
}
