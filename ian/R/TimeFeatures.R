#--------------------------------------**--------------------------------------#
#  File Name:
#  Purpose:
#
#  Creation Date: 12-04-2015
#  Last Modified: Wed Apr 15 16:21:36 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)

TimeFeatures = function(dsn,varn,idvar="orderID"){
   #needs lubridate
   library(lubridate)
   library(plyr)
   #don't overwrite you data
   stopifnot(!(paste0(varn,'DoW') %in% names(dsn)))
   
   #store time variables in this set
   dsn.time = dsn[,c(idvar,varn)]

   # add information about order dates
   dsn.time[,varn] = ymd_hms(dsn.time[,varn])

   #split the date-time variable into date and time
   timedate = ldply(strsplit(dsn[,varn]," "))

   #get time of day, date, and day of week alone
   dsn.time$Date = ymd(timedate[,1])
   #dsn.time$Time = as.numeric(as.interval(hms(timedate[,2])))
   dsn.time$Time = hms(timedate[,2])

   #get the day of the week
   dsn.time$DoW = wday(dsn.time[,varn],label=TRUE,abbr=FALSE)

   #weekend or preweekend indicators
   dsn.time$Weekend = factor(1*(dsn.time$DoW %in% c("Saturday", "Sunday")))
   dsn.time$FriSat = factor(1*(dsn.time$DoW %in% c("Friday","Saturday")))

   #merge with original data set
   names(dsn.time)[-c(1:2)] = paste0(varn,names(dsn.time)[-c(1:2)])

   dsn = merge(dsn.time, dsn[,names(dsn) != varn], by=idvar)
   return(dsn)
}
