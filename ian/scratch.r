#--------------------------------------**--------------------------------------#
#  File Name: scratch.r
#  Purpose:
#
#  Creation Date: 07-04-2015
#  Last Modified: Wed Apr 22 18:21:10 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#
superheroes<-c("name,alignment,gender,publisher",
               "Magneto,bad,male,Marvel",
               "Storm,good,female,Marvel",
               "Mystique,bad,female,Marvel",
               "Batman,good,male,DC",
               "Joker,bad,male,DC",
               "Catwoman,bad,female,DC",
               "Hellboy,good,male,DarkHorseComics")
superheroes <- read.csv(text = superheroes, strip.white = TRUE)

publishers <- c("publisher, yr_founded", "DC, 1934", "Marvel, 1939", "Image, 1992")
publishers <- read.csv(text = publishers, strip.white = TRUE)

library(dplyr)

superheroes %>% left_join(publishers)
publishers %>% left_join(superheroes )
