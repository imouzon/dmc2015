#--------------------------------------**--------------------------------------#
#  File Name: peter_model.R
#  Purpose:
#
#  Creation Date: 28-04-2015
#  Last Modified: Tue Apr 28 18:34:15 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#

X = mayjune rates previouscrop timeform 
     3.2      2.1       0 / 1    1,2,3,4,5


vector[5] beta_timeform;

vector[N] timeform;

y[i] = beta0 + beta1*mayjune[i] + beta2*rates[i] + beta3*previouscrop[i] + beta4[timeform[i]]+ beta5*(previouscrop and timeform)

if prevcrop = 1
y = beta0 + beta1*mayjune + beta2*rates + 2*beta3 + beta4*timeform 

if prevcrop = 0
y = beta0 + beta1*mayjune + beta2*rates + beta4*timeform 


#model 2
y = beta0 + beta1*mayjune + beta2*rates 

if prevcrop = 0 and timeform = 1
y = beta0 + beta1*mayjune + beta2*rates + beta[1]
