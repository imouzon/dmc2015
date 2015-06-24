#--------------------------------------**--------------------------------------#
#  File Name: stan_test.r
#  Purpose:
#
#  Creation Date: 23-06-2015
#  Last Modified: Tue Jun 23 17:49:03 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#

library(rstan)
xhat<-c(1,2,3,4,5,6,7,8,9,10)
yhat<- 2*xhat+rnorm(1,0,1)

model="
data{
   int<lower=1> n; 
   vector[n]  y;
   vector[n]  x;
}

parameters{
   real Beta;
   real Epislon;
   real<lower=0>  sigma;
}

model{
   y~normal(x*Beta+Epislon,sigma);
   sigma ~ cauchy(0,1);
   Beta ~ normal(0,10);
   Epislon ~ normal(0,10);
}
"

m<- stan_model(model_code=model)

d1<- list(n=10,y=yhat,x=xhat)
r<- sampling(m,data=d1)

