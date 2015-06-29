#--------------------------------------**--------------------------------------#
#  File Name: paired_preds.R
#  Purpose:
#
#  Creation Date: 29-06-2015
#  Last Modified: Mon Jun 29 13:33:40 2015
#  Created By:
#
#--------------------------------------**--------------------------------------#
#
#  FORTRAN and C: 
#  source('~/R/shlib/C_FORTRAN.shlib.r')
#  .Fortran("subroutine name",as.integer(input1),as.double(input2), etc)
#

require(dplyr)
require(tidyr)
# paired predictions
team1 = read.csv("~/dmc2015/predictions/predictions_based_on_3.csv")
team2 = read.csv("~/dmc2015/predictions/predictions_based_on_1.csv")

names(team1) = c("orderID","cpn1","cpn2","cpn3")
names(team2) = c("orderID","cpn1","cpn2","cpn3")

team1 = team1 %>% gather("cpn_col","pred_team1",-orderID)
team2 = team2 %>% gather("cpn_col","pred_team2",-orderID)

teams = team1 %>% left_join(team2,by=c("orderID","cpn_col"))

corrs = teams %>% group_by(cpn_col) %>% summarize(corr = cor(pred_team1,pred_team2)) %>% mutate(label = gsub("cpn","Coupon column ", with(corrs,paste0(cpn_col,": ",round(corr,3)))))

teamcorr = teams %>% left_join(corrs,by="cpn_col")


library(ggplot2)
qplot(pred_team1,pred_team2,data=teamcorr,alpha=I(.3),facets=.~label,xlim = c(0,.7), ylim = c(0,.7)) + coord_fixed(ratio=1) + geom_abline(intercept=0,slope=1,color="red")

teamdens = teamcorr %>% select(pred_team1,pred_team2,cpn_col) %>% gather("team","prediction",-cpn_col) %>% mutate(team = gsub("pred_team","Team ",team))
qplot(prediction,geom="density",fill = team,alpha = I(.3),data=teamdens)
qplot(prediction,geom="density",fill = team,alpha = I(.3),facets=.~cpn_col,data=teamdens)
