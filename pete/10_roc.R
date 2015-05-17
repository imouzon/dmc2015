# ============================================================================
# File Name: 10_roc.R
#
# Author: Evan P. Walsh
# Contact: epwalsh@iastate.edu
#
# Creation Date: 15-05-2015
# Last Modified: Sat May 16 21:11:52 2015
#
# Purpose: Plot ROC curves for classifiers.
#
# ============================================================================

require(ggplot2)

roc <- function(predictions, actual) {
  cutoffs <- seq(0,1,by=0.005)
  # False positive rate
  fpr <- sapply(cutoffs, FUN = function(cutoffs) {
                  sum(predictions >= cutoffs & actual == 0) / sum(actual == 0)
                })
  # True positive rate
  tpr <- sapply(cutoffs, FUN = function(cutoffs) {
                  sum(predictions >= cutoffs & actual == 1) / sum(actual == 1)
                })
  index <- 1:length(tpr - 1)
  auc <- sum(sapply(index, FUN = function(index) {
                      mean(tpr[index:index+1]) * (fpr[index] - fpr[index+1])
                    }),
             na.rm = T)
  auc <- round(auc, 3)
  qplot(fpr, tpr, geom = "step") + theme_bw() + xlab("False Positive Rate") +
    ylab("True Postive Rate") + 
    annotate("text", x = 0.75, y = 0.25, 
             label = paste("AUC = ", as.character(auc), sep = ""))
}

roc_dat <- function(predictions, actual) {
  cutoffs <- seq(0,1,by=0.005)
  # False positive rate
  fpr <- sapply(cutoffs, FUN = function(cutoffs) {
                  sum(predictions >= cutoffs & actual == 0) / sum(actual == 0)
             })
  # True positive rate
  tpr <- sapply(cutoffs, FUN = function(cutoffs) {
                  sum(predictions >= cutoffs & actual == 1) / sum(actual == 1)
             })
  return(data.frame(fpr, tpr))
}


