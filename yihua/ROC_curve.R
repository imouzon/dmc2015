# return x and y for ROC curve plot
# Input:
#   predicted: probability version prediction
#   true: actual 0/1
ROC_curve <- function(predicted, true) {
  thresh <- predicted[order(predicted)]
  thresh <- unique(thresh)
  result <- data.frame(x=c(1, rep(NA,length(thresh)-1), 0), 
                       y=c(1, rep(NA,length(thresh)-1), 0))
  for (i in 1:(length(thresh)-1)) {
    tmp <- table(true, as.numeric(predicted>thresh[i]))
    (result$y[i+1] <- tmp[2,2]/sum(true))
    (result$x[i+1] <- tmp[1,2]/(length(true)-sum(true)))
  }
  return(result)
}