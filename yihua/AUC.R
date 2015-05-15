# calculate area-under-curve (ROC)
AUC <- function(predicted, true) {
  roc <- ROC_curve(predicted, true)
  x.diff <- -diff(roc$x)
  y <- roc$y[1:(length(roc$y)-1)]
  return(sum(x.diff * y))
}
