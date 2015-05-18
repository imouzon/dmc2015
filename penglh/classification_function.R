# PCA
# K is the number of principal components used
classifierPCA <- function(xTest,xTrain,yTrain,K=5){ # yTrain is factor
  x1 <- scale(xTrain,center=TRUE,scale=TRUE)
  pcx <- prcomp(x1)
  m <- x1%*%pcx$rotation[,1:K]
  y1 <- as.numeric(yTrain)-1
  #  data <- list(x=m,y=y1)
  data <- data.frame(y1,m)
  colnames(data) <- c("y",paste0("x",1:K))
  glm.pcc <- glm(y~.,data=data,family=binomial(link=logit))
  x2 <- (scale(xTest,center=TRUE,scale=TRUE)%*%pcx$rotation)[,1:K]
  newdata <- data.frame(x2)
  colnames(newdata) <- paste0("x",1:K)
  pred <- predict(glm.pcc,newdata,type="response")
  return(pred)
}

# sparse PCA
require(elasticnet)
# K is the number of principal components used;
# para is a vector of length K, 
# which is the thresholding parameter indicates how many dimensions are used 
classifierSPAPCA <- function(xTest,xTrain,yTrain,K=5,para=rep(50,5)){ # yTrain is factor
  x1 <- scale(xTrain,center=TRUE,scale=TRUE)  
  spc <- spca(x1,K=K,para=para,type="predictor",sparse="varnum")
  m.spc <- x1%*%spc$loadings[,1:K]
  y1 <- as.numeric(yTrain)-1
  data <- data.frame(y1,m.spc)
  colnames(data) <- c("y",paste0("x",1:K))
  glm.spc <- glm(y~.,data=data,family=binomial(link=logit))
  x2 <- (scale(xTest,center=TRUE,scale=TRUE)%*%spc$loadings)[,1:K]
  newdata <- data.frame(x2)
  colnames(newdata) <- paste0("x",1:K)
  pred <- predict(glm.spc,newdata,type="response")
  return(pred)
}

# sparse partial least square
require(spls)
classifierSPLS <- function(xTest,xTrain,yTrain,n.fold=10){ # yTrain is factor
  # find tuning parameter in SPLS
  y <- as.numeric(yTrain)-1
  cv <- cv.sgpls(xTrain,y,K=c(1:5),eta=seq(0.1,0.9,0.1),fold=n.fold,scale.x=FALSE,plot.it=FALSE,n.core=6)
  cat(cv$K.opt,cv$eta.opt,"\n")
  c_spls <- sgpls(xTrain,y,K=cv$K.opt,eta=cv$eta.opt,scale.x=FALSE)
  pred <- predict(c_spls,xTest)
#  pred[pred==0] <- -1
  return(pred)
}

# nearest shrunken centroid
require(pamr)
classifierNSC <- function(xTest,xTrain,yTrain){ # yTrain is factor
  mydata <- list(x=t(xTrain),y=yTrain)
  mytrain<- pamr.train(mydata)
  mycv <- pamr.cv(mytrain,mydata)
  pred <- as.numeric(pamr.predict(mytrain, t(xTest),
                                  threshold=min(mycv$threshold[which(mycv$error==min(mycv$error))]),type="posterior"))
  pred[pred==1] <- 0
  pred[pred==2] <- 1
  return(pred)
}