##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title savePred
##' @param d d = readRDS("../data/featureMatrix/featMat_based-on-HTVset1_LONG_ver0.3.rds")
##' @param validation prediction for the validation set
##' @param class.T prediction for the class set using training data only
##' @param class.TV prediction for the class set using training and validation data sets
##' @param err loss value
##' @param method string, like "pca", "c5.0", "ada", "svm" and so on
##' @param file path to save the file, ex: "~/dmc2015/"
##' @return list
##' @author 
savePred <- function(d, validation, class.T, class.TV, err, method, file){
    V = d$validation$y[,1, drop=FALSE]
    C = d$class$y[,1, drop=FALSE]
    V.n = nrow(V)
    C.n = nrow(C)
    if(V.n != length(validation))
        stop("Validation set length error!")
    if(C.n != length(class.T))
        stop("Classification set length error!")
    V$couponCol = rep(1:3, V.n/3)
    C$couponCol = rep(1:3, C.n/3)
    colnames = paste0(method, c(".pred", ".pred.T", ".pred.TV"))
    V[colnames[1]] = validation
    C[colnames[2]] = class.T
    C[colnames[3]] = class.TV
    res = list(validation = V, class = C, err = err)
    saveRDS(res, file)
}
