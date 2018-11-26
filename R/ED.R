#' @title Euclidean distance (ED)
#' @author Thi-Thu-Hong Phan, Andre Bigand, Emilie Poisson-Caillault
#' @description Compute the Euclidean distance between two vectors having the same length Y and X.
#' @details
#' This function returns the  Euclidean distance of two vectors corresponding to univariate signals.
#' A lower ED (\eqn{ED \in [0, \inf]}) value indicates that the two vectors are more similar.
#' The both vectors Y and X must be of equal length, on the contrary an error will be displayed.
#' In two input vectors, eventual NA will be exluded with a warning diplayed.
#' @param Y vector of imputed values
#' @param X vector of true values
#' @examples
#' data(dataFSMUMI)
#' X <- dataFSMUMI[, 1] ; Y <- dataFSMUMI[, 2]
#' compute.ed(Y,X)

compute.ed <- function(Y,X){
	if(length(Y)!=length(X)){stop("Input vectors are different length !!!")}
	lengthNAX <- sum(is.na(X)) # Number of NA values
	if(lengthNAX > 0){warning(paste("The first vector contains ", lengthNAX, " NA !!! NA excluded", sep = ""))}
	lengthNAY <- sum(is.na(Y)) # Number of NA values
	if(lengthNAY > 0){warning(paste("the second vector contains ", lengthNAY, " NA !!! NA excluded", sep = ""))}
	
    return(sqrt(sum((Y - X)^2)))
}
