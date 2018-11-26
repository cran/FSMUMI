#' @title Root Mean Square Error (RMSE)
#' @author Thi-Thu-Hong Phan, Andre Bigand, Emilie Poisson-Caillault
#' @description Calculate the FA2 between two univariate signals Y (imputed values) and X (true values).
#' @details
#' This function computes the value of RMSE of two univariate signals.
#' A lower RMSE (\eqn{RMSE \in [0, \inf]}) value demonstrates a better performance method for the imputation task.
#' The length of the two vectors Y and X must be equal, on the contrary an error will be displayed.
#' In both input vectors, eventual NA will be exluded with a warning diplayed.
#' @param Y vector of imputed values
#' @param X vector of true values
#' @examples
#' data(dataFSMUMI)
#' X <- dataFSMUMI[, 1] ; Y <- dataFSMUMI[, 2]
#' compute.rmse(Y,X)

compute.rmse<-function(Y,X){
  
  if(length(Y)!=length(X)){stop("Input vectors are of different length !!!")}
  
  lengthNAX <- sum(is.na(X)) # Number of NA values
  if(lengthNAX > 0){warning(paste("Vector of true values contains ", lengthNAX, " NA !!! NA excluded", sep = ""))}
  lengthNAY <- sum(is.na(Y)) # Number of NA values
  if(lengthNAY > 0){warning(paste("Vector of imputed values contains ", lengthNAY, " NA !!! NA excluded", sep = ""))}
  n <- length(X)-max(lengthNAX, lengthNAY)
  
  out=sqrt(sum((Y-X)^2, na.rm = T)/n)
  
  return(out)
  
}