#' @title Global threshold for missing data imputation
#' @author Thi-Thu-Hong Phan, Andre Bigand, Emilie Poisson-Caillault
#' @description Computing the new similarity measure between two univariate signals  Y and X  based on fuzzy logic .
#' @details
#' This function returns the similarity measure of two vectors \eqn{0 <= fbsm <= 3}.
#' fbsm is close to 3 the two vectors are more similar.
#' Both vectors Y and X must be of equal length, otherwise an error will be appreared.
#' The input vectors do not contain NA.
#' @param Y input vector 
#' @param X input vector 
#' #' @examples
#' data(dataFSMUMI)
#' X <- dataFSMUMI[, 1] ; Y <- dataFSMUMI[, 2]
#' fbsm(Y,X)
#' @import FuzzyR lsa
#' @keywords internal


fbsm<-function(Y, X){
		cosi=cosine(Y, X)	
		sim=compute.sim(Y,X)
		ed=compute.ed(Y,X)
		ed=1/(1+ed)
		input=t(c(cosi,sim,ed))
		fis=create.Myfis()
		w=evalfis(input, fis)
		fsb=w[1]*cosi + w[2]*sim + w[3]*ed	
return (fsb)

}	
