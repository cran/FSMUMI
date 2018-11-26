#' @title Completing isolated missing points (1NA)
#' @author Thi-Thu-Hong Phan, Andre Bigand, Emilie Poisson-Caillault
#' @description Complete isolated missing points  using the average of nearest neighbours.
#' @param signal a univariate time series containing isolated missing values.
#' @param pos1 the position of the begining of gaps of size 1,  obtained  from Indexes_size_missing() function.
#' This function returns a series with imputed values of all 1NAs. 

imp_1NA<-function(signal,pos1){
imp_value1=c()
N=length(signal)
for (i in pos1)
{
	if((i>1)&&(i<N)) imp=(signal[i-1]+signal[i+1])/2
	if (i==1) imp=signal[i+1]
	if (i==N) imp=signal[N-1]
	imp_value1=c(imp_value1,imp)
}
return(imp_value1)
}