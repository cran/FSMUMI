#' @title Creating gaps in multivariate time series
#' @author Thi-Thu-Hong Phan, Andre Bigand, Emilie Poisson-Caillault
#' @description This function builds gaps (with the same size) in multivariate time series. 
#' The size of each gap is defined as a percentage of input data length.
#' By default, the position of each gap is randomly chosen.
#' @param data input multivariate time series
#' @param rate size of a gap, as a percentage of input time series size
#' @param ngaps number of gaps to create on each signal 
#' @param begin the starting position of a gap (random by default)
#' @return Creating_gaps returns a multivariate time series with one gap per signal.
#' @examples
#' data(dataFSMUMI)
#' X <- dataFSMUMI[1:5000,]; #reduction for demo
#' rate <- 0.1
#' ngaps <- 1
#' incompleted_signal <- Creating_gaps(X, rate,1)
Creating_gaps <- function(data, rate, ngaps, begin=NULL){
	n=ncol(data)
	data_gap=data
	
	for (icol in 1:n)
	{
		for (igap in 1:ngaps)
			{
			data_gap[,icol]<-Creating_gap_univariate(data[,icol],rate,begin)	
			data=data_gap
		
			}
	}
  
  return(data_gap)
}
