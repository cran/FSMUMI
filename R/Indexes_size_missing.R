#' @title Indexes and sizes of gaps
#' @author Thi-Thu-Hong Phan, Andre Bigand, Emilie Poisson-Caillault
#' @description Find the first positions of all gaps and their sizes respectively  within a multivariate time series.
#' @param data a multivariate data
#' @return Return a list  per signal in which each element containing the position of the starting of a gap (1st column) and its size (2nd column).
#' 
#' @examples
#' data(dataFSMUMI)
#' X <- dataFSMUMI
#' rate <- 0.1
#' ngaps <- 1
#' incompleted_signal <- Creating_gaps(X, rate,1)
#' id_NA <- Indexes_size_missing(incompleted_signal)

Indexes_size_missing <- function(data){
  store_miss <- list()
  for(icol in 1:ncol(data)){
    pos_w <- c()
    num_w <- c()
    count <- 0
    i <- 1
    while(i < nrow(data)){
      if(is.na(data[i, icol])==TRUE){
        count <- count+1
      }
      if(is.na(data[i+1, icol])==FALSE){
        if(count>0){
          num_w <- c(num_w, count)
          pos_w <- c(pos_w, i-count+1)
        }
        count <- 0
      }
      i <- i+1
    }
    store_w <- matrix(0, ncol=2, nrow=length(pos_w))
    store_w[, 1] <- pos_w
    store_w[, 2] <- num_w
	store_miss=.lappend(store_miss,store_w)
    # store_miss <- rlist::list.append(store_miss, store_w)
  }
  return(store_miss)
}