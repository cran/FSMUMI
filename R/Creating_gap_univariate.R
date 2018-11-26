#' @title Creating a gap in a univariate series
#' @author Thi-Thu-Hong Phan, Andre Bigand, Emilie Poisson-Caillault
#' @description This function creates a gap (successive missing values) within a univariate signal.
#' The size of gap is defined as a percentage of input vector length.
#' By default, the starting position of the gap is chosen randomly.
#' @param X input vector
#' @param rate size of desired gap, as a percentage of input vector size
#' @param begin the begining position of the gap (random by default)
#' @return This function  returns a series with a gap of defined size.
#' @examples
#' data(dataFSMUMI)
#' X <- dataFSMUMI[1:5000, 1] #reduction for demo
#' rate <- 0.1
#' incompleted_signal <- Creating_gap_univariate(X, rate)

Creating_gap_univariate <- function(X, rate, begin=NULL){
  
  Xgap <- X
  
  # No missing data (pass-through)
  if(rate == 0){
    warning("rate = 0, No gap is created")
    return(X)
  }
  
  if(rate == 1){
    stop("rate must be < 1")
  }
  
  gap_size <- round(rate*length(X))
  if(is.null(begin)){
    gap_id <- sample(1:(length(X)-gap_size), 1)
  } else {gap_id <- begin}
  Xgap[gap_id:(gap_id+gap_size-1)] <- NA
    
  # gap_final <- list("output_vector" = Xgap,
                    # "input_vector" = X,
                    # "begin_gap" = gap_id,
                    # "rate" = rate,
                    # "gap_size" = gap_size)
  return(Xgap)
}