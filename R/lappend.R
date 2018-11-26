#' @title lappend
#' @author Thi-Thu-Hong Phan, Andre Bigand, Emilie Poisson-Caillault
#' @description Appending elements (a vector, a list) to a list.
#' @param lst  a list 
#' @param obj a list or a vector to append 
#' @return Return a list  a new vector of same size with imputed values


.lappend <- function(lst, obj) {
    lst[[length(lst)+1]] <- obj
    return(lst)
}
