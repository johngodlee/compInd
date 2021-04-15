#' Calculate basal area from DBH 
#'
#' @param x numeric vector of DBH values
#'
#' @return numeric vector
#' 
#' @examples 
#' a <- c(1.23, 5.67, 10.11)
#' basalArea(a)
#' @export
#' 
basalArea <- function(x){ 
  pi * (x/2)^2
}


