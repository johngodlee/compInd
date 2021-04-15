#' Lorimer's Competition Zone Radius - Lorimer 1983
#'
#' @param k constant, usually 0.4
#' @param n number of trees per hectare
#'
#' @return atomic vector of competition zone radius
#' 
#' @details Estimates the competition zone radius, based on the number of 
#' trees per hectare in the plot multiplied by a constant (\eqn{k}).
#' 
#' @references Lorimer, C. G. (1983). Tests of age-independent competition 
#' indices for individual trees in natural hardwood stands. Forest Ecology and 
#' Management. Volume 6. Pages 343-360.
#' 
#' @export
#' 
lorimerCZR <- function(k, n) {
  k * sqrt(10000 / n)
}

