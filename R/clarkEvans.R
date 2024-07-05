#' Clark-Evans index of neighbourhood pattern
#'
#' @param x two column matrix of individual x and y coordinates
#' @param area area of the plot, in the same units as \code{x} and 
#'     \code{y} coordinates
#'
#' @return Value of the competition index for the structural unit, i.e. plot.
#' 
#' @details Essentially the sum of nearest neighbour distances of all 
#'     individuals in the plot, normalised by the density of individuals in 
#'     the plot. Bounded between 0 and 2.15, with values <1 indicating a 
#'     clustered distribution. A completely regular hexagonal distribution 
#'     results in the highest value of 2.15.
#' 
#' @references Clark, F. J., Evans, F. C. (1954). Distance to the nearest 
#'     neighbour as a measure of spatial relationships in populations. Ecology. 
#'     Volume 35. Pages 445-453
#' 
#' @examples
#' data(bicuar)
#' clarkEvans(bicuar[,c("x", "y")], 10000)
#' 
#' @export
#' 
clarkEvans <- function(x, area) {
  nn <- nearNeighb(x, k = 1)

  dists <- unlist(lapply(nn, "[[", "nb_dist"))

  sums <- sum(dists) * 2 * sqrt(nrow(x) / area)

  nsum <- 1 / nrow(x)

  out <- sums * nsum

  return(out)
}

