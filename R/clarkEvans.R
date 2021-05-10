#' Clark-Evans index of neighbourhood pattern
#'
#' @param x vector of individual x axis coordinates
#' @param y vector of individual y axis coordinates
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
#' @importFrom sf st_as_sf
#' @importFrom nngeo st_nn
#' 
#' @examples
#' data(bicuar)
#' clarkEvans(bicuar$x, bicuar$y, 10000)
#' 
#' @export
#' 
clarkEvans <- function(x, y, area) {
  dat_sf <- sf::st_as_sf(data.frame(x,y), coords = c("x", "y"))
  
  dists <- suppressMessages(
    nngeo::st_nn(dat_sf, dat_sf, k = 2, progress = FALSE, returnDist = TRUE))

  sums <- sum(unlist(lapply(dists$dist, "[[", 2))) * 2 * sqrt(nrow(dat_sf) / area)

  nsum <- 1 / nrow(dat_sf)

  out <- sums * nsum

  return(out)
}

