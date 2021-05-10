#' Pielou's index of non-randomness
#'
#' @param x vector of individual x axis coordinates
#' @param y vector of individual y axis coordinates
#' @param xmin minimum x coordinate in plot
#' @param xmax maximum x coordinate in plot
#' @param ymin minimum y coordinate in plot
#' @param ymax maximum y coordinate in plot
#' @param k number of randomly allocated sample points
#'
#' @return value of the competition index for the structural unit, i.e. plot.
#' 
#' @details The sum of squared nearest neighbour distances normalised by the number of sample points and the number of individuals in the structural unit. Defined by the equation: \deqn{\pi \frac{n}{A} \frac{1}{k} \sum_{1}^{k} r_{i}^{2}} where \eqn{n} is the number of individuals in the structural unit, \eqn{A} is the structural unit area, \eqn{k} is the number of sample points, and \eqn{r_{i}} is the nearest neighbour distance to individual \eqn{i}. 
#' 
#' As the sample points are randomly allocated within the bounds of 
#'     xmin,xmax,ymin,ymax, the mean of a number of runs of this function 
#'     could be used to further constrain the estimate of Pielou's index.
#' 
#' @references Pielou, E. C. (1959). The use of point to plant distances in 
#'     the study of the pattern of plan populations. Journal of Ecology. 
#'     Volume 47. Pages 607-613.
#' 
#' @importFrom sf st_as_sf 
#' @importFrom nngeo st_nn
#' 
#' @examples 
#' data(bicuar)
#' pielou(bicuar$x_grid, bicuar$y_grid, 0, 100, 0, 100, 4)
#' 
#' @export
#' 
pielou <- function(x, y, xmin, xmax, ymin, ymax, k) {
  dat_sf <- sf::st_as_sf(data.frame(x,y), coords = c("x", "y"))
  
  spt <- data.frame(x = runif(k,xmin,xmax), y = runif(k,ymin,ymax))

  spt_sf <- sf::st_as_sf(spt, coords = c("x", "y"))

  dists <- suppressMessages(
    nngeo::st_nn(spt_sf, dat_sf, k = 1, progress = FALSE, returnDist = TRUE))

  sqdistsum <- sum(unlist(dists$dist)^2)

  kfrac <- 1/k

  areafrac <- nrow(dat_sf) / ((xmax-xmin) * (ymax-ymin))

  out <- pi * kfrac * areafrac * sqdistsum

  return(out)
}
