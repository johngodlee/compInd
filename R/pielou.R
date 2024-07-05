#' Pielou's index of non-randomness
#'
#' @param x two column matrix of individual x and y coordinates
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
#' @examples
#' data(bicuar)
#' pielou(bicuar[,c("x", "y")], 0, 100, 0, 100, 50)
#'
#' @export
#'
pielou <- function(x, xmin, xmax, ymin, ymax, k) {
 
  # Check input
  if (xmax <= xmin | ymax <= ymin) {
    stop("[x|y]max small than [x|y]min")
  }

  # Create dataframe of sample points
  spt <- data.frame(
    x = runif(k, xmin, xmax), 
    y = runif(k, ymin, ymax))

  # Calculate nearest neighbour to each sample point
  nb <- nearNeighb(spt, x, k = 1)

  # Calculate summed squared distances
  sqdistsum <- sum(unlist(lapply(nb, "[[", "nb_dist"))^2)

  kfrac <- 1 / k

  areafrac <- nrow(dat_sf) / ((xmax - xmin) * (ymax - ymin))

  out <- pi * kfrac * areafrac * sqdistsum

  return(out)
}
