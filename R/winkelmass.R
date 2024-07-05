#' Calculate the Winkelmass (spatial regularity of individuals)
#'
#' @param x two column matrix of individual x and y coordinates
#' @param k number of neighbours to consider
#'
#' @return value of the competition index for each individual in the structural unit, i.e. plot.
#' 
#' @references von Gadow, K., Hui, G. Y. (2001). Characterising forest spatial 
#' structure and diversity. Sustainable Forestry in Temperate Regions. Proc. of 
#' an international workshop organized at the University of Lund, Sweden. 
#' Pages 20-30.
#' 
#' @details literally in German, the 'angle measure', describes the degree of spatial regularity of individuals surrounding a given focal individual. The angle between each sequential neighbour with reference to the focal tree is calculated. As per the equation: \deqn{\frac{1}{k} \sum_{j=1}^{k} v_{j}} where \eqn{v_{j}} is \eqn{1} if the angle (\eqn{\alpha}) between neighbours is less than the critical angle, i.e. \eqn{\alpha \le \frac{360}{k}}, or \eqn{0} otherwise. As per von Gadow and Hui (2001) this function could be adapted to calculate regularity as a point attribute rather than an individual attribute. 
#' 
#' As per von Gadow and Hui (2001) this function could be adapted to calculate spatial mingling as a point attribute rather than an individual attribute. 
#' 
#' Normally expressed as the mean of values per structural unit to scale up.
#' 
#' @examples 
#' data(bicuar)
#' winkelmass(bicuar[,c("x", "y")], 4)
#' 
#' @export
#' 
winkelmass <- function(x, k = 4) {

  x <- as.matrix(x)

  nb <- nearNeighb(x, k = k)

  a0 <- 360 / k

  wi <- unlist(lapply(nb, function(i) {
    focal_i <- x[i$focal[1],]
    nb_i <- x[i$nb,]
    nb_angles <- sort(unlist(apply(nb_i, 1, function(j) {
      angleCalc(focal_i, j)
    })))
  
    aj <- nb_angles - c(NA, head(nb_angles, -1))
    aj[1] <- nb_angles[k] - nb_angles[1]
    aj <- ifelse(aj > 180, 360 - aj, aj)
    aj <- round(aj, 1)
    sum(aj < a0)
  }))

  out <- 1 / k * wi

  return(out)
}

