#' Calculate the Winkelmass (spatial regularity of individuals)
#'
#' @param x vector of individual x axis coordinates
#' @param y vector of individual y axis coordinates
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
#' @importFrom sf st_as_sf st_geometry
#' @importFrom nngeo st_nn 
#' 
#' @examples 
#' data(bicuar)
#' winkelmass(bicuar$x_grid, bicuar$y_grid, 4)
#' 
#' @export
#' 
winkelmass <- function(x, y, k = 4) {
  dat_sf <- sf::st_as_sf(data.frame(x,y), coords = c("x", "y"))

  dists <- suppressMessages(nngeo::st_nn(dat_sf, dat_sf, k = k+1, 
      progress = FALSE))

  a0 <- 360 / k

  wi <- unlist(lapply(dists, function(i) {
    focal_sfg <- sf::st_geometry(dat_sf[i[1],])[[1]]
    nb_sfg <- sf::st_geometry(dat_sf[i[-1],])
    nb_angles <- sort(unlist(lapply(nb_sfg, function(j) {
      angleCalc(focal_sfg, j)
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

