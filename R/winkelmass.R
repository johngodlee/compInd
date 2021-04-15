#' Calculate the Winkelmass (spatial regularity of trees)
#'
#' @param x vector of individual x axis coordinates
#' @param y vector of individual y axis coordinates
#' @param k number of neighbours to consider
#'
#' @return 
#' 
#' @references von Gadow, K., Hui, G. Y. (2001). Characterising forest spatial 
#' structure and diversity. Sustainable Forestry in Temperate Regions. Proc. of 
#' an international workshop organized at the University of Lund, Sweden. 
#' Pages 20- 30.
#' 
#' @export
#' 
winkelmass <- function(x, y, k = 4) {

  dat_sf <- sf::st_as_sf(data.frame(x,y), coords = c("x", "y"))

  dists <- nngeo::st_nn(dat_sf, dat_sf, k = k+1)

  a0 <- 360 / k

  wi <- unlist(lapply(dists, function(i) {
    focal_sfg <- st_geometry(dat_sf[i[1],])[[1]]
    nb_sfg <- st_geometry(dat_sf[i[-1],])
    nb_angles <- sort(unlist(lapply(nb_sfg, function(j) {
      angleCalc(focal_sfg, j)
    })))
    aj <- nb_angles - lag(nb_angles)
    aj[1] <- nb_angles[k] - nb_angles[1]
    aj <- ifelse(aj > 180, 360 - aj, aj)
    sum(aj > a0)
  }))

  out <- 1 / k * wi

  return(out)
}


