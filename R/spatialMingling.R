#' von Gadow's spatial mingling index
#'
#' @param x vector of individual x axis coordinates
#' @param y vector of individual y axis coordinates
#' @param sp vector of individual species names
#' @param k number of neighbours to consider
#' @param adj logical, if TRUE the basic spatial mingling index is multiplied 
#'     by Si/nmax, where Si is the number of species in the neighbourhood of 
#'     the focal tree, and nmax is the total number of species in the data.
#'
#' @references von Gadow, K., Hui, G. Y. (2001). Characterising forest spatial 
#' structure and diversity. Sustainable Forestry in Temperate Regions. Proc. of 
#' an international workshop organized at the University of Lund, Sweden. 
#' Pages 20- 30.
#' 
#' @return 
#' 
#' @export
#' 
spatialMingling <- function(x, y, sp, k = 4, adj = FALSE) {

  dat_sf <- sf::st_as_sf(data.frame(x,y,sp), coords = c("x", "y"))

  dists <- nngeo::st_nn(dat_sf, dat_sf, k = k+1)

  mi <- unlist(lapply(dists, function(i) {
    1/k * sum(sp[i[1]] != sp[i[-1]])
  }))

  if (adj) {
    si <- unlist(lapply(dists, function(i) {
      length(unique(sp[i[-1]]))
    }))
    nmax <- length(unique(sp))

    out <- mi * (si/nmax)
  } else {
    out <- mi
  }

  return(out)
}

