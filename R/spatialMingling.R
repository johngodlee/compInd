#' von Gadow's spatial mingling index
#'
#' @param x vector of individual x axis coordinates
#' @param y vector of individual y axis coordinates
#' @param sp vector of individual species names
#' @param k number of neighbours to consider
#' @param adj logical, if TRUE the basic spatial mingling index is multiplied 
#'     by Si/nmax, where Si is the number of species in the neighbourhood of 
#'     the focal tree, and nmax is the maximum number of species possible in the
#'     neighbourhood, including the focal tree, i.e. k + 1.
#'
#' @return 
#' 
#' @references von Gadow, K., Hui, G. Y. (2001). Characterising forest spatial 
#' structure and diversity. Sustainable Forestry in Temperate Regions. Proc. of 
#' an international workshop organized at the University of Lund, Sweden. 
#' Pages 20-30.
#' 
#' @importFrom sf st_as_sf 
#' @importFrom nngeo st_nn
#' 
#' @export
#' 
spatialMingling <- function(x, y, sp, k = 4, adj = FALSE) {
  dat_sf <- sf::st_as_sf(data.frame(x,y,sp), coords = c("x", "y"))

  dists <- suppressMessages(
    nngeo::st_nn(dat_sf, dat_sf, k = k+1, progress = FALSE))

  mi <- unlist(lapply(dists, function(i) {
    1 / k * sum(sp[i[1]] != sp[i[-1]])
  }))

  if (adj) {
    si <- unlist(lapply(dists, function(i) {
      length(unique(sp[i[-1]]))
    }))
    nmax <- k + 1

    out <- mi * (si/nmax)
  } else {
    out <- mi
  }

  return(out)
}
