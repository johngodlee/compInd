#' von Gadow's spatial mingling index
#'
#' @param x two column matrix of individual x and y coordinates
#' @param sp vector of individual species names
#' @param k number of neighbours to consider
#' @param adj logical, if \code{TRUE} the basic spatial mingling index is multiplied
#'     by \eqn{\frac{S_{i}}{n_{max}}}, where \eqn{S_{i}} is the number of species in the neighbourhood of
#'     the focal individual, and \eqn{n_{max}} is the maximum number of species possible in the
#'     neighbourhood, including the focal individual, i.e. \eqn{k + 1}.
#'
#' @return value of the spatial mingling index for each individual in the structural unit.
#'
#' @details Describes the degree of variety in species in the vicinity of a given focal individual. The proportion of the \eqn{k} nearest neighbours not belonging to the same species as the focal individual, given by the equation: \deqn{\frac{1}{k} \sum_{j=1}^{k} v_{j}} where \eqn{v_{j}} is the status of the competitor individual \eqn{j}, either \eqn{0} if \eqn{j} belongs to the same species as the focal individual, or \eqn{1} if \eqn{j} belongs to a different species. Values of spatial mingling for a given individual therefore vary between 0 and 1.
#'
#' As per von Gadow and Hui (2001) this function could be adapted to calculate spatial mingling as a point attribute rather than an individual attribute.
#'
#' Normally expressed as the mean of values per structural unit to scale up.
#'
#' @references von Gadow, K., Hui, G. Y. (2001). Characterising forest spatial
#' structure and diversity. Sustainable Forestry in Temperate Regions. Proc. of
#' an international workshop organized at the University of Lund, Sweden.
#' Pages 20-30.
#'
#' @examples
#' data(bicuar)
#' spatialMingling(bicuar[, c("x", "y")], bicuar$species,
#'   k = 4, adj = FALSE
#' )
#' spatialMingling(bicuar[, c("x", "y")], bicuar$species,
#'   k = 4, adj = TRUE
#' )
#'
#' @export
#'
spatialMingling <- function(x, sp, k = 4, adj = FALSE) {
  nb <- nearNeighb(x, k = k)

  mi <- unlist(lapply(nb, function(i) {
    1 / k * sum(sp[i$focal[1]] != sp[i$nb])
  }))

  if (adj) {
    si <- unlist(lapply(nb, function(i) {
      length(unique(sp[i$nb]))
    }))
    nmax <- k + 1
    out <- mi * (si / nmax)
  } else {
    out <- mi
  }

  return(out)
}
