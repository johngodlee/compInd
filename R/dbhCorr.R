#' Diameter correlation index - Davies 2008
#'
#' @param dbh vector of DBH (diameter at breast height) measurements of competitor trees
#' @param focal_dbh DBH of focal tree
#'
#' @return value of competition index for focal tree
#'
#' @details All else being equal, the value of \code{diamCorr()} increases as 
#' the focal tree size increases. For a given focal tree size, the 
#' value increases as average neighbour size increases. Given by the equation:
#' \deqn{DCI_{i} = \frac{dbh_{i} \sum_{j=1}^{n} dbh_{j}}{n \overline{dbh}^2}}
#' 
#' @references Davis, O., Pommerening, A. (2008). The contribution of 
#' structural indices to the modelling of Sitka spruce (Picea sitchensis) and 
#' birch (Betula spp.) crowns. Forest Ecology and Management. Volume 256. 
#' Pages 68-77.
#' 
#' @examples
#' data(bicuar)
#' nb <- nearNeighb(bicuar$x, bicuar$y, bicuar$stem_id, k = 4) 
#' lapply(nb, function(x) {
#'   nb <- merge(x, bicuar, by.x = "nb", by.y = "stem_id")
#'   focal_diam <- unique(bicuar[bicuar$stem_id == nb$focal,"diam"])
#'   dbhCorr(nb$diam, focal_diam)
#'   })
#' 
#' @export
#' 
dbhCorr <- function(dbh, focal_dbh) {
  eq1 <- sum(dbh)
  eq2 <- length(dbh) * mean(dbh)^2
  eq3 <- focal_dbh * eq1
  eq3 / eq2
}


