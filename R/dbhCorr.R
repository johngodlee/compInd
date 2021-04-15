#' Diameter correlation index - Davies 2008
#'
#' @param dbh vector of DBH (diameter at breast height) measurements of competitor trees
#' @param focal_dbh DBH of focal tree
#'
#' @return atomic vector of competition index for focal tree
#'
#' @details All else being equal, the value of \code{diamCorr()} increases as 
#' the reference tree size increases. For a given reference tree size, the 
#' value increases as average neighbour size increases. Given by the equation:
#' \deqn{DCI_{i} = \frac{dbh_{i} \sum_{j=1}^{n} dbh_{j}}{n \overbar{dbh}^2}}
#' 
#' @references Davis, O., Pommerening, A. (2008). The contribution of 
#' structural indices to the modelling of Sitka spruce (Picea sitchensis) and 
#' birch (Betula spp.) crowns. Forest Ecology and Management. Volume 256. 
#' Pages 68-77.
#' 
#' @export
#' 
dbhCorr <- function(dbh, focal_dbh) {
  eq1 <- sum(dbh)
  eq2 <- length(dbh) * mean(dbh)^2
  eq3 <- focal_dbh * eq1
  eq3 / eq2
}


