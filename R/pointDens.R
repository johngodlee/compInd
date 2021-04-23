#' Point density - Purr 1962
#' 
#' @param dbh vector of DBH (diameter at breast height) measurements of 
#'     competitor trees
#' @param dist vector of distances from focal tree to competitor trees
#'
#' @return atomic vector of competition index for focal tree
#' 
#' @references Spurr, S. H. (1962). A measure of point density. Forest Science. 
#' Volume 8. Issue 1. Pages 85–96.
#' 
#' @export
#'
pointDens <- function(dbh, dist) {
  eq1 <- (dbh / dist)^2
  ranks <- rank(dbh)
  eq2 <- (0.25 * (ranks - 0.5) * eq1) / ranks
  sum(eq2)
}

