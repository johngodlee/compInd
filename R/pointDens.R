#' Point density - Spurr (1962)
#' 
#' @param dbh vector of DBH (diameter at breast height) measurements of 
#'     competitor trees
#' @param dist vector of distances from focal tree to competitor trees
#'
#' @references Spurr, S. H. (1962). A measure of point density. Forest Science. 
#' Volume 8. Issue 1. Pages 85–96.
#' 
#' @details Calculates point density in units of \code{dist}, with the equation: \deqn{\sum_{k=i}^{n} (0.25 (k - 0.5) (D_{k} / L_{k})^2 ) / k} where \eqn{k} is the rank of the \eqn{k}th competitor by dbh value, \eqn{D_{k}} is the dbh of the \eqn{k}th competitor, and \eqn{L_{k}} is the distance of the \eqn{k}th competitor to the focal tree \eqn{i}.
#'
#' @return value of competition index for focal tree
#' 
#' @examples
#' data(bicuar)
#' nb <- nearNeighb(bicuar$x, bicuar$y, bicuar$stem_id, k = 4) 
#' lapply(nb, function(x) {
#'   nb <- merge(x, bicuar, by.x = "nb", by.y = "stem_id")
#'   pointDens(nb$diam, nb$nb_dist)
#'   })
#' 
#' @export
#'
pointDens <- function(dbh, dist) {
  eq1 <- (dbh / dist)^2
  ranks <- rank(dbh)
  eq2 <- (0.25 * (ranks - 0.5) * eq1) / ranks
  sum(eq2)
}

