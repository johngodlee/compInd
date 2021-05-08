#' Martin and Ek 1984
#' 
#' @param dbh vector of DBH (diameter at breast height) measurements of 
#'     competitor trees
#' @param dist vector of distances from focal tree to competitor trees
#' @param focal_dbh DBH of focal tree
#'
#' @return atomic vector of competition index for focal tree
#' 
#' @details Gives the sum of ratio of competitor to focal tree DBHs, multiplied 
#' by the exponential of distances divided by competitor DBH plus focal DBH. 
#' Given by the equation:
#' \deqn{\sum_{j\neq{}i}^{n} (d_{j} / d_{i}) exp((16l_{ij}) / (d_{i} + d_{j}))}
#' 
#' @references Martin G. L., Ek A. R. (1984). A Comparison of Competition 
#' Measures and Growth Models for Predicting Plantation Red Pine Diameter and 
#' Height Growth. Forest Science. Volume 30. Pages 731-743.
#' 
#' @export
#'
martinEk <- function(dbh, dist, focal_dbh) {
	eq1 <- dbh / focal_dbh
	eq2 <- (16 * dist) / (dbh + focal_dbh)
	eq3 <- eq1 * exp(eq2)
	sum(eq3)
}


