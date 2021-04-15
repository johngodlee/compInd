#' Alemdag's (1978) tree competition index
#' 
#' @param dbh vector of DBH (diameter at breast height) measurements of competitor trees
#' @param dist vector of distances from focal tree to competitor trees
#' @param focal_dbh DBH of focal tree
#'
#' @return atomic vector of competition index for focal tree
#'
#' @details A spatially explicit competition index originally used in white 
#' spruce plantations. The value of this function increases when competitor 
#' trees are closer to the focal tree, or when competitor trees are larger. 
#' Alemdag's competition index is defined by the following equation:
#' \deqn{\sum{n}{j\neq{}i}(\pi[(l_{ij}d_{i})/(d_{i} + d_{j})]^2 (d_{j} / l_{ij}) / \sum(d_{j} / l_{ij}))}
#' where \eqn{l_{ij}} is the distance between focal tree \eqn{i} and tree 
#' \eqn{j}, \eqn{d_{i}} is the diameter (DBH) of focal tree \eqn{i}.
#' 
#' @references Alemdag I. S. (1978). Evaluation of some competition indexes for 
#' the prediction of diameter increment in planted white spruce. Forest 
#' Management Institute, Ottawa, Canada.
#' 
#' @export
#'
alemdag <- function(dbh, dist, focal_dbh) {
	n <- length(dbh)
	eq1 <- dist * (dbh / (dbh + focal_dbh))
	eq2 <- dbh / dist
	
	n / sum(pi * eq1^2 * (eq2 / sum(eq2)))
}

