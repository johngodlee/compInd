#' DBH differentiation - Pommerening 2002
#'
#' @param dbh vector of dbh (diameter at breast height) measurements of competitor trees
#' @param focal_dbh dbh of focal tree
#' 
#' @return atomic vector of competition index for focal tree
#' 
#' @details Gives the size difference of neighbouring trees and describes the 
#' spatial distribution of tree sizes. The value increases with increasing 
#' average size difference between neighbuoring trees. \code{dbhDiff == 0} 
#' when all neighbours have equal size.
#' 
#' @references Pommerening, A. (2002). Approaches to quantifying forest 
#' structures. Forestry, Volume 75, Issue 3. Pages 305-324.
#' 
#' @export
dbhDiff <- function(dbh, focal_dbh) {
	n <- length(dbh)
	dom_min <- ifelse(dbh > focal_dbh, min(dbh), focal_dbh)
	dom_max <- ifelse(dbh > focal_dbh, max(dbh), focal_dbh)
	1 / n * (sum(1 - (dom_min / dom_max)))
}

