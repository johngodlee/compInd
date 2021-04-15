#' DBH dominance - Aguirre et al. 2003
#'
#' @param dbh vector of dbh (diameter at breast height) measurements of 
#'     competitor trees
#' @param focal_dbh dbh of focal tree
#'
#' @return atomic vector of competition index for focal tree
#' 
#' @details Gives the proportion of the n nearest neighbours which are smaller 
#' than the focal tree. 
#'
#' @references Aguirre, O., Hui, G., von Gadow, K., Jimenez, J. (2003). An 
#' analysis of spatial forest structure using neighbourhood-based variables. 
#' Forest Ecology and Management. Volume 183. Pages 13.
#' 
#' @export
#' 
dbhDom <- function(dbh, focal_dbh) {
	n <- length(dbh)
	dom <- sum(ifelse(dbh > focal_dbh, 1, 0))
	1 / n * dom
}

