#' DBH dominance - Aguirre et al. 2003
#'
#' @param dbh vector of dbh (diameter at breast height) measurements of 
#'     competitor trees
#' @param focal_dbh dbh of focal tree
#'
#' @return value of competition index for focal tree
#' 
#' @details Gives the proportion of the n nearest neighbours which are smaller 
#' than the focal tree. 
#'
#' @references Aguirre, O., Hui, G., von Gadow, K., Jimenez, J. (2003). An 
#' analysis of spatial forest structure using neighbourhood-based variables. 
#' Forest Ecology and Management. Volume 183. Pages 13.
#' 
#' @examples 
#' data(bicuar)
#' nb <- nearNeighb(bicuar$x, bicuar$y, bicuar$stem_id, k = 4) 
#' lapply(nb, function(x) {
#'   nb <- merge(x, bicuar, by.x = "nb", by.y = "stem_id")
#'   focal_diam <- unique(bicuar[bicuar$stem_id == nb$focal,"diam"])
#'   dbhDom(nb$diam, focal_diam)
#'   })
#' 
#' @export
#' 
dbhDom <- function(dbh, focal_dbh) {
	n <- length(dbh)
	dom <- sum(ifelse(dbh > focal_dbh, 1, 0))
	1 / n * dom
}

