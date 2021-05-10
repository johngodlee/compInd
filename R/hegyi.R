#' Hegyi index - Hegyi 1974
#'
#' @param dbh vector of DBH (diameter at breast height) measurements of 
#'     competitor trees
#' @param dist vector of distances from focal tree to competitor trees
#' @param focal_dbh DBH of focal tree
#'
#' @return value of competition index for focal tree
#' 
#' @details A spatially explicit competition index which takes into account DBH
#' and distance of competitor trees. The iterative Hegyi index is a variant 
#' which picks competitors based on minimum distance of neighbouring trees 
#' within arc zones around the focal tree.
#' 
#' @references Hegyi, F., 1974. A simulation model for managing jack-pine 
#' stands. In: Fries, J. (Ed.), Growth Models for Tree and Stand Simulation. 
#' Royal College of Forestry, Stockholm, pages. 74–90.
#' 
#' @examples
#' data(bicuar)
#' nb <- nearNeighb(bicuar$x, bicuar$y, bicuar$stem_id, k = 4) 
#' lapply(nb, function(x) {
#'   nb <- merge(x, bicuar, by.x = "nb", by.y = "stem_id")
#'   focal_diam <- unique(bicuar[bicuar$stem_id == nb$focal,"diam"])
#'   hegyi(nb$diam, nb$nb_dist, focal_diam)
#'   })
#' 
#' @export
#'
hegyi <- function(dbh, dist, focal_dbh) {
	sum((dbh / focal_dbh) / dist)
}

