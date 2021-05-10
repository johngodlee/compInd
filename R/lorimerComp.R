#' Lorimer's competition index - Lorimer 1983
#'
#' @param dbh vector of DBH (diameter at breast height) measurements of competitor trees
#' @param dist vector of distances from focal tree to competitor trees
#' @param focal_dbh DBH of focal tree
#' @param czr Competition zone radius (CZR), normally based on plot-level stem density
#'
#' @return value of competition index for focal tree
#' 
#' @details Value increases with dbh of competitor trees, decreases as the 
#' focal tree DBH increases. Value increases as the distance of competitor 
#' trees decreases. Tree distances are divided by the CZR in order to account 
#' for "stand age".
#' 
#' @references Lorimer, C. G. (1983). Tests of age-independent competition 
#' indices for individual trees in natural hardwood stands. Forest Ecology and 
#' Management. Volume 6. Pages 343-360.
#'
#' @seealso [lorimerCZR()] to calculate the CZR
#' 
#' @examples 
#' data(bicuar)
#' czr <- lorimerCZR(k = 1, n = nrow(bicuar))
#' nb <- nearNeighb(bicuar$x, bicuar$y, bicuar$stem_id, radius = czr) 
#' lapply(nb, function(x) {
#'   nb <- merge(x, bicuar, by.x = "nb", by.y = "stem_id", all.x = TRUE)
#'   focal_diam <- unique(bicuar[bicuar$stem_id == unique(nb$focal),"diam"])
#'   lorimerComp(nb$diam, nb$nb_dist, focal_diam, czr)
#'   })
#' 
#' @export
#' 
lorimerComp <- function(dbh, dist, focal_dbh, czr) {
  eq1 <- (dbh / focal_dbh) / sqrt(dist / czr)
  sum(eq1)
}
