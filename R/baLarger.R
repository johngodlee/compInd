#' Basal area of larger trees - Wykoff et al. 1982
#'
#' @param ba vector of basal area of competitor trees
#' @param focal_ba value basal area of subject tree
#'
#' @return value of competition index for focal tree
#' 
#' @details Returns the sum of basal areas of all trees with basal area larger 
#' than the focal tree.
#'
#' @references Wykoff, W. R., Crookston, N. L., Stage, A. R. (1982). User's 
#' guide to the stand prognosis model. United States Department of Agriculture 
#' Forest Service. Ogden UT, USA.
#' 
#' @export
#' 
# Basal area of larger trees - Wykoff et al. 1982
baLarger <- function(ba, focal_ba) {
	sum(ba[ba > focal_ba])
}

