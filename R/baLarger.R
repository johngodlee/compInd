#' Basal area of larger trees - Wykoff et al. 1982
#'
#' @param ba vector of basal area of competitor trees
#' @param focal_size size of subject tree
#' @param size vector of sizes of competitor trees
#'
#' @return value of competition index for focal tree
#' 
#' @details Returns the sum of basal areas of all trees larger than the focal 
#' tree. In Wykoff et al. (1982), \code{size} is basal area, though other 
#' measures of size could be used such as tree height (e.g. Flake et al. 2022).
#'
#' @references Wykoff, W. R., Crookston, N. L., Stage, A. R. (1982). User's 
#' guide to the stand prognosis model. United States Department of Agriculture 
#' Forest Service. Ogden UT, USA.
#' 
#' @export
#' 
# Basal area of larger trees - Wykoff et al. 1982
baLarger <- function(ba, focal_size, size) {
	sum(ba[size > focal_size])
}

