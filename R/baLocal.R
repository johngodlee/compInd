#' Total basal area of competitor trees - Steneker and Jarvis 1963
#'
#' @param ba vector of basal area of competitor trees
#'
#' @return value of competition index for focal tree
#'
#' @details Included mainly for posterity, simply the sum of basal areas of all 
#' competitor trees.
#' 
#' @references Steneker, G.A.; Jarvis, J.M. (1963). A preliminary study to 
#' assess competition in a white spruce-trembling aspen stand. Forestry 
#' Chronicle. Volume 39. Issue 3. Pages 334-336.
#' 
#' @export
#' 
baLocal <- function(ba) {
	sum(ba)
}

