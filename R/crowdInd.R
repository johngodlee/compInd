#' Forest matrix crowding index - Seydack et al. 2011
#'
#' @param dbh vector of DBH (diameter at breast height) measurements of competitor trees
#' @param dbh_min minimum DBH threshold considered
#' @param dbh_max maximum DBH threshold considered
#'
#' @return atomic vector of competition index for focal tree
#' 
#' @details A simple competition index which returns the sum of DBH values of 
#'     all competitor trees within user-defined DBH thresholds. Armin et al. 
#'     (2011) use a minimum threshold of 10 cm DBH and a maximum threshold of 
#'     30 cm DBH.
#' 
#' @references Armin H.W. Seydack, Graham Durrheim, Josua H. Louw. 
#'     Spatiotemporally interactive growth dynamics in selected South African 
#'     forests: Edaphoclimatic environment, crowding and climate effects. 
#'     Forest Ecology and Management. Volume 261. Issue 7. 2011. 
#'     Pages 1152-1169.
#' 
#' @export
#'
crowdInd <- function(dbh, dbh_min = 10, dbh_max = 30) {
	sum(dbh[dbh > dbh_min & dbh < dbh_max])
}

