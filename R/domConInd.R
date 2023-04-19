#' Dominance concentration index (Su et al. 2020)
#'
#' @param ba vector of basal area measurements of competitor trees
#' @param sp vector of individual species names
#'
#' @return value of dominance concentration index for focal tree
#' 
#' @examples
#' data(bicuar)
#' nb <- nearNeighb(bicuar$x, bicuar$y, bicuar$stem_id, k = 10) 
#' lapply(nb, function(x) {
#'   nb <- merge(x, bicuar, by.x = "nb", by.y = "stem_id")
#'   nb$ba <- basalArea(nb$diam)
#'   domConInd(nb$ba, nb$sp)
#'   })
#' 
#' @references Su S., Guan B. T., Chang-Yang C., Sun I., Wang H., Hsieh C. 
#'     (2020). Multi-stemming and size enhance survival of dominant tree 
#'     species in a frequently typhoon-disturbed forest. Journal of Vegetation
#'     Science 31(3), pp. 429-439. DOI: 10.1111/jvs.12858
#' 
#' @export
#' 
domConInd <- function(ba, sp) { 
  # Split basal area by species
  ba_split <- split(ba, sp)

  # For each species, calculate b_{i}
  bi_list <- lapply(ba_split, function(x) {
    sum(x) / sum(unlist(ba_split))
  })

  # Calculate DC
  sum(unlist(bi_list)^2)
}
