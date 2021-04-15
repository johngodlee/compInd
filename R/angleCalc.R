#' Calculate angle between two sf point objects
#'
#' @param x point feature of class 'sf'
#' @param y point feature of class 'sf'
#'
#' @return azimuthal from x to y, in degrees
#' 
#' @examples
#' p1 <- st_point(c(0,1))
#' p2 <- st_point(c(1,2))
#' angleCalc(p1, p2)
#' 
#' @export
#' 
angleCalc <- function(x, y) {
  dst_diff <- as.numeric(x - y)
  return((atan2(dst_diff[1], dst_diff[2]) + pi) / 0.01745329)
}

