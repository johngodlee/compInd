#' Calculate angle between two sf point objects
#'
#' @param x numeric vector with two elements, X and Y coordinates of a point
#' @param y numeric vector with two elements, X and Y coordinates of a point
#'
#' @return azimuthal from x to y, in degrees
#'
#' @examples
#' p1 <- c(0, 1)
#' p2 <- c(1, 2)
#' angleCalc(p1, p2)
#'
#' @export
#'
angleCalc <- function(x, y) {
  dst <- x - y
  angle <- (atan2(dst[1], dst[2]) + pi) / ((1 / 180) * pi)
  return(angle)
}
