#' Find individuals inside a buffer zone away from the plot edge
#'
#' @param x two column matrix of individual x and y coordinates
#' @param buffer size of buffer zone inside plot, same scale as coordinates 
#'     \code{xmin}, \code{xmax}, \code{ymin}, \code{ymax}
#' @param xmin minimum x coordinate in plot
#' @param xmax maximum x coordinate in plot
#' @param ymin minimum y coordinate in plot
#' @param ymax maximum y coordinate in plot
#'
#' @details Generally, the buffer size should be the same size as the expected
#'     competition zone radius around each tree, to ensure that the value of a 
#'     given competition index isn't under-estimated for trees near the plot 
#'     edge, due to a lack of data collected outside the plot. For example, if 
#'     the competition radius set by \code{nearNeighb} is 5 m, the buffer 
#'     should also be 5 m. Trees within the buffer zone are generally excluded 
#'     as focal trees in competition indices, but may still be used as 
#'     competitor trees by other focal trees not inside the buffer zone.
#'
#' @return vector of rows from \code{x} which are not within the buffer zone.
#' 
#' @examples
#' data(bicuar)
#' edgeExclude(bicuar[,c("x", "y")], 5, 0, 100, 0, 100)
#'
#' @export
#' 
edgeExclude <- function(x, buffer, xmin, xmax, ymin, ymax) {
  # Check input
  if (buffer < 0 | !is.numeric(buffer) | length(buffer) != 1) {
    stop("Buffer must be a single positive number")
  }

  if (xmax <= xmin | ymax <= ymin) {
    stop("[x|y]max small than [x|y]min")
  }

  if (buffer > (xmax - xmin) | buffer > (ymax - ymin)) {
    stop("Buffer larger than plot")
  }

  # Coerce x to matrix
  x <- as.matrix(x)
  rownames(x) <- seq_len(nrow(x))

  # Are all points inside plot?
  if (any(x[,1] < xmin | x[,1] > xmax | x[,2] < ymin | x[,2] > ymin)) {
    warning("Some points not within plot, these will be excluded.")
  }

  # Find individuals well inside the plot and not in the buffer
  out <- which(
    x[,1] < xmax - buffer &
      x[,1] > xmin + buffer & 
      x[,2] < ymax - buffer &
      x[,2] > ymin + buffer)

  # Return IDs
  return(out)
}
