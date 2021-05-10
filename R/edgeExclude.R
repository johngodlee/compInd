#' Find individuals in the "edge" of a plot
#'
#' @param shape \code{sf POLYGON} object defining plot area
#' @param buffer size of buffer zone inside plot, same scale as coordinates of 
#'     \code{shape}
#' @param x vector of individual x axis coordinates in the same coordinates as 
#'     \code{shape}
#' @param y vector of individual y axis coordinates in the same coordinates as 
#'     \code{shape}
#' @param id vector of individual IDs. If \code{NULL}, vector positions are used.
#'
#' @details Generally, the buffer size should be the same size as the 
#'     competition zone radius around each tree, to ensure that the value of a 
#'     given competition index isn't under-estimated for trees near the plot 
#'     edge, due to a lack of data collected outside the plot. For example, if 
#'     the competition radius set by \code{nearNeighb} is 5 m, the buffer 
#'     should also be 5 m. Trees within the buffer zone are generally excluded 
#'     as focal trees in competition indices, but may still be used as 
#'     competitor trees by other focal trees not inside the buffer zone.
#'
#' @return vector of \code{id} values from individuals which fall inside the buffer zone of the plot. 
#' 
#' @importFrom sf st_as_sf st_sf st_sfc st_buffer st_join st_intersects st_drop_geometry
#'
#' @examples
#' data(bicuar)
#' shape <- sf::st_polygon(list(
#'     cbind(c(0,0,120,120,0), c(0,120,120,0,0))))
#' edgeExclude(shape, 5, bicuar$x, bicuar$y, bicuar$stem_id)
#'
#' @export
#' 
edgeExclude <- function(shape, buffer, x, y, id = NULL) {
  # Check parameters defined properly
  if (length(x) != length(y)) {
    stop("Unequal coordinate vector lengths")
  }

  if (buffer < 0 | !is.numeric(buffer) | length(buffer) != 1) {
    stop("Buffer must be a single positive number")
  }

  # Add IDs if missing
  if (is.null(id)) {
    id <- seq_along(x)
  }

  # Are IDs unique?
  if (any(duplicated(id))) {
    stop("ID values are not unique")
  }

  # Convert coordinates to sf object
  pts <- sf::st_as_sf(data.frame(x, y, id), coords = c("x", "y"))

  # Are all points inside polygon?
  if ( nrow(sf::st_join(sf::st_sf(sf::st_sfc(shape)), pts, sf::st_intersects)) != nrow(pts) ) {
    stop("Some points not in polygon")
  }

  # Make buffered polygon
  shape_buf <- sf::st_buffer(shape, -buffer, endCapStyle = "FLAT")

  # Find IDs inside buffer
  outside <- unname(unlist(sf::st_drop_geometry(pts[!lengths(sf::st_intersects(pts, shape_buf)), "id"])))

  # Return IDs
  return(outside)
}
