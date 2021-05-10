#' Find nearest neighbours within a radius
#'
#' @param x vector of individual x axis coordinates
#' @param y vector of individual y axis coordinates
#' @param id vector of individual IDs. If NULL, vector positions are used.
#' @param k number of neighbours to search for, starting from nearest in 
#'     coordinate space. If NULL, \code{radius} must be provided.
#' @param radius radius to look for nearest neighbours, in units of XY 
#'     coordinates. If \code{NULL}, \code{k} must be provided.
#' @param zones number of zones of equal arc angle, e.g. \code{zones = 4} results in 
#'     four zones each with 90deg arc. If \code{NULL}, no zones are defined. If zones
#'     are defined, the nearest competitor within each zone is returned. If 
#'     zones are defined, \code{radius} must also be defined.
#'
#' @return list of dataframes per focal tree, of neighbours, their distances 
#'     and angles relative to the focal tree. If no competitors are found within the radius of a focal 
#'     tree, NA is returned for all columns except focal ID.
#' 
#' @importFrom sf st_as_sf
#' 
#' @examples
#' data(bicuar)
#' nearNeighb(bicuar$x, bicuar$y, bicuar$stem_id, k = 4)
#' nearNeighb(bicuar$x, bicuar$y, bicuar$stem_id, radius = 5)
#' nearNeighb(bicuar$x, bicuar$y, bicuar$stem_id, radius = 5, zones = 4)
#' 
#' @export
#' 
nearNeighb <- function(x, y, id = NULL, k = NULL, radius = NULL, zones = NULL) {
  # Check parameters defined properly
  if (!is.null(zones) & is.null(radius)) {
    stop("If zones defined, radius must also be defined")
  }

  if (is.null(k) & is.null(radius)) {
    stop("If k is not defined, radius must be defined")
  }

  if (!is.null(k) & !is.null(radius)) { 
    stop("Either k or radius must be defined, not both")
  }

  if (!is.null(k)) {
    if (k >= length(x)) {
      stop("k is larger than the number of stems in the plot")
    }
  }

  if (length(x) != length(y)) {
    stop("Unequal coordinate vector lengths")
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
  dat_sf <- sf::st_as_sf(data.frame(x,y,id), coords = c("x", "y"))

  # Distance matrix
  dist_mat <- sf::st_distance(dat_sf)
  colnames(dist_mat) <- id
  rownames(dist_mat) <- id

  # Find neighbours 
  nb <- lapply(seq_len(nrow(dist_mat)), function(z) {
    # Get focal tree 
    focal <- row.names(dist_mat)[z]

    # Convert focal tree to sfg geometry
    focal_sfg <- sf::st_geometry(dat_sf[dat_sf$id == focal,])[[1]]

    # Get IDs of neighbours:

    if (!is.null(radius)) {
      # Within radius
      ids <- colnames(dist_mat)[dist_mat[z,] <= radius]
    } else if (!is.null(k)) {
      # Within k
      ids <- names(sort(dist_mat[z,])[seq_len(k + 1)])
    }

    ids <- ids[ids != focal]
    out <- dist_mat[z, c(ids)]

    # Create dataframe
    if (length(out) > 0) {
      out_df <- data.frame(focal, nb = ids, nb_dist = out)

      # Add angle
      out_df$nb_angle <- unlist(lapply(sf::st_geometry(dat_sf[dat_sf$id %in% ids,]), function(i) {
        angleCalc(focal_sfg, i)
      }))
    } else {
      out_df <- data.frame(focal, nb = NA_character_, nb_dist = NA_real_)
      out_df$nb_angle <- NA_real_
    }

    # If zones
    if (!is.null(zones)) {
      if (length(out) > 0) {
        # Find zones for each neighbour
        zone_vec <- c(0, seq(360 / zones, 360, length.out = zones))
        out_df$nb_zone <- cut(out_df$nb_angle, breaks = zone_vec)

        # Get nearest neighbour in each zone
        out_df <- do.call(rbind, by(out_df, out_df$nb_zone, function(i) {
          i[which.min(i$nb_dist), ] 
        }))
      } else {
        out_df$nb_zone <- NA
      }
    }
    row.names(out_df) <- NULL
    out_df 
  })

  names(nb) <- rownames(dist_mat)

  return(nb)
}

