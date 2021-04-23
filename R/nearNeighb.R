#' Find nearest neighbours within a radius
#'
#' @param x vector of individual x axis coordinates
#' @param y vector of individual y axis coordinates
#' @param id vector of individual IDs. If NULL, vector indices are used.
#' @param radius radius to look for nearest neighbours, in units of XY coordinates
#' @param zones number of zones of equal arc angle, e.g. zones == 4 results in 
#'     four zones each with 90deg arc. If NULL, no zones are defined. If zones
#'     are defined, the nearest competitor within each zone is returned.
#'
#' @return list of dataframes per focal tree, of neighbours, their distances 
#'     and angles. If no competitors are found within the radius of a focal 
#'     tree, NA is returned for all columns except focal ID.
#' 
#' @importFrom sf st_as_sf
#' 
#' @export
#' 
nearNeighb <- function(x, y, id = NULL, radius, zones = NULL) {
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

  # Find neighbours within radius
  nb <- lapply(seq_len(nrow(dist_mat)), function(z) {
    # Get focal tree 
    focal <- row.names(dist_mat)[z]

    # Convert focal tree to sfg geometry
    focal_sfg <- sf::st_geometry(dat_sf[dat_sf$id == focal,])[[1]]

    # Get IDs of neighbours
    ids <- colnames(dist_mat)[dist_mat[z,] <= radius]
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

