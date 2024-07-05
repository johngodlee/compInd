#' Find nearest neighbours within a radius
#'
#' @param x two column matrix of individual x and y coordinates
#' @param y optional two column matrix of individual x and y coordinates
#' @param k number of neighbours to search for, starting from nearest in 
#'     coordinate space. If NULL, \code{radius} must be provided.
#' @param radius radius to look for nearest neighbours, in units of XY 
#'     coordinates. If \code{NULL}, \code{k} must be provided.
#' @param zones number of zones of equal arc angle, e.g. \code{zones = 4} results in 
#'     four zones each with 90deg arc. If \code{NULL}, no zones are defined. If zones
#'     are defined, the nearest competitor within each zone is returned. If 
#'     zones are defined, \code{radius} must also be defined.
#' 
#' @details If \code{y} is provided, nearest neighbours of individuals in 
#' \code{y} are identified for each individual in \code{x}, otherwise, 
#' nearest neighbours in \code{x} are identified.
#' 
#' @details In the case of ties, the first nearest neighbour is returned.
#'
#' @return List of dataframes per focal individual in x, of neighbours, 
#' their distances and angles relative to the focal individual. If no 
#' competitors are found within the radius of a focal individual, 
#' NA is returned for all columns except focal ID.
#' 
#' @examples
#' data(bicuar)
#' 
#' nearNeighb(bicuar[,c("x", "y")], k = 4)
#' nearNeighb(bicuar[1:10, c("x", "y")], bicuar[, c("x", "y")], radius = 5)
#' nearNeighb(bicuar[,c("x", "y")], radius = 5, zones = 4)
#' 
#' @export
#' 
nearNeighb <- function(x, y = NULL, k = NULL, radius = NULL, zones = NULL) {
  # If y not provided make y = x  
  if (is.null(y)) { 
    y2 <- x
  } else {
    y2 <- y
  }

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
    if (k >= nrow(y2)) {
      stop("k is larger than the number of individuals in y")
    }
  }

  # Coerce x and y to matrices
  x <- as.matrix(x)
  y2 <- as.matrix(y2)
  rownames(y2) <- seq_len(nrow(y2))

  # Identify neighbours in y for each element of x (z)
  nb_all <- lapply(seq_len(nrow(x)), function(z) {
    # Get focal individual 
    focal <- x[z,]

    # If y was not supplied drop focal from neighbours
    if (is.null(y)) { 
      yz <- y2[-z,]
    }

    # Calculate distance between focal individual and each element of y
    ydist <- unname(unlist(lapply(seq_len(nrow(yz)), function(i) {
      sqrt((focal[1] - yz[i, 1])^2 + (focal[2] - yz[i, 2])^2)
    })))
    names(ydist) <- rownames(yz)

    if (!is.null(radius)) {
      # Get neighbours within radius
      nb <- names(ydist[ydist <= radius])
    } else if (!is.null(k)) {
      # Get nearest k neighbours
      nb <- as.numeric(names(sort(ydist)[1:k]))
    }

    # Get distances
    nb_dist <- ydist[names(ydist) %in% nb]

    # Create dataframe
    if (length(nb) > 0) {
      out_df <- data.frame(focal = z, nb = nb, nb_dist = nb_dist)

      # If zones
      if (!is.null(zones)) {
        # Add angle
        out_df$nb_angle <- unlist(lapply(nb, function(i) {
            angleCalc(focal, yz[i,])
          })
        )

        # Find zones for each neighbour
        zone_vec <- c(0, seq(360 / zones, 360, length.out = zones))
        out_df$nb_zone <- cut(out_df$nb_angle, breaks = zone_vec)

        # Get nearest neighbour in each zone
        out_df <- do.call(rbind, by(out_df, out_df$nb_zone, function(i) {
          i[which.min(i$nb_dist), ] 
        }))
      }
    } else {
      out_df <- data.frame(focal = z, nb = NA_real_, nb_dist = NA_real_)

      # If zones
      if (!is.null(zones)) { 
        # Add missing angle
        out_df$nb_angle <- NA_real_
        out_df$nb_zone <- NA_character_
      }
    }

    row.names(out_df) <- NULL
    out_df 
  })

  # Return
  return(nb_all)
}

