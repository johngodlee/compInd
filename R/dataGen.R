#' Generate fake data from a rectangular tree plot
#'
#' @param nplots number of plots 
#' @param min_stems minimum number of stems per plot
#' @param max_stems maximum number of stems per plot
#' @param min_diam minimum stem diameter
#' @param max_diam maximum stem diameter
#' @param plot_width plot width
#' @param plot_length plot length
#' @param species vector of species names from which to sample
#'
#' @return dataframe, where each row is a tree stem. Default of five plots, 
#'     each with between 200 and 500 stems, from 20 species, with stem diameter 
#'     values between 5 and 100. All trees have a single stem. Diameter values
#'     are drawn from a uniform distribution. Stem locations and species are 
#'     randomly sampled, with repeats.
#' 
#' @examples
#' dat <- dataGen()
#' dat2 <- dataGen(nplots = 1, min_stems = 10, max_stems = 50, dbh_min = 10, 
#'   dbh_max = 200, plot_width = 20, plot_length = 50, 
#'   sp = c("Burkea africana", "Ochna pulchra"))
#' 
#' @export
#' 
dataGen <- function(nplots = 5, min_stems = 200, max_stems = 500, 
  min_diam = 5, max_diam = 100, plot_width = 100, plot_length = plot_width,
  species = LETTERS[1:20]) {

  # Get plot names
  plots <- as.character(seq_len(nplots))

  # Get number of stems per plot
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  n_stems <- resample(seq(min_stems, max_stems, 1), nplots, replace = TRUE)

  # Get grid coordinates
  x_coords <- seq(0, plot_width, 0.1)
  y_coords <- seq(0, plot_length, 0.1)
  xy_comb <- expand.grid(x_coords, y_coords)

  # For each plot:
  plots_df <- do.call(rbind, lapply(seq_along(plots), function(x) {
    # Plot ID
    plot_id <- plots[x]

    # Vector of stem IDs
    stem_id <- as.character(seq_len(n_stems[x]))

    # Stem coordinates
    grid_coords <- xy_comb[sample(seq_len(nrow(xy_comb)), n_stems[x], 
      replace = FALSE),]

    # Stem diameter values 
    diam <- round(runif(n_stems[x], min_diam, max_diam), 1)

    # Species 
    species <- sample(species, n_stems[x], replace = TRUE)

    # Create dataframe
    out <- data.frame(
      plot_id,
      stem_id,
      x_grid = grid_coords[,1],
      y_grid = grid_coords[,2],
      species,
      diam)

    # Return
    return(out)
  }))

  # Return
  return(plots_df)
}

