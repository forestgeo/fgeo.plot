# Output a list of plots, each of one species from a census.

# General plot of gx by gy faceted by species.
plot_xy <- function(cns_data, xlim, ylim, ...) {
  ggplot2::ggplot(
    data = cns_data,
    ggplot2::aes(x = cns_data$gx, y = cns_data$gy)
  ) +
    ggplot2::geom_point(...) +
    ggplot2::facet_grid(. ~ cns_data$sp) +
    ggplot2::coord_fixed(xlim = xlim, ylim = ylim) +
    ggplot2::theme_bw()
}

# Standarized plot for each species (fixed ratio and limits).
plot_sp <- function(one_sp, cns_data, ...) {
  xlim <- c(0, max(cns_data$gx))
  ylim <- c(0, max(cns_data$gy))

  filtered_cns_data <- cns_data[cns_data$sp %in% one_sp, ]
  plot_xy(filtered_cns_data, xlim, ylim, ...)
}


#' Plot species from a list or vector.
#'
#' @param species A string of the species codes to plot (`sp`).
#' @param cns_data Census data.
#' @param ... Arguments passed to [ggplot2::geom_point()].
#'
#' @seealso [ggplot2::geom_point()], [grDevices::pdf()], [grDevices::png()].
#'
#' @section Acknowledgements:
#' Thanks to Gabriel Arellano and "Kenfack, David" for ideas and feedback.
#'
#' @return A list of plots. Usually you will want to save the list on a file (
#'     e.g. a .pdf, .png, etc.)
#' @export
#'
#' @examples
#' \dontrun{
#' # Ensure you have these packages
#' library(ggplot2)
#' library(dplyr)
#'
#' # Minimal data to show
#' library(bci)
#' cns_data <- bci::bci12full7
#' # Filtering only 3 species for a minimal example.
#' (species_selected <- unique(cns_data$sp)[1:3])
#' cns_data_sub <- filter(cns_data, sp %in% species_selected)
#' # Sampling only 1000 rows for a quick example
#' cns_data_sub <- sample_n(cns_data_sub, 1000)
#'
#' # Selecting all species in the example dataset
#' all_species <- unique(cns_data_sub$sp)
#' pdf(onefile = TRUE)
#' lapply_plot_sp(species = all_species, cns_data = cns_data_sub)
#' dev.off()
#'
#' pdf(onefile = TRUE)
#' lapply_plot_sp(
#'   species = all_species, cns_data = cns_data_sub,
#'   # Change the look of the points  passed to ?geom_point
#'   color = "blue", size = 3, shape = 1
#' )
#' dev.off()
#'
#' }
lapply_plot_sp <- function(species, cns_data, ...) {
  lapply(X = species, FUN = plot_sp, cns_data, ...)
}
