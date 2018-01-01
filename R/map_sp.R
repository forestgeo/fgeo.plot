# Map species from a census.

#' Map the distribution of one, some or all species in a census data set.
#'
#' @param census Census data.
#' @param species A string of the species codes to plot (`sp`).
#' @param xlim,ylim A vector giving the limits of x, y axes, for example
#'   `xlim = c(0, 1000), ylim = c(0, 500)`. Default limits should be OK -- they
#'   are set to be c(0, max), where max is the maximum value of `gx` or `gy`
#'   in the data set.
#' @template theme
#' @param elevation A dataframe with variables gx, gy, and elev giving the
#'   elevation of the site.
#' @param line_size A number to customize the width of the elevation lines.
#' @param low,high Colours to represent the range between low and high
#'   elevation. Use colour names like `low = "black", high = "red"` or HEX
#'   colours like `low = "#132B43", high = "#56B1F7"` (for more colours google
#'   #132B43).
#' @param bins A number. Setting bins creates evenly spaced contours in the
#'   range of the data. Integers
#' @param ... Arguments passed to [ggplot2::geom_point()] to customize, for
#'   example, the size, shape, or colour of the points.
#'
#' @seealso [grDevices::pdf()], [grDevices::png()],
#'   [ggplot2::theme()].
#'
#' @section Acknowledgement:
#' Thanks to Gabriel Arellano and David Kenfack for ideas and feedback.
#'
#' @return A list of which each element is a plot of class ggplot.
#' @export
#'
#' @examples
#' \dontrun{
#' # Setup -------------------------------------------------------------------
#'
#' # For easier data manipulation
#' library(dplyr)
#' # Print only a few rows of tibbles (modern dataframes) to save space
#' options(dplyr.print_min = 6, dplyr.print_max = 6)
#'
#' # Quick exploration with defaults -----------------------------------------
#'
#' # Example data. Converting dataframe to tibble for better printing
#' census <- as_tibble(bciex::bci12t7mini)
#'
#' # Print to screen
#' map_sp(census, "hybapr")
#'
#' # Print to .pdf -- one species per page of a single file
#' pdf()
#' map_sp(census, c("hybapr", "faraoc"))
#' dev.off()
#'
#' # Add elevation data
#' elev <- bciex::bci_elevation
#'
#' # Not the right names
#' names(elev)
#' elev <- rename(elev, gx = x, gy = y)
#' # OK now
#' elev
#'
#' # Common changes ----------------------------------------------------------
#'
#' # Changing points' properties
#' map_sp(
#'   census, "hybapr",
#'   # Passed to ggplot2::geom_point()
#'   size = 4, shape = 22, fill = "green", colour = "black", stroke = 2
#' )
#'
#' # Changing lines' properties
#' map_sp(
#'   census, "hybapr",
#'   elevation = elev, line_size = 1, low = "red", high = "blue", bins = 10
#' )
#'
#' # Dealing with overplotting
#' crowded <- dplyr::tibble(
#'   sp = sample(c("species1"), 10000, replace = TRUE),
#'   gx = sample.int(1000, 10000, replace = TRUE),
#'   gy = sample.int(500, 10000, replace = TRUE)
#' )
#' # Too crowded
#' map_sp(crowded, c("species1"))
#' # Less crowded
#' map_sp(
#'   crowded, c("species1"),
#'   # passing arguments to ggplot2::geom_point()
#'   size = 1, alpha = 5/10, shape = 21
#' )
#'
#' # Changing theme
#' map_sp(census, "hybapr", theme = ggplot2::theme_classic())
#' map_sp(census, "hybapr", theme = ggplot2::theme_dark())
#' # For more options see ?ggplot2::theme_bw()
#'
#' # Less common changes -----------------------------------------------------
#'
#' # Changing limits
#' map_sp(census, "hybapr", xlim = c(0, 1500), ylim = c(0, 1000))
#'
#' # Fine tunning ------------------------------------------------------------
#'
#' library(ggplot2)
#' library(gridExtra)
#'
#' # Multiple maps per page
#' three_species <- c("hybapr", "faraoc", "des2pa")
#' maps <- map_sp(census, three_species)
#' multipaged <- marrangeGrob(maps, nrow = 1, ncol = 2)
#' multipaged
#' # Saving to .pdf: Option 1
#' ggplot2::ggsave("my_multipaged.pdf", multipaged)
#' # Saving to .pdf: Option 2
#' pdf()
#' multipaged
#' dev.off()
#'
#' # Custom theme; see all the options with ?ggplot2::theme()
#' my_theme <- ggplot2::theme(
#'   text = element_text(size = 25, face = "bold.italic", colour = "white"),
#'   plot.background = element_rect(fill = "black"),
#'   plot.margin = margin(2, 2, 2, 2, "cm"),
#'   strip.background = element_rect(fill = "darkgreen"),
#'   strip.text = element_text(colour = "white"),
#'   # make grid dissapear by matching background colour
#'   panel.background = element_rect(fill = "lightgreen"),
#'   panel.grid.minor = element_line(colour = "lightgreen"),
#'   panel.grid.major = element_line(colour = "lightgreen")
#' )
#' map_sp(census, "hybapr", theme = my_theme)
#'
#'
#' # Extending with ggplot2 --------------------------------------------------
#'
#' # Adding new layers
#' p0 <- map_sp(census, c("hybapr", "faraoc"))
#' # To one element of the plots' list
#' p0[["hybapr"]] + geom_vline(aes(xintercept = 300), colour = "red")
#' # To all elements of the plots' list
#' p1 <- lapply(p0, `+`, geom_vline(aes(xintercept = 300), colour = "red"))
#' p1
#' p2 <- lapply(p1, `+`, geom_hline(aes(yintercept = 400), colour = "blue"))
#' p2
#' #' }
map_sp <- function(census,
                   species,
                   xlim = NULL,
                   ylim = NULL,
                   theme = theme_map_sp(),
                   elevation = NULL,
                   line_size = 0.5,
                   low = "#132B43",
                   high = "#56B1F7",
                   bins = NULL,
                   ...) {
  check_map_sp(census = census, species = species, xlim = xlim, ylim = ylim)

  # If plot limits are not given by the user, set limits based on entire dataset
  if (is.null(xlim)) {xlim <- c(0, max(census$gx, na.rm = TRUE))}
  if (is.null(ylim)) {ylim <- c(0, max(census$gy, na.rm = TRUE))}

  # Focus on given species
  cns <- census[census$sp %in% species, ]
  cns_list <- split(cns, cns$sp)
  p <- lapply(
    cns_list,
    map_sp_each,
    xlim = xlim,
    ylim = ylim,
    theme = theme,
    elevation =
      elevation,
    line_size = line_size,
    low = low,
    high = high,
    bins = bins,
    ...
  )
  setNames(p, sort(species))
}

check_map_sp <- function(census, species, xlim, ylim) {
  stopifnot(is.data.frame(census))
  stopifnot(is.character(species))
  if (length(species) == 0) {stop("The vector `sp` is empty.")}
  fgeo.utils::check_crucial_names(census, c("gx", "gy", "sp"))
  invisible(census)
}

map_sp_each <- function(cns,
                        xlim,
                        ylim,
                        theme,
                        elevation,
                        line_size,
                        low,
                        high,
                        bins,
                        ...) {
  p <- ggplot(cns, aes(gx, gy)) +
    geom_point(...) +
    labs(x = NULL, y = NULL, title = unique(cns$sp)) +
    coord_fixed(xlim = xlim, ylim = ylim) +
    scale_x_continuous(minor_breaks = seq(xlim[1], xlim[2], 20)) +
    scale_y_continuous(minor_breaks = seq(ylim[1], ylim[2], 20)) +
    theme
  if (!is.null(elevation)) {
    p <- add_elevation(
      p, elev = elev, line_size = line_size, low = low, high = high, bins = bins
    )
  }
  p
}

#' Add elevation lines to a ggplot.
#' @noRd
add_elevation <- function(ggplot,
                          elev,
                          line_size = 0.5,
                          low = "#132B43",
                          high = "#56B1F7",
                          bins = NULL) {
  base_plot_is_class_ggplot <- any(grepl("ggplot", class(ggplot)))
  stopifnot(base_plot_is_class_ggplot)
  elevation_is_dataframe <- any(grepl("data.frame", class(elev)))
  stopifnot(elevation_is_dataframe)
  fgeo.utils::check_crucial_names(elev, c("gx", "gy", "elev"))

  p <- ggplot +
    stat_contour(data = elev,
      aes(x = gx, y = gy, z = elev, colour = ..level..),
      size = line_size, bins = bins) +
    scale_colour_continuous(low = low, high = high)
  labels_properties <- list("far.from.others.borders", "calc.boxes",
    "enlarge.box", box.color = NA, fill = "transparent", "draw.rects")
  p_with_labels <- directlabels::direct.label(p, labels_properties)
  p_with_labels
}
