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
#' library(map)
#' library(fgeo.utils)
#' library(dplyr)
#' # Avoid conflict with `stats::filter()`
#' filter <- dplyr::filter
#'
#' # Example data
#' census <- bciex::bci12t7mini
#' species <- c("hybapr", "faraoc")
#'
#' # Defaults
#' p <- map_sp(census, species)
#' # Visualizing only the first plot of `p`
#' first(p)
#' # Printing all plots of `p` to .pdf, with parameters optimized for size letter
#' tmp <- tempfile()  # Remplace this by somehtihing like "maps.pdf"
#' pdf(tmp, paper = "letter", height = 10.5, width = 8)
#' p
#' dev.off()
#' unlink(tmp)
#'
#' # Simple tweaks
#' p <- map_sp(
#'   census, species,
#'   # Passed to ggplot2::geom_point()
#'   size = 4, shape = 22, fill = "green", colour = "black", stroke = 2
#' )
#' first(p)
#'
#' # Add elevation and tweak lines
#' # Fixing wrong names of elevation data
#' elevation <- rename(bciex::bci_elevation, gx = x, gy = y)
#' p <- map_sp(
#'   census, species,
#'   elevation = elevation, line_size = 1, low = "red", high = "blue", bins = 10
#' )
#' first(p)
#'
#' # Dealing with overplotting
#' crowded <- tibble(
#'   sp = sample(c("species1"), 10000, replace = TRUE),
#'   gx = sample.int(1000, 10000, replace = TRUE),
#'   gy = sample.int(500, 10000, replace = TRUE)
#' )
#' map_sp(crowded, c("species1"))
#' # Less overplotting
#' map_sp(crowded, c("species1"), size = 1, alpha = 5/10, shape = 21)
#'
#' # Limits
#' p <- map_sp(census, species, xlim = c(0, 1500), ylim = c(0, 1000))
#' first(p)
#'
#' # Themes
#' library(ggplot2)
#' # Using pre-made themes
#' p <- map_sp(census, species, theme = ggplot2::theme_classic())
#' first(p)
#' # Tweaking the default theme of map_sp()
#' small_tweak <- theme_map_sp(
#'   text = element_text(size = 30, face = "bold.italic")
#' )
#' p <- map_sp(census, species, theme = small_tweak)
#' first(p)
#' large_tweak <- theme(
#'   legend.position = "bottom",
#'   legend.title = element_blank(),
#'   legend.text = element_text(size = 8, colour = "red"),
#'   text = element_text(size = 11, face = "bold.italic", colour = "white"),
#'   plot.background = element_rect(fill = "black"),
#'   plot.margin = margin(2, 2, 2, 2, "cm"),
#'   strip.background = element_rect(fill = "darkgreen"),
#'   strip.text = element_text(colour = "white"),
#'   panel.background = element_rect(fill = "lightgreen"),
#'   panel.grid.minor = element_line(colour = "black", linetype = "dotted"),
#'   panel.grid.major = element_line(colour = "black")
#' )
#' p <- map_sp(census, species, theme = large_tweak)
#' first(p)
#'
#' # Multiple maps per page
#' library(gridExtra)
#' four_species <- c("hybapr", "faraoc", "des2pa", "tri2tu")
#' p <- map_sp(census, four_species)
#' multipaged <- marrangeGrob(p, nrow = 1, ncol = 2)
#' # Printing all plots of `p` to .pdf, with parameters optimized for size letter
#' tmp <- tempfile()
#' # Option 1
#' pdf(tmp, paper = "letter", height = 10.5, width = 8)
#' multipaged
#' dev.off()
#' unlink(tmp)
#' # Option 2
#' ggsave("map.pdf", multipaged, height = 10.5, width = 8)
#'
#' # Extending with ggplot2
#' p0 <- map_sp(census, species)
#' #  Adding new layer to one element of the plots' list
#' p0[["hybapr"]] + geom_vline(aes(xintercept = 300), colour = "red")
#' # Adding new layer to all elements of the plots' list
#' # * Adding a vertical line
#' p1 <- lapply(p0, `+`, geom_vline(aes(xintercept = 300), colour = "red"))
#' marrangeGrob(p1, nrow = 2, ncol = 1)
#' # * Also adding a horizontal line
#' p2 <- lapply(p1, `+`, geom_hline(aes(yintercept = 400), colour = "blue"))
#' marrangeGrob(p2, nrow = 2, ncol = 1)
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
    elevation = elevation,
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
      p, elevation = elevation, line_size = line_size, low = low, high = high,
      bins = bins
    )
  }
  p
}

#' Add elevation lines to a ggplot.
#' @noRd
add_elevation <- function(ggplot,
                          elevation,
                          line_size = 0.5,
                          low = "#132B43",
                          high = "#56B1F7",
                          bins = NULL) {
  base_plot_is_class_ggplot <- any(grepl("ggplot", class(ggplot)))
  stopifnot(base_plot_is_class_ggplot)
  elevation_is_dataframe <- any(grepl("data.frame", class(elevation)))
  stopifnot(elevation_is_dataframe)
  fgeo.utils::check_crucial_names(elevation, c("gx", "gy", "elev"))

  p <- ggplot +
    stat_contour(data = elevation,
      aes(x = gx, y = gy, z = elev, colour = ..level..),
      size = line_size, bins = bins) +
    scale_colour_continuous(low = low, high = high)
  labels_properties <- list("far.from.others.borders", "calc.boxes",
    "enlarge.box", box.color = NA, fill = "transparent", "draw.rects")
  p_with_labels <- directlabels::direct.label(p, labels_properties)
  p_with_labels
}
