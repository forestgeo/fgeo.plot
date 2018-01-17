#' Map a census plot -- empty or with elevation.
#' 
#' `map_plot()` produces a base on top of which you can add any __ggplot2__
#' layer (e.g. [ggplot2::geom_point()] and [ggplot2::facet_wrap_sp()]; also
#' [geom_point_sp()] and [facet_wrap_sp()]). `map_elevation()` is a wrapper
#' that forces elevation, and has an informative name.
#' 
#' @seealso [ggplot2::geom_point()].
#'
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
#'
#' @return A ggplot.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' 
#' sp4 <- fgeo.tool::top(bciex::bci12s7mini, sp, 4)
#' bad_nms_elev <- bciex::bci_elevation
#' bci_elev <- dplyr::rename(bad_nms_elev, gx = x, gy = y)
#' 
#' map_plot(sp4)
#' 
#' # Same
#' elev1 <- map_plot(sp4, elevation = bci_elev)
#' # A small wrapper to make it easy to find this function by its name
#' elev2 <- map_elevation(sp4, elevation = bci_elev)
#' elev2
#' all.equal(elev1, elev2)
#' 
#' # This works but it is not equal. Nowhere we are giving census data.
#' elev3 <- map_elevation(elevation = bci_elev)
#' # This works because we are using the argument `data`
#' elev3 + geom_point_sp(data = sp4)
#' # But this fails
#' # elev3 + geom_point_sp()
#' # This works because we passed the data before
#' elev2 + geom_point_sp()
#' 
#' # See xlim, ylim at ?ggplot2::coord_fixed())
#' map_elevation(sp4, elevation = bci_elev, xlim = c(200, 600), ylim = c(0, 400))
#' 
#' map_plot(sp4) + geom_point_sp()
#' # Same: A small wrapper to make it easy to find this function by its name
#' map_species(sp4)
#' # See ?ggplot2::geom_point(), ?ggplot2::theme_gray().
#' map_species(sp4, size = 4, stroke = 2, xlim = c(0, 500), theme = theme_gray())
#' 
#' 
#' # See also ?ggplot2::facet_wrap()
#' map_species(sp4) + facet_wrap_sp()
#' # See ?graphics::points
#' map_species(sp4, drop_fill = TRUE) + facet_wrap_sp()
#' 
#' # See also ?ggplot2::facet_grid()
#' map_elevation(sp4, elevation = bci_elev) +
#'   geom_point_sp(drop_fill = TRUE) +
#'   facet_grid_sp_v()
#' 
#' map_elevation(sp4, elevation = bci_elev) +
#'   geom_point_sp(drop_fill = TRUE) +
#'   facet_grid_sp_h()
#' }
map_plot <- function(data,
                     elevation = NULL,
                     xlim = NULL,
                     ylim = NULL,
                     theme = theme_map_sp(),
                     line_size = 0.5,
                     low = "#132B43",
                     high = "#56B1F7",
                     bins = NULL) {
  # p <- map_base(
  #   data = data,
  #   xlim = xlim,
  #   ylim = ylim,
  #   theme = theme
  # )
  check_map_plot(data = data, xlim = xlim, ylim = ylim, theme = theme)
    
  # If limits are not given by the user, set limits based on entire dataset
  if (is.null(xlim)) {xlim <- c(0, max(data$gx, na.rm = TRUE))}
  if (is.null(ylim)) {ylim <- c(0, max(data$gy, na.rm = TRUE))}
  
  if (is.null(elevation)) {
    base <- ggplot(data, aes(gx, gy))
  } else {
    base <- ggplot(data, aes(gx, gy, z = elevation$elev))
  }
  p <- base +
    coord_fixed(xlim = xlim, ylim = ylim) +
      scale_x_continuous(minor_breaks = seq(xlim[1], xlim[2], 20)) +
      scale_y_continuous(minor_breaks = seq(ylim[1], ylim[2], 20)) +
      labs(x = NULL, y = NULL) +
      theme
  
  if (!is.null(elevation)) {
    p <- add_elev(
      p, 
      elevation = elevation, 
      line_size = line_size,
      low = low,
      high = high,
      bins = bins
    )
  }
  p
}

#' @rdname map_plot
#' @export
map_elevation <- function(data = NULL, elevation, ...) {
  if (!is.null(data)) {
    map_plot(data = data, elevation = elevation, ...)
  } else {
    warning(
      "Using elevation as data.\n",
      "In new layers you may need to pass census data to the argument `data`."
    )
    data <- elevation
    map_plot(data = data, elevation = elevation, ...)
  }
}

#' @rdname map_plot
#' @export
map_species <- function(data, 
                        xlim = NULL,
                        ylim = NULL,
                        theme = theme_map_sp(),
                        drop_fill = FALSE, 
                        shape = 21, 
                        ...) {
  map_plot(
    data = data,
    xlim = xlim,
    ylim = ylim,
    theme = theme,
  ) +
    geom_point_sp(data = data, drop_fill = drop_fill, shape = shape, ...)
}

add_elev <- function(p,
  elevation,
  line_size = 0.5,
  low = "#132B43",
  high = "#56B1F7",
  bins = NULL) {
  check_add_elev(
    p = p, 
    elevation = elevation,
    line_size = line_size,
    low = low,
    high = high,
    bins = bins
  )
  
  p_elev <- p +
    stat_contour(
      data = elevation,
      aes(x = gx, y = gy, z = elev, colour = ..level..),
      size = line_size,
      bins = bins
    ) +
    scale_colour_continuous(low = low, high = high)
  # p_elev references elevation on a legend to the right of the plot. Not nice.
  reference_elev_on_map(p_elev)
}

reference_elev_on_map <- function(p) {
  label_properties <-   list(
    "far.from.others.borders", "calc.boxes",
    "enlarge.box", box.color = NA, fill = "transparent", "draw.rects"
  )
  directlabels::direct.label(p, label_properties)
}



# Check -------------------------------------------------------------------

check_map_plot <- function(data, xlim, ylim, theme) {
  stopifnot(is.data.frame(data))
  fgeo.tool::check_crucial_names(data, c("gx", "gy"))
  if (!is.null(xlim)) {stopifnot(xlim >= 0)}
  if (!is.null(ylim)) {stopifnot(ylim >= 0)}
  theme_has_class_theme <- any(grepl("theme", class(theme)))
  stopifnot(theme_has_class_theme)
}

check_add_elev <- function(p, elevation, line_size, low, high, bins) {
  p_has_class_ggplot <- any(grepl("ggplot", class(p)))
  stopifnot(p_has_class_ggplot)
  elevation_is_dataframe <- any(grepl("data.frame", class(elevation)))
  stopifnot(elevation_is_dataframe)
  fgeo.tool::check_crucial_names(elevation, c("gx", "gy", "elev"))
  stopifnot(is.numeric(line_size))
  stopifnot(is.character(low))
  stopifnot(is.character(high))
  if (!is.null(bins)) {stopifnot(is.numeric(bins))}
}



# Layers ------------------------------------------------------------------

#' Geoms to add on top of ggplots based on ForestGEO's data.
#' @seealso [ggplot2::geom_point()]
#' @name fgeo_geoms
NULL

#' @rdname fgeo_geoms
#' @export
geom_point_sp <- function(data = NULL, drop_fill = FALSE, shape = 21, ...){
  if (drop_fill) {
    geom_point(data = data, aes(gx, gy), shape = shape, ...)
  } else {
    geom_point(data = data, aes(gx, gy, fill = sp), shape = shape, ...)
  }
}

#' Facets to add on top of ggplots based on ForestGEO's data.
#' @seealso [ggplot2::facet_wrap()], [ggplot2::facet_grid()].
#' @name fgeo_facets
NULL

#' @rdname fgeo_facets
#' @export
facet_wrap_sp <- function(...) {
  facet_wrap(~sp, ...)
}

#' @rdname fgeo_facets
#' @export
facet_grid_sp_h <- function(...) {
  facet_grid(.~sp, ...)
}

#' @rdname fgeo_facets
#' @export
facet_grid_sp_v <- function(...) {
  facet_grid(sp~., ...)
}

