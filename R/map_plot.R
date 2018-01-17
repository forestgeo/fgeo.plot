#' Map a census plot -- empty or with elevation.
#' 
#' `map_plot()` produces a base on top of which you can add any __ggplot2__
#' layer (e.g. [ggplot2::geom_point()] and [ggplot2::facet_wrap()]; also
#' [geom_point_sp()] and [facet_wrap_sp()]).
#' `map_elevation()` and `map_species()` are convenient wrappers.
#' 
#' @param data Dataframe passed to [ggplot2::geom_point()].
#' @param elevation A dataframe with variables gx, gy, and elev giving the
#'   elevation of the site.
#' @param xlim,ylim A vector giving the limits of x, y axes, for example
#'   `xlim = c(0, 1000), ylim = c(0, 500)`. Default limits should be OK -- they
#'   are set to be c(0, max), where max is the maximum value of `gx` or `gy`
#'   in the data set.
#' @template theme
#' @param line_size A number to customize the width of the elevation lines.
#' @param low,high Colours to represent the range between low and high
#'   elevation. Use colour names like `low = "black", high = "red"` or HEX
#'   colours like `low = "#132B43", high = "#56B1F7"` (for more colours google
#'   #132B43).
#' @param bins A number. Setting bins creates evenly spaced contours in the
#'   range of the data. Integers
#' @param label_elev Logical; `FALSE` removes the labels of elevation lines.
#' @inheritParams fgeo_geoms
#' 
#' @seealso [ggplot2::geom_point()].
#' 
#' @return A ggplot.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' 
#' sp2 <- fgeo.tool::top(bciex::bci12s7mini, sp, 2)
#' bad_nms_elev <- bciex::bci_elevation
#' bci_elev <- dplyr::rename(bad_nms_elev, gx = x, gy = y)
#' 
#' # Plot
#' 
#' map_plot(sp2)
#' 
#' map_plot(sp2, elevation = bci_elev)
#' 
#' map_plot(sp2, elevation = bci_elev) +
#'   geom_point_sp(data = sp2)
#' 
#' # Elevation
#' 
#' map_elevation(NULL, bci_elev)
#' 
#' map_elevation(bci_elev)
#' 
#' map_elevation(sp2)
#' 
#' map_elevation(sp2) +
#'   geom_point_sp(data = sp2) +
#'   facet_wrap_sp()
#' 
#' # Species
#' 
#' map_species(sp2)
#' 
#' map_species(sp2, bci_elev, drop_fill = TRUE, label_elev = FALSE, size = 4) +
#'   facet_wrap_sp() +
#'   guides(color = "none")
#' 
#' map_species(sp2) +
#'   facet_grid_sp_h()
#' 
#' map_species(sp2) +
#'   facet_grid_sp_v()
#' }
map_plot <- function(data = NULL,
                     elevation = NULL,
                     xlim = NULL,
                     ylim = NULL,
                     theme = theme_map_sp(),
                     line_size = 0.5,
                     low = "#132B43",
                     high = "#f70404",
                     bins = NULL,
                     label_elev = TRUE) {
  msg <- "One of `data` or `elevation` must be not null."
  if (all(is.null(data), is.null(elevation))) rlang::abort(msg)
  
  if (all(is.null(elevation), any(grepl("elev", names(data))))) {
    rlang::warn(
      "You passed an elevation dataset as data (see argument `elevation`)"
    )
  }
  
  if (is.null(data)) {data <- elevation}
  check_map_plot(
    data = data, elevation = elevation, xlim = xlim, ylim = ylim, theme = theme
  )
    
  # If limits are not given by the user, set limits based on entire dataset
  if (is.null(xlim)) {xlim <- c(0, max(data$gx, na.rm = TRUE))}
  if (is.null(ylim)) {ylim <- c(0, max(data$gy, na.rm = TRUE))}
  
  if (is.null(elevation)) {
    base <- ggplot(data, aes(gx, gy))
  } else {
    msg <- paste0(
      "Using elevation as base data.\n",
      "* To plot census data on new layers, use `data` = your-census-data",
      collapse = ""
    )
    rlang::inform(msg)
    data <- elevation
    base <- ggplot(data, aes(gx, gy, z = elev))
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
      bins = bins,
      label_elev = label_elev
    )
  }
  p
}

add_elev <- function(p,
                     elevation,
                     line_size = 0.5,
                     low = "#132B43",
                     high = "#56B1F7",
                     bins = NULL,
                     label_elev = TRUE) {
  check_add_elev(
    p = p, 
    elevation = elevation,
    line_size = line_size,
    low = low,
    high = high,
    bins = bins,
    label_elev = label_elev
  )
  
  p_elev <- p +
    stat_contour(
      data = elevation,
      aes(x = gx, y = gy, z = elev, colour = ..level..),
      size = line_size,
      bins = bins
    ) +
    scale_colour_continuous(low = low, high = high)
  
  if (label_elev) label_elev(p_elev) else p_elev
}

label_elev <- function(w_elev,
                            # size = 3,
                            xyjust = 1.5,
                            fontface = "italic",
                            color = "grey",
                            ...) {
  built <- ggplot_build(w_elev)$data[[1]]
  elev <-  mutate(built, gx = .data$x, gy = .data$y)
  elev_x <- elev[elev$gx == max0(elev$gx), ]
  elev_y <- elev[elev$gy == max0(elev$gy), ]
  w_elev + 
    text_at_max(
      elev_x, 
      # size = size, 
      hjust = xyjust, 
      fontface = fontface, 
      color = color,
      ...
    ) + 
    text_at_max(
      elev_y, 
      # size = size, 
      vjust = xyjust, 
      fontface = fontface, 
      color = color,
      ...
    )
}

text_at_max <- function(x, ...) {
  suppressWarnings(
    # Warns that `z` is not used. This is intentional.
    geom_text(data = x, aes(label = x$level, z = NULL), ...)
  )
}


# Wrappers ----------------------------------------------------------------

#' @rdname map_plot
#' @export
map_elevation <- function(data = NULL,
                          elevation = NULL,
                          xlim = NULL,
                          ylim = NULL,
                          theme = theme_map_sp(),
                          line_size = 0.5,
                          low = "#132B43",
                          high = "#f70404",
                          bins = NULL,
                          label_elev = TRUE) {
  if (all(is.null(elevation), !any(grepl("elev", names(data))))) {
    rlang::warn("Did you forget to provide elevation data?")
  }
  map_plot(
    data = data,
    elevation = elevation,
    xlim = xlim,
    ylim = ylim,
    theme = theme,
    line_size = line_size,
    low = low,
    high = high,
    bins = bins,
    label_elev = label_elev
  )
}

#' @rdname map_plot
#' @export
map_species <- function(data,
                        elevation = NULL,
                        xlim = NULL,
                        ylim = NULL,
                        theme = theme_map_sp(),
                        drop_fill = FALSE, 
                        shape = 21, 
                        label_elev = TRUE,
                        ...) {
  map_plot(
    data = data,
    elevation = elevation,
    xlim = xlim,
    ylim = ylim,
    theme = theme,
    label_elev = label_elev
  ) +
    geom_point_sp(data = data, drop_fill = drop_fill, shape = shape, ...)
}

# Check -------------------------------------------------------------------

check_map_plot <- function(data, elevation, xlim, ylim, theme) {
  stopifnot(is.data.frame(data))
  if (!is.null(elevation)) {
    stopifnot(is.data.frame(elevation))
  }
  fgeo.tool::check_crucial_names(data, c("gx", "gy"))
  if (!is.null(xlim)) {stopifnot(xlim >= 0)}
  if (!is.null(ylim)) {stopifnot(ylim >= 0)}
  theme_has_class_theme <- any(grepl("theme", class(theme)))
  stopifnot(theme_has_class_theme)
}

check_add_elev <- function(p,
                           elevation,
                           line_size,
                           low,
                           high,
                           bins,
                           label_elev) {
  p_has_class_ggplot <- any(grepl("ggplot", class(p)))
  stopifnot(p_has_class_ggplot)
  elevation_is_dataframe <- any(grepl("data.frame", class(elevation)))
  stopifnot(elevation_is_dataframe)
  fgeo.tool::check_crucial_names(elevation, c("gx", "gy", "elev"))
  stopifnot(is.numeric(line_size))
  stopifnot(is.character(low))
  stopifnot(is.character(high))
  if (!is.null(bins)) {stopifnot(is.numeric(bins))}
  stopifnot(is.logical(label_elev))
}



# Layers ------------------------------------------------------------------

#' Geoms to add on top of ggplots based on ForestGEO's data.
#' @param data Dataframe passed to [ggplot2::geom_point()].
#' @param drop_fill Logical; `TRUE` drops the fill legend.
#' @param shape Numeric; Point shape passed to [ggplot2::geom_point()].
#' @param ... Arguments passed to [ggplot2::geom_point()].
#' 
#' @seealso [ggplot2::geom_point()]
#' @name fgeo_geoms
NULL

#' @rdname fgeo_geoms
#' @export
geom_point_sp <- function(data = NULL, drop_fill = FALSE, shape = 21, ...){
  if (is.null(data)) {
    msg <- paste0(
      "Did you forget to pass your census data?\n",
      "* You may need someting like: `geom_point_sp(data = your-census-data)`",
      collapse = ""
    )
    rlang::abort(msg)
  }
  
  if (drop_fill) {
    suppressWarnings(
      # Warns that z is NULL
      geom_point(data = data, aes(gx, gy, z = NULL), shape = shape, ...)
    )
  } else {
    suppressWarnings(
      # Warns that z is NULL
      geom_point(
        data = data, aes(gx, gy, z = NULL, fill = data$sp), shape = shape, ...
      )
    )
  }
}

#' Facets to add on top of ggplots based on ForestGEO's data.
#' 
#' @param ... Arguments passed to [ggplot2::facet_wrap()] and 
#'   [ggplot2::facet_grid()].
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

