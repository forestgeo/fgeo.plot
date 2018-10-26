# Limits ------------------------------------------------------------------

#' Set the map limits.
#' 
#' @template p
#' @template xlim_ylim
#' 
#' @seealso [plot_sp_elev()], [plot_each_species()],
#'   [ggplot2::coord_fixed()].
#' 
#' @family functions to create or modify plot layers
#' 
#' @export
#' @examples 
#' some_sp <- c("PREMON", "CASARB")
#' census <- subset(fgeo.data::luquillo_stem5_random, sp %in% some_sp)
#' p <- plot_base_census(census)
#' p
#' 
#' axis_limits(p, xlim = c(0, 100), ylim = c(0, 400))
#' 
#' fgeo.data::luquillo_elevation %>% 
#'   plot_base_elevation() %>% 
#'   axis_limits(xlim = c(0, 100), ylim = c(0, 400))
axis_limits <- function(p, xlim = NULL, ylim = NULL) {
  # If user doesn't provide limits, set limits based on entire dataset
  data <- p[["data"]]
  xlim <- best_lim(xlim, data$gx)
  ylim <- best_lim(ylim, data$gy)
  
  p +
    coord_fixed(xlim = xlim, ylim = ylim) +
    scale_x_continuous(minor_breaks = seq(xlim[1], xlim[2], 20)) +
    scale_y_continuous(minor_breaks = seq(ylim[1], ylim[2], 20))
}

# Layers ------------------------------------------------------------------

add_species <- function(p,
  data = NULL,
  fill = "sp",
  shape = 21,
  point_size = 3) {
  check_add_species(p = p, data = data)
  
  if (fill != "sp") {
    # `z` = NULL because base may have `z`, e.g.: aes(z = elevation)`
    p <- p + 
      suppressWarnings(
        geom_point(
          data = data, aes(gx, gy, z = NULL), 
          shape = shape, size = point_size, fill = fill
        )
      )
    return(p)
  } else {
    p + 
      suppressWarnings(
        geom_point(
          data = data, 
          aes(gx, gy, z = NULL, fill = sp), 
          shape = shape, size = point_size
        )
      )
    
  }
}

#' Represent elevation with lines.
#' 
#' @template p
#' @param contour_size A number giving the size of the contour of elevation
#'   lines. Passed to `ggplot2::stat_contour()` (see [ggplot2::geom_contour()]).
#' @template low_high
#' @param bins A number giving the number of elevation lines to map.
#' 
#' @seealso [plot_each_species()], [plot_sp_elev()], [plot_elev()], 
#'   [ggplot2::geom_contour()].
#' 
#' @family functions to create or modify plot layers
#' @export
#' @examples 
#' p <- plot_base_elevation(fgeo.data::luquillo_elevation)
#' add_elevation_contours(p)
#' add_elevation_contours(p, contour_size = 0.5, low = "grey", high = "black", bins = 4)
add_elevation_contours <- function(p, 
  contour_size = 1, 
  low = "blue", 
  high = "red", 
  bins = NULL) {
  check_crucial_names(p[["data"]], "elev")
  
  p +
    stat_contour(
      aes(x = gx, y = gy, z = elev, colour = ..level..), 
      size = contour_size, bins = bins
    ) +
    scale_colour_continuous(low = low, high = high)
}

#' Label elevation lines.
#' 
#' @template p
#' @template label_size_label_color_fontface
#' @param xyjust A number to adjust the position of the text labels of the 
#'   elevation lines.
#'
#' @seealso [plot_each_species()], [plot_sp_elev()],
#'   [plot_elev()], [ggplot2::geom_text()].
#' 
#' @family functions to create or modify plot layers
#' 
#' @export
#' @examples 
#' elevation <- fgeo.data::luquillo_elevation
#' 
#' add_elevation_contours(plot_base_elevation(elevation))
#' 
#' elevation %>%
#'   plot_base_elevation() %>% 
#'   add_elevation_contours() %>%
#'   add_elevation_labels(label_size = 2, label_color = "black", xyjust = -0.25)
add_elevation_labels <- function(p, 
  label_size = 3,
  label_color = "grey",
  xyjust = 1, 
  fontface = "italic") {
  check_add_elevation_labels(
    p = p,
    label_color = label_color,
    label_size = label_size,
    xyjust = xyjust,
    fontface = fontface
  )
  
  p +
    text_at_max(
      max_elev(p)$x,
      label_size = label_size,
      label_color = label_color,
      xyjust = xyjust,
      fontface = fontface
    ) +
    text_at_max(
      max_elev(p)$y,
      label_size = label_size,
      label_color = label_color,
      xyjust = xyjust * 1.3,
      fontface = fontface
    )
}

max_elev <- function(p) {
  built <- ggplot_build(p)$data[[1]]
  elev <-  mutate(built, gx = .data$x, gy = .data$y)
  elev_x <- elev[elev$gx == max0(elev$gx), ]
  elev_y <- elev[elev$gy == max0(elev$gy), ]
  list(x = elev_x, y = elev_y)
}

text_at_max <- function(x,
  xyjust,
  label_size = 3,
  label_color = "grey",
  fontface = "italic") {
  # Mute warning that `z` NULL
  suppressWarnings(
    geom_text(
      data = x,
      aes(label = level, z = NULL),
      size = label_size,
      color =  label_color,
      hjust = xyjust,
      vjust = xyjust,
      fontface =  fontface
    )
  )
}




# Hide --------------------------------------------------------------------

#' Hide elements of a plot.
#' 
#' `hide_axis_labels` hides axis labels.
#' `hide_color_legend` hides the color legend, for example, of elevation lines.
#' 
#' @template p
#' 
#' @seealso [ggplot2::labs()],  [ggplot2::guides()].
#' 
#' @family functions to create or modify plot layers
#' 
#' @examples 
#' elevation <- fgeo.data::luquillo_elevation
#' p <- plot_base_elevation(elevation)
#' hide_axis_labels(p)
#' 
#' elevation %>% 
#'   plot_base_elevation() %>% 
#'   add_elevation_contours() %>% 
#'   hide_color_legend()
#' @name hide 
NULL

#' @rdname hide
#' @export
hide_axis_labels <- function(p) {
  p + labs(x = NULL, y = NULL)
}

#' @rdname hide
#' @export
hide_color_legend <- function(p) {
  p + guides(color = "none")
}

# Facets ------------------------------------------------------------------

# @param ... Arguments passed to [ggplot2::facet_wrap()] and 
#   [ggplot2::facet_grid()].



#' Facet data by a variable, and wrap multiple plots to fit a single page.
#' 
#' This function behaves like [ggplot2::facet_wrap()] but it is designed to 
#' allow using it as part of a pipe.
#' 
#' @template p
#' @inheritParams ggplot2::facet_wrap
#' @inheritDotParams ggplot2::facet_wrap
#' @seealso [ggplot2::facet_wrap()], [ggplot2::facet_grid()].
#' 
#' @family functions to create or modify plot layers
#' 
#' @export
#' @examples
#' some_sp <- c("PREMON", "CASARB")
#' census <- subset(fgeo.data::luquillo_stem5_random, sp %in% some_sp)
#' p <- add_species(plot_base_census(census))
#' p
#' 
#' facet(p, "sp")
#' # Same
#' facet(p, ~sp)
#' 
#' facet(p, c("sp", "status"))
#' 
#' # Same, all in one step
#' census %>% 
#'   plot_base_census() %>% 
#'   add_species() %>% 
#'   facet(c("sp", "status"))
facet <- function(p, facets, ...) {
  p + facet_wrap(facets, ...)
}

