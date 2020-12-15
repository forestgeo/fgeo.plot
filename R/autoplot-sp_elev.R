#' Plot species distribution and/or topography.
#'
#' @description
#' Plot the columns `sp` and/or `elev` of ForestGEO-like datasets of class 'sp'
#' and/or 'sp_elev'.
#'
#' * You can create a 'sp' `object` with:
#' @description
#' ```
#'     object <- sp(DATA-WITH-VARIABLE-sp)
#' ```
#' * You can create an 'elev' `object` with:
#' @description
#' ```
#'     object <- elev(DATA-WITH-VARIABLE-elev)
#' ```
#' @description
#' * You can create a 'sp_elev' `object` with:
#' @description
#' ```
#'     object <- sp_elev(DATA-WITH-VARIABLE-sp, DATA-WITH-VARIABLE-elev)
#' ```
#'
#' @description
#' See __Examples__ below.
#'
#' @details
#' `autoplot(sp_elev(DATA-WITH-VARIABLE-sp)` (without elevation data) is
#' equivalent to `autoplot(sp(DATA-WITH-VARIABLE-sp))`.
#'
#' @template compare_ggplot2
#'
#' @inheritParams autoplot.elev
#' @param object An object created with [sp()], [elev()], or [sp_elev()].
#' @param fill Character; either a color or "sp", which maps each species to a
#'   different color.
#' @param hide_fill_legend Logical; `TRUE` hides the fill legend.
#' @param point_size A number giving point size. Passed to
#'   [ggplot2::geom_point()].
#' @param shape A number giving point shape (as in [graphics::points()]). Passed
#'   to [ggplot2::geom_point()].
#' @param facet Logical; `TRUE` wraps multiple panels within the area of a single
#'   graphic plot.
#' @param contour_size A number giving the size of the contour of elevation
#'   lines. Passed to `ggplot2::stat_contour()` (see [ggplot2::geom_contour()]).
#' @template low_high
#' @param bins A number giving the number of elevation lines to plot.
#' @param add_elevation_labels Logical; `FALSE` hides elevation labels.
#' @param hide_color_legend Logical; `TRUE` hides the color legend.
#' @template label_size_label_color_fontface
#' @param xyjust A number to adjust the position of the text labels of the
#'   elevation lines.
#' @template xlim_ylim
#' @param custom_theme A valid [ggplot2::theme()]. `NULL` uses the default
#'   theme [theme_default()].
#' @template autoplot_unused_dots
#'
#' @return A "ggplot".
#'
#' @examples
#' assert_is_installed("fgeo.x")
#' 
#' # Species ---------------------------------------------------------------
#' 
#' # Small dataset with a few species for quick examples
#' census <- fgeo.x::tree5 %>%
#'   subset(sp %in% c("PREMON", "CASARB"))
#' 
#' autoplot(sp(census))
#' 
#' # Skip R CMD check for speed
#' \donttest{
#' # Customize
#' autoplot(sp(census), point_size = 1)
#' 
#' # Elevation -------------------------------------------------------------
#' 
#' elevation <- fgeo.x::elevation
#' autoplot(elev(elevation))  
#' 
#' # Skip R CMD check for speed
#' # Same as `autoplot(elev(elevation))`
#' autoplot(elev(elevation$col))
#' 
#' # Customize
#' autoplot(elev(elevation), contour_size = 1)
#' 
#' # Species and elevation -------------------------------------------------
#' autoplot(sp_elev(census, elevation), facet = FALSE, point_size = 1)
#' }
#' @family plot functions
#' @family autoplots
#' @family functions to plot elevation
#' @family functions to plot species
#' @export
autoplot.sp_elev <- function(object,
                             fill = "sp",
                             hide_fill_legend = FALSE,
                             shape = 21,
                             point_size = 3,
                             facet = TRUE,
                             contour_size = 0.5,
                             low = "blue",
                             high = "red",
                             hide_color_legend = FALSE,
                             bins = NULL,
                             add_elevation_labels = TRUE,
                             label_size = 3,
                             label_color = "grey",
                             xyjust = 1,
                             fontface = "italic",
                             xlim = NULL,
                             ylim = NULL,
                             custom_theme = NULL,
                             ...) {
  plot_sp_elev(
    census = object[[1]],
    elevation = object[[2]],
    fill = fill,
    hide_fill_legend = hide_fill_legend,
    shape = shape,
    point_size = point_size,
    facet = facet,
    contour_size = contour_size,
    low = low,
    high = high,
    hide_color_legend = hide_color_legend,
    bins = bins,
    add_elevation_labels = add_elevation_labels,
    label_size = label_size,
    label_color = label_color,
    xyjust = xyjust,
    fontface = fontface,
    xlim = xlim,
    ylim = ylim,
    custom_theme = custom_theme
  )
}

#' @rdname autoplot.sp_elev
#' @export
autoplot.sp <- function(object,
                        fill = "sp",
                        hide_fill_legend = FALSE,
                        shape = 21,
                        point_size = 3,
                        facet = TRUE,
                        xlim = NULL,
                        ylim = NULL,
                        custom_theme = NULL,
                        ...) {
  autoplot(
    sp_elev(sp = object, elev = NULL),
    fill = fill,
    hide_fill_legend = hide_fill_legend,
    shape = shape,
    point_size = point_size,
    facet = facet,
    xlim = xlim,
    ylim = ylim,
    custom_theme = custom_theme
  )
}

#' @rdname autoplot.sp_elev
#' @export
autoplot.elev <- function(object,
                          contour_size = 0.5,
                          low = "blue",
                          high = "red",
                          hide_color_legend = FALSE,
                          bins = NULL,
                          add_elevation_labels = TRUE,
                          label_size = 3,
                          label_color = "grey",
                          xyjust = 1,
                          fontface = "italic",
                          xlim = NULL,
                          ylim = NULL,
                          custom_theme = NULL,
                          ...) {
  plot_elev(
    elevation = object,
    contour_size = contour_size,
    low = low,
    high = high,
    hide_color_legend = hide_color_legend,
    bins = bins,
    add_elevation_labels = add_elevation_labels,
    label_size = label_size,
    label_color = label_color,
    xyjust = xyjust,
    fontface = fontface,
    xlim = xlim,
    ylim = ylim,
    custom_theme = custom_theme
  )
}

autoplot.default <- function(object, ...) {
  abort(glue("
    Can't deal with data unless it is of class 'sp', 'elev' or 'sp_elev'.
    Do you forget to use `sp()`, `elev()` or `sp_elev()`?
  "))
}
