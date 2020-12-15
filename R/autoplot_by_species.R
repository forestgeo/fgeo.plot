#' List plots of species distribution and topography (good for pdf output).
#'
#' @description
#' These functions extend [autoplot.sp()] and [autoplot.elev()] and return not a
#' single plot but a list of plots. They are particularly useful if you want to
#' print a _.pdf_ file with one plot per page. They automatically plot the
#' variables `sp` and `elev` of a ForestGEO-like dataset of class 'sp' or
#' 'sp_elev'.
#' * Create a 'sp' `object` with:
#' @description
#' ```
#'     object <- sp(DATA-WITH-VARIABLE-sp)
#' ```
#' * Create a 'sp_elev' `object` with:
#' @description
#' ```
#'     object <- sp_elev(DATA-WITH-VARIABLE-sp, DATA-WITH-VARIABLE-elev)
#' ```
#' @description
#' See sections __Usage__ and __Examples__.
#'
#' @details
#' `autoplot_by_species(sp_elev(DATA-WITH-VARIABLE-sp)` (without elevation data)
#' is equivalent to `autoplot_by_species(sp(DATA-WITH-VARIABLE-sp))`.
#'
#' @template compare_ggplot2
#'
#' @param object An object created with [sp()] or [sp_elev()].
#' @param species A character vector giving values in the column `sp`. The
#'   output will be a list with as many plots as elements in this vector.
#'   The string "all" (default) plots all unique values of `sp`.
#' @template low_high
#' @template label_size_label_color_fontface
#' @template xlim_ylim
#' @inheritParams autoplot.sp
#' @inheritParams autoplot.elev
#' @inheritParams autoplot_by_species.sp
#' @template autoplot_unused_dots
#'
#' @seealso [autoplot()], [sp()], [sp_elev()].
#'
#' @template return_a_list_of_ggplots
#'
#' @examples
#' assert_is_installed("fgeo.x")
#' 
#' # Species ---------------------------------------------------------------
#' # Small dataset with a few species for quick examples
#' census <- fgeo.x::tree6_3species
#' 
#' # Showing only two species for speed
#' autoplot_by_species(sp(census))[1:2]
#' 
#' # To print all plots in a .pdf see `?pdf()`
#' autoplot_by_species(sp(census))
#' 
#' # Species and elevation (optional) ---------------------------------------
#' 
#' # Species and elevation
#' elevation <- fgeo.x::elevation
#' autoplot_by_species(sp_elev(census, elevation))
#' @family plot functions
#' @family functions to list plots from different ForestGEO classes
#' @family functions to plot elevation
#' @family functions to plot species
#' @export
autoplot_by_species.sp_elev <- function(object,
                                        species = "all",
                                        fill = "black",
                                        shape = 21,
                                        point_size = 3,
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
  plot_each_species(
    census = object[[1]],
    elevation = object[[2]],
    species = species,
    fill = fill,
    shape = shape,
    point_size = point_size,
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

#' @rdname autoplot_by_species.sp_elev
#' @export
autoplot_by_species.sp <- function(object,
                                   species = "all",
                                   fill = "black",
                                   shape = 21,
                                   point_size = 3,
                                   hide_color_legend = FALSE,
                                   xlim = NULL,
                                   ylim = NULL,
                                   custom_theme = NULL,
                                   ...) {
  plot_each_species(
    census = object,
    elevation = NULL,
    species = species,
    fill = fill,
    shape = shape,
    point_size = point_size,
    hide_color_legend = hide_color_legend,
    xlim = xlim,
    ylim = ylim,
    custom_theme = custom_theme
  )
}

#' Generic function to create a list of autoplots by species.
#'
#' `autoplot_by_species` uses __ggplot2__ to create a list of plots by species
#' for an object of a particular class in a single command. It extends the
#' S3 generic defined by [ggplot2::autoplot()].
#'
#' @param object An object of supported S3 class.
#' @param ... Other arguments passed to specific methods.
#'
#' @seealso [ggplot2::autoplot()].
#'
#' @template return_a_list_of_ggplots
#'
#' @family generics for ForestGEO classes
#' @keywords internal
#' @export
autoplot_by_species <- function(object, ...) {
  UseMethod("autoplot_by_species")
}

autoplot_by_species.default <- function(object, ...) {
  abort(glue("
    Can't deal with data unless it is of class 'sp' or 'sp_elev'.
    Do you forget to use `sp()` or `sp_elev()`?
  "))
}
