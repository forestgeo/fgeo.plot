#' Quick plot of species distribution.
#' 
#' @description
#' Automatically plot the `sp` variable of a ForestGEO-like dataset of class
#' 'sp'. You can create a 'sp' `object` with:
#' @description
#' ```
#' object <- sp(DATA-WITH-VARIABLE-sp)
#' ```
#' @description
#' See sections __Usage__ and __Examples__.
#' 
#' @template compare_ggplot2
#' 
#' @param object An object created with `sp()`.
#' @param fill Character; either a colour or "sp", which maps each species to a
#'   different color.
#' @template shape_point_size
#' @param facet (Not available for `plot_each_species()`) Logical; `TRUE` wraps
#'   multiple maps within the area of a single graphic plot.
#' @param hide_color_legend Logical; `TRUE` hides the color legend.
#' @inheritParams axis_limits
#' @param custom_theme A valid [ggplot2::theme()]. `NULL` uses the default
#'   theme [theme_default()].
#'   
#' @seealso [autoplot()], [sp()], [add_species()].
#' @family autoplots
#'
#' @return A "ggplot".
#'
#' @export
#' @examples
#' # Small dataset with a few species for quick examples
#' tree <- subset(fgeo.data::luquillo_tree5_random, sp %in% c("PREMON", "CASARB"))
#' autoplot(sp(tree))
#' 
#' # Customize
#' autoplot(sp(tree), point_size = 1)
autoplot.sp <- function(object, 
                        fill = "black",
                        shape = 21,
                        point_size = 3,
                        facet = TRUE,
                        hide_color_legend = FALSE,
                        xlim = NULL,
                        ylim = NULL,
                        custom_theme = NULL) {
  plot_sp_elev(
    census = object,
    elevation = NULL,
    fill = fill,
    shape = shape,
    point_size = point_size,
    facet = facet,
    hide_color_legend = hide_color_legend,
    xlim = xlim,
    ylim = ylim,
    custom_theme = custom_theme
  )
}

#' Quick plot of species distribution.
#' 
#' @description
#' Automatically plot the `elev` variable of a ForestGEO-like dataset of class 
#' 'elev'. You can create an 'elev' `object` with:
#' @description
#' ```
#' object <- elev(DATA-WITH-VARIABLE-elev)
#' ```
#' @description
#' See sections __Usage__ and __Examples__.
#' 
#' @template compare_ggplot2
#' 
#' @param object An object created with `elev()`. 
#' @inheritParams add_elevation_contours
#' @param hide_color_legend Logical; `TRUE` hides the color legend.
#' @inheritParams add_elevation_labels
#' @inheritParams axis_limits
#' @param custom_theme A valid [ggplot2::theme()]. `NULL` uses the default
#'   theme [theme_default()].
#'
#' @seealso [autoplot()], [elev()], [add_elevation_contours].
#' @family autoplots
#'
#' @return A "ggplot".
#'
#' @export
#' @examples
#' elevation_list <- fgeo.data::luquillo_elevation
#' autoplot(elev(elevation_list))
#' # Same
#' elevation_dataframe <- elevation_list$col
#' p <- autoplot(elev(elevation_dataframe))
#' p
#' 
#' # Customize
#' hide_color_legend(p)
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
                          custom_theme = NULL) {
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

#' Quick plot of species distribution and topography.
#' 
#' @description
#' Automatically plot the `sp` and `elev` variables of a ForestGEO-like
#' dataset of class 'sp_elev'. You can create a 'sp_elev' `object` with:
#' @description
#' ``` 
#' object <- sp_elev(DATA-WITH-VARIABLE-sp, DATA-WITH-VARIABLE-elev)
#' ```
#' @description
#' See sections __Usage__ and __Examples__.
#' 
#' @template compare_ggplot2
#' 
#' @param object An object created with `sp_elev()`.
#' @inheritParams autoplot.sp
#' @inheritParams autoplot.elev
#'
#' @seealso [autoplot()], [sp_elev()].
#' @family autoplots
#'
#' @return A "ggplot".
#'
#' @export
#' @examples
#' # Small dataset with a few species for quick examples
#' tree <- subset(fgeo.data::luquillo_tree5_random, sp %in% c("PREMON", "CASARB"))
#' elevation_list <- fgeo.data::luquillo_elevation
#' 
#' autoplot(sp_elev(tree, elevation_list))
#' 
#' # Same
#' elevation_dataframe <- elevation_list$col
#' autoplot(sp_elev(tree, elevation_dataframe))
#' 
#' # Customize
#' p <- autoplot(sp_elev(tree, elevation_dataframe), fill = "red")
#' p
#' hide_color_legend(p)
#' 
#' @examples 
#' # Small dataset with a few species for quick examples
#' some_sp <- c("PREMON", "CASARB")
#' census <- subset(fgeo.data::luquillo_tree5_random, sp %in% some_sp)
#' elevation <- fgeo.data::luquillo_elevation
#' 
#' autoplot(sp_elev(census, elevation))
autoplot.sp_elev <- function(object, 
                             fill = "black",
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
                             custom_theme = NULL) {
  plot_sp_elev(
    census = object[[1]], 
    elevation = object[[2]], 
    fill = fill,
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

autoplot.default <- function(object, ...) {
  abort(glue("
    Can't deal with data unless it is of class 'sp', 'elev' or 'sp_elev'.
    Do you forget to use `sp()`, `elev()` or `sp_elev()`?
    "))
}
