#' Generic function to create a list of autoplots by species.
#'
#' @param object An object of supported S3 class.
#' @param ... Other arguments passed to methods.
#'
#' @return A list of 'ggplots'.
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

#' List plots of species distribution (good for pdf output).
#' 
#' @description
#' This function extends the functionality of [autoplot.sp()] and returns not a
#' single plot but a list of plots. It is ideal to print a .pdf file with one
#' plot per page.
#' 
#' @description
#' Automatically plot the variable `sp` of a ForestGEO-like dataset of class
#' 'sp'.
#' @description
#' ``` 
#' Create a 'sp' `object` with:
#' object <- sp(DATA-WITH-VARIABLE-sp)
#' ```
#' @description
#' See sections __Usage__ and __Examples__.
#' 
#' @template compare_ggplot2
#' 
#' @param object An object created with  [sp()].
#' 
#' @inheritParams autoplot.sp
#' @param species A character vector. Each element of the vector must be the 
#'   code for one species in the column `sp`. This function will produce as 
#'   many maps as elements in this vector. The string "all" is a shortcut to 
#'   map all unique codes in the column `sp`.
#' @param ... Other arguments passed to methods.
#' 
#' @seealso [autoplot()], [sp()].
#' @family functions to create a list of plots
#' @family functions to plot species
#' 
#' @export
#' @examples
#' # Small dataset with a few species for quick examples
#' some_sp <- c("PREMON", "CASARB")
#' census <- subset(fgeo.data::luquillo_tree5_random, sp %in% some_sp)
#' 
#' autoplot_by_species(sp(census))
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

#' List plots of species distribution and topography (good for pdf output).
#' 
#' @description
#' This function extends the functionality of [autoplot.sp()] and 
#' [autoplot.elev()] and returns not a single plot but a list of plots. It is
#' ideal to print a .pdf file with one plot per page.
#' 
#' @description
#' Automatically plots the variables `sp` and `elev` of a ForestGEO-like dataset
#' of class 'sp_elev'.
#' @description
#' ``` 
#' Create a 'sp_elev' `object` with:
#' object <- sp_elev(DATA-WITH-VARIABLE-sp, DATA-WITH-VARIABLE-elev)
#' ```
#' @description
#' See sections __Usage__ and __Examples__.
#' 
#' @template compare_ggplot2
#' 
#' @param object An object created with [sp_elev()].
#' 
#' @inheritParams autoplot.sp
#' @inheritParams autoplot.elev
#' @inheritParams autoplot_by_species.sp
#' @param ... Other arguments passed to methods.
#' 
#' @seealso [autoplot()], [sp()], [sp_elev()].
#' @family functions to create a list of plots
#' @family functions to plot elevation
#' @family functions to plot species
#' 
#' @export
#' @examples
#' # Small dataset with a few species for quick examples
#' some_sp <- c("PREMON", "CASARB")
#' census <- subset(fgeo.data::luquillo_tree5_random, sp %in% some_sp)
#' elevation <- fgeo.data::luquillo_elevation
#' 
#' autoplot_by_species(sp_elev(census, elevation))
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
