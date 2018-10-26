#' List plots of species distribution and topography (good for pdf output).
#' 
#' @description
#' This function extends the functionality of [autoplot.sp()] and 
#' [autoplot.sp()] and returns not a single plot but a list of plots. It is
#' ideal to print a .pdf file with one plot per page.
#' 
#' @description
#' Automatically plots the variables `sp` and optionally `elev` of a 
#' ForestGEO-like dataset of class 'sp' or sp_elev'.
#' @description
#' ``` 
#' Create a 'sp' `object` with:
#' object <- sp(DATA-WITH-VARIABLE-sp)
#' 
#' Create a 'sp_elev' `object` with:
#' object <- sp_elev(DATA-WITH-VARIABLE-sp, DATA-WITH-VARIABLE-elev)
#' ```
#' @description
#' See sections __Usage__ and __Examples__.
#' 
#' @template compare_ggplot2
#' 
#' @param object An object created with  [sp()] or [sp_elev()].0
#' 
#' @inheritParams autoplot.sp
#' @inheritParams autoplot.elev
#'
#' @seealso [autoplot()], [sp()], [sp_elev()].
#' @family autoplots
#' @family functions to create a list of plots
#' @param species A character vector. Each element of the vector must be the 
#'   code for one species in the column `sp`. This function will produce as 
#'   many maps as elements in this vector. The string "all" is a shortcut to 
#'   map all unique codes in the column `sp`.
#' @param ... Other arguments passed to methods.
#' @examples
#' # Small dataset with a few species for quick examples
#' some_sp <- c("PREMON", "CASARB")
#' census <- subset(fgeo.data::luquillo_tree5_random, sp %in% some_sp)
#' elevation <- fgeo.data::luquillo_elevation
#' 
#' plot_by_species(sp(census))
#' plot_by_species(sp_elev(census, elevation))
plot_by_species <- function(object, ...) {
  UseMethod("plot_by_species")
}

plot_by_species.default <- function(object, ...) {
  abort(glue("
    Can't deal with data unless it is of class 'sp' or 'sp_elev'.
    Do you forget to use `sp()` or `sp_elev()`?
  "))
}

#' @export
#' @rdname plot_by_species
plot_by_species.sp <- function(object, ...) {
  plot_each_species(census = object, elevation = NULL, ...)
}

#' @export
#' @rdname plot_by_species
plot_by_species.sp_elev <- function(object, ...) {
  plot_each_species(census = object[[1]], elevation = object[[2]], ...)
}
