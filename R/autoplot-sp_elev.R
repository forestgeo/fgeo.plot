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
#' @param object An object created with `sp()`.
#' @param ... Other arguments passed to `add_species()`.
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
autoplot.sp <- function(object, ...) {
  add_species(plot_base(object), ...)
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
#' @param object An object created with `elev()`. 
#' @param ... Other arguments passed to `add_elevation_contours()`.
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
#' autoplot(elev(elevation_dataframe))
#' 
#' # Customize
#' p <- autoplot(elev(elevation_dataframe), fill = "red")
#' p
#' hide_color_legend(p)
autoplot.elev <- function(object, ...) {
  add_elevation_contours(plot_base(object), ...)
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
#' @param object An object created with `sp_elev()`.
#' @param ... Other arguments passed to `plot_species_or_elevation()`.
#'
#' @seealso [autoplot()], [sp_elev()], [plot_species_or_elevation()].
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
autoplot.sp_elev <- function(object, ...) {
  plot_species_or_elevation(census = object[[1]], elevation = object[[2]], ...)
}
