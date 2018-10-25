#' Quick plots of species distribution and/or topography.
#' 
#' These `autoplot()`s work in combination with functions that let you specify
#' which are the primary variable(s) you want to plot. Use these templates:
#' * `autoplot(sp(<data with variable sp>))` to plot species distribution.
#' * Use `autoplot(elev(<data with variable elev>))` to plot topography.
#' * Use `autoplot(sp_elev(<data with variable sp>, <data with variable elev>))`
#' to plot species distribution and topography.
#' 
#' @param object A function call with one of these formats: 
#'     * `sp(<data with variable sp>)`.
#'     * `elev(<data with variable elev>)`.
#'     * `sp_elev(<data with variable sp>, <data with variable elev>)`.
#'     
#' @param ... Other arguments passed to methods:
#'   * `sp(<sp-data>)` passes `...` to `add_species()`.
#'   * `elev(<elev-data>)` passes `...` to `add_elevation_contours()`.
#'   * `sp_elev(<sp-data>, <elev-data>)` passes `...` to
#'   `plot_species_or_elevation()`.
#'   
#' @seealso [add_sp()], [add_elevation_controus()], [plot_species_or_elevation()].
#' 
#'
#' @return An object of class "ggplot".
#' @export
#' 
#' @family autoplots
#'
#' @examples
#' # Small dataset with a few species for quick examples
#' tree <- subset(fgeo.data::luquillo_tree5_random, sp %in% c("PREMON", "CASARB"))
#' autoplot(sp(tree))
#' # Customize
#' autoplot(sp(tree), point_size = 1, shape = 1)
#' 
#' elevation_list <- fgeo.data::luquillo_elevation
#' autoplot(elev(elevation_list))
#' # Same
#' elevation_df <- elevation_list$col
#' autoplot(elev(elevation_list))
#' 
#' autoplot(sp_elev(tree, elevation_df))
#' 
autoplot.sp <- function(object, ...) {
  add_species(plot_base(object), ...)
}

autoplot.elev <- function(object, ...) {
  add_elevation_contours(plot_base(object), ...)
}

#' @export
autoplot.sp_elev <- function(object, ...) {
  plot_species_or_elevation(census = object[[1]], elevation = object[[2]], ...)
}
