#' Transform data to `autoplot()` the variable `sp`.
#' 
#' @param sp A ForestGEO-like dataframe with the column `sp`.
#' 
#' @seealso [autoplot.sp()].
#' @family functions to construct fgeo classes
#' @family functions to plot species
#' 
#' @return An S3 object of class 'sp'.
#' 
#' @export
#' @examples
#' class(sp(fgeo.data::luquillo_stem5_random))
sp <- function(sp) {
  structure(sp, class = c("sp", class(sp)))
}

#' Transform data to `autoplot()` the variable `elev`.
#' 
#' @param elev A ForestGEO-like elevation list or its `col` dataframe -- with
#'   the variable `elev`.
#'   
#' @seealso [autoplot.elev()].
#' @family functions to construct fgeo classes
#' @family functions to plot elevation
#' 
#' @return An S3 object of class 'elev'.
#' 
#' @export
#' @examples
#' class(elev(fgeo.data::luquillo_elevation))
#' # Same
#' class(elev(fgeo.data::luquillo_elevation$col))
elev <- function(elev) {
  structure(elev, class = c("elev", class(elev)))
}

#' Transform data to `autoplot()` the variables `sp` and `elev`.
#' 
#' @param sp A ForestGEO-like dataframe with column the column `sp`.
#' @param elev A ForestGEO-like elevation list or its `col` dataframe -- with
#'   the column `elev`.
#' 
#' @seealso [autoplot.sp_elev()].
#' @family functions to construct fgeo classes
#' @family functions to plot elevation
#' @family functions to plot species
#' 
#' @return An S3 object of class 'sp_elev'.
#' 
#' @export
#' @examples
#' species_elevation <- sp_elev(
#'   fgeo.data::luquillo_stem5_random, fgeo.data::luquillo_elevation
#' )
#' class(species_elevation)
sp_elev <- function(sp, elev = NULL) {
  validate_sp_elev(sp, elev)
  cns_elev <- list(sp = sp, elev = elev)
  structure(cns_elev, class = c("sp_elev", class(cns_elev)))
}

validate_sp_elev <- function(sp, elev) {
  if (!is.null(elev)) {
    stopifnot(is.data.frame(fgeo.tool::fgeo_elevation(elev)))
  }
  
  stopifnot(is.data.frame(sp))
  fgeo.base::check_crucial_names(sp, c("gx", "gy", "sp"))
}

#' @export
#' @noRd
print.sp_elev <- function(x, ...) {
  print(unclass(x))
  invisible(x)
}
