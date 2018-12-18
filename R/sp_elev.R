#' Enable autoplotting the variable `sp`.
#' 
#' @param sp A ForestGEO-like dataframe with the column `sp`.
#' 
#' @seealso [autoplot.sp()].
#' 
#' @return An S3 object of class 'sp'.
#' 
#' @examples
#' class(sp(fgeo.x::stem5))
#' 
#' @family functions to construct fgeo classes
#' @family functions to plot species
#' @export
sp <- function(sp) {
  stopifnot(is.data.frame(sp))
  structure(sp, class = c("sp", class(sp)))
}

#' Enable autoplotting the variable `elev`.
#' 
#' @param elev A ForestGEO-like elevation list or its `col` dataframe -- with
#'   the variable `elev`.
#'   
#' @seealso [autoplot.elev()].
#' 
#' @return An S3 object of class 'elev'.
#' 
#' @examples
#' class(elev(fgeo.x::elevation))
#' # Same
#' class(elev(fgeo.x::elevation$col))
#' 
#' @family functions to construct fgeo classes
#' @family functions to plot elevation
#' @export
elev <- function(elev) {
  stopifnot(is.list(elev))
  structure(elev, class = c("elev", class(elev)))
}

#' Enable autoplotting the variables `sp` and `elev`.
#' 
#' @param sp A ForestGEO-like dataframe with column the column `sp`.
#' @param elev A ForestGEO-like elevation list or its `col` dataframe -- with
#'   the column `elev`.
#' 
#' @seealso [autoplot.sp_elev()].
#' 
#' @return An S3 object of class 'sp_elev'.
#' 
#' @examples
#' species_elevation <- sp_elev(
#'   fgeo.x::stem5, fgeo.x::elevation
#' )
#' class(species_elevation)
#' 
#' @family functions to construct fgeo classes
#' @family functions to plot elevation
#' @family functions to plot species
#' @export
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
  check_crucial_names(sp, c("gx", "gy", "sp"))
}

#' @export
#' @noRd
print.sp_elev <- function(x, ...) {
  print(unclass(x))
  invisible(x)
}
