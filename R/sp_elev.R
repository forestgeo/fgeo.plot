#' Allow autoplotting the column `sp`.
#'
#' @param sp A ForestGEO-like dataframe with the column `sp`.
#'
#' @seealso [autoplot.sp()].
#'
#' @return An S3 object of class 'sp'.
#'
#' @examples
#' assert_is_installed("fgeo.x")
#' 
#' inherits(sp(fgeo.x::stem5), "sp")
#' @family plot functions
#' @family autoplots
#' @family functions to construct fgeo classes
#' @family functions to plot species
#' @export
sp <- function(sp) {
  stopifnot(is.data.frame(sp))
  structure(sp, class = c("sp", class(sp)))
}

#' Allow autoplotting the column `elev`.
#'
#' @param elev A ForestGEO-like elevation list or its `col` dataframe (with
#'   the column `elev`).
#'
#' @seealso [autoplot.elev()].
#'
#' @return An S3 object of class 'elev'.
#'
#' @examples
#' assert_is_installed("fgeo.x")
#' 
#' inherits(elev(fgeo.x::elevation), "elev")
#' inherits(elev(fgeo.x::elevation$col), "elev")
#' @family plot functions
#' @family autoplots
#' @family functions to construct fgeo classes
#' @family functions to plot elevation
#' @export
elev <- function(elev) {
  stopifnot(is.list(elev))
  structure(elev, class = c("elev", class(elev)))
}

#' Allow autoplotting the columns `sp` and `elev`.
#'
#' @param sp A ForestGEO-like dataframe with the column `sp`.
#' @param elev A ForestGEO-like elevation list or its `col` dataframe -- with
#'   the column `elev`.
#'
#' @seealso [autoplot.sp_elev()].
#'
#' @return An S3 object of class 'sp_elev'.
#'
#' @examples
#' assert_is_installed("fgeo.x")
#' 
#' species_elevation <- sp_elev(fgeo.x::stem5, fgeo.x::elevation)
#' inherits(species_elevation, "sp_elev")
#' @family plot functions
#' @family autoplots
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
