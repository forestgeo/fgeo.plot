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
#'   the column `elev`. The datasets you pass to `sp` and `elev` should come
#'   from the same forest plot. This is not compulsory but not doing so is most
#'   likely a mistake.
#'
#' @seealso [autoplot.sp_elev()].
#'
#' @return An S3 object of class 'sp_elev'.
#'
#' @examples
#' assert_is_installed("fgeo.x")
#'
#' species_from_luquillo <- fgeo.x::stem5
#' elevation_from_luquillo <- fgeo.x::elevation
#'
#' species_elevation <- sp_elev(species_from_luquillo, elevation_from_luquillo)
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
  stopifnot(is.data.frame(sp))
  check_crucial_names(sp, c("gx", "gy", "sp"))

  if (!is.null(elev)) {
    elev <- fgeo.tool::fgeo_elevation(elev)
    stopifnot(is.data.frame(elev))
    warn_if_plotdim_are_different(sp, elev)
  }

  invisible(list(sp = sp, elev = elev))
}

warn_if_plotdim_are_different <- function(sp, elev) {
  plotdim_elev <- purrr::quietly(fgeo.tool::guess_plotdim)(elev)
  plotdim_sp <- purrr::quietly(fgeo.tool::guess_plotdim)(sp)

  identical_plotdim <- !identical(plotdim_sp$result, plotdim_elev$result)
  if (identical_plotdim) {
    warn(glue("
      The datasets passed to `sp` and `elev` seem to have different dimensions.
      Are you sure they come from the same forest plot? (see `?sp_elev()`)
      * `sp` {rm_brake(plotdim_sp$messages)}
      * `elev` {rm_brake(plotdim_elev$messages)}
    "))
  }

  invisible(list(sp = sp, elev = elev))
}

rm_brake <- function(x) {
  sub("\n", "", x)
}

#' @export
#' @noRd
print.sp_elev <- function(x, ...) {
  print(unclass(x))
  invisible(x)
}
