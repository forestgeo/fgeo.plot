#' Define data containing variables `sp` (species) and/or `elev` (elevation).
#' 
#' * `sp()` takes a dataset with variables `gx`, `gy`, and `sp`.
#' * `elev()` takes a dataset with variables `gx`, `gy`, and `elev`.
#' * `sp_elev()` takes two dataset: 
#'     * The first one with variables `gx`, `gy`, and `sp`.
#'     * A second one with variables `gx`, `gy`, and `elev`.
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
#' 
sp_elev <- function(sp, elev) {
  validate_sp_elev(sp, elev)
  cns_elev <- list(sp = sp, elev = elev)
  structure(cns_elev, class = c("sp_elev", class(cns_elev)))
}

#' @export
#' @noRd
print.sp_elev <- function(x, ...) {
  print(unclass(x))
  invisible(x)
}

#' @rdname sp_elev
#' @export
sp <- function(sp) {
  structure(sp, class = c("sp", class(sp)))
}

#' @rdname sp_elev
#' @export
elev <- function(elev) {
  structure(elev, class = c("elev", class(elev)))
}

validate_sp_elev <- function(sp, elev) {
  stopifnot(is.data.frame(fgeo.tool::fgeo_elevation(elev)))
  
  stopifnot(is.data.frame(sp))
  fgeo.base::check_crucial_names(sp, c("gx", "gy", "sp"))
}
