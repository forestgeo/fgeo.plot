#' Plot habitats.
#'
#' @param object An object of class "fgeo_habitat" (see `fgeo_habitat` at
#'   \url{https://forestgeo.github.io/fgeo/articles/siteonly/reference.html}).
#' @template autoplot_unused_dots
#'
#' @return An object of class "ggplot".
#'
#' @examples
#' assert_is_installed("fgeo.x")
#' habitats <- fgeo.x::habitat
#' autoplot(habitats)
#' @family plot functions
#' @family autoplots
#' @export
autoplot.fgeo_habitat <- function(object, ...) {
  # Use quadrats' center instead of quadrats' bottom-left corner
  gridsize <- fgeo.tool::extract_gridsize(object)
  object$gx <- object$gx + (gridsize / 2)
  object$gy <- object$gy + (gridsize / 2)
  object$habitats <- as.factor(object$habitats)

  ggplot(object, aes(x = gx, y = gy)) +
    geom_raster(aes(fill = .data$habitats)) +
    coord_fixed() +
    labs(fill = "habitats")
}
