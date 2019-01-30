#' Plot habitats.
#'
#' @param object An object of class "fgeo_habitat" (see `fgeo_habitat` at
#'   \url{http://bit.ly/fgeo-reference}).
#' @template autoplot_unused_dots
#'
#' @return An object of class "ggplot".
#'
#' @examples
#' \dontrun{
#' assert_is_installed("fgeo.x")
#' assert_is_installed("fgeo.analyze")
#' library(fgeo.analyze)
#' 
#' habitats <- fgeo_habitat(fgeo.x::elevation, gridsize = 20, n = 4)
#' autoplot(habitats)
#' }
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
