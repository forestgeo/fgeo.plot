#' Quick habitat plots.
#' 
#' @param object An object objects of class "fgeo_habitat" 
#'   (\url{http://bit.ly/fgeo-reference}).
#' @param ... Other arguments passed to methods.
#'
#' @return An object of class "ggplot".
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("fgeo", quietly = TRUE)) {

#'   library(fgeo)
#'   
#'   habitats <- fgeo_habitat(fgeo.x::elevation, gridsize = 20, n = 4)
#'   autoplot(habitats)
#' } 
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
