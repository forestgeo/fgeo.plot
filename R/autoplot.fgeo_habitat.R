#' Plot objects of class "fgeo_habitat".
#' 
#' @param object An object objects of class "fgeo_habitat" (see 
#'   `?fgeo::fgeo_habitat`).
#' @param ... Other arguments passed to methods.
#'
#' @return An object of class "ggplot".
#' @export
#' 
#' @family autoplots
#'
#' @examples
#' if (requireNamespace("fgeo")) {
#'   library(fgeo)
#'   
#'   habitats <- fgeo_habitat(fgeo.x::elevation, gridsize = 20, n = 4)
#'   autoplot(habitats)
#' } 
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
