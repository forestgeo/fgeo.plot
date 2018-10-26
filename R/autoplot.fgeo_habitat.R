#' Plot objects of class "fgeo_habitat".
#' 
#' @param object An S3 object of class "fgeo_habitat".
#' @param ... Other arguments passed to methods.
#'
#' @return An object of class "ggplot".
#' @export
#' 
#' @family autoplots
#'
#' @examples
#' elev_list <- fgeo.data::luquillo_elevation
#' habitats <- fgeo.tool::fgeo_habitat(elev_list, gridsize = 20, n = 4)
#' 
#' autoplot(habitats)
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
