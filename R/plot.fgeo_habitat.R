#' Plot objects of class "fgeo_habitat".
#'
#' @param x An S3 object of class "fgeo_habitat".
#' @param ... Other arguments passed to methods.
#'
#' @return An object of class "ggplot".
#' @export
#'
#' @examples
#' \dontrun{
#' library(fgeo.tool)
#' elev_list <- fgeo.data::luquillo_elevation
#' habitats <- fgeo_habitat(elev_list, gridsize = 20, n = 4)
#' plot(habitats)
#' }
plot.fgeo_habitat <- function(x, ...) {
  # Use quadrats' center instead of quadrats' bottom-left corner
  gridsize <- fgeo.tool::extract_gridsize(x)
  x$gx <- x$gx + (gridsize / 2)
  x$gy <- x$gy + (gridsize / 2)
  
  x$habitats <- as.factor(x$habitats)
  
  ggplot(x, aes(x = gx, y = gy)) +
    geom_raster(aes(fill = .data$habitats)) + 
    coord_fixed() +
    labs(fill = "habitats")
}
