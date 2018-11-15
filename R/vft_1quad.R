#' Small ViewFullTables from Luquillo.
#'
#' One quadrat of a ViewFullTable from Luquillo.
#' 
#' @return One quadrat of a ViewFullTable dataframe.
#' @export
#'
#' @seealso `fgeo.data::luquillo_vft_4quad`
#' 
#' @family datasets
#' 
#' @examples
#' str( vft_1quad())
vft_1quad <- function() {
  fgeo.x::vft_4quad %>% 
  fgeo.tool::pick_top(.data$CensusID) %>% 
  fgeo.tool::pick_top(.data$QuadratID, 1)
}
