#' Small ViewFullTables from Luquillo.
#'
#' One quadrat of a ViewFullTable from Luquillo.
#' 
#' @return One quadrat of a ViewFullTable dataframe.
#' 
#' @examples
#' str(vft_1quad())
#' 
#' @family datasets
#' @export
vft_1quad <- function() {
  fgeo.x::vft_4quad %>% 
  pick_top(.data$CensusID) %>% 
  pick_top(.data$QuadratID, 1)
}
