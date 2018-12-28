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

#' @export
#' @rdname vft_1quad
pick_vft <- function(.data = fgeo.x::vft_4quad, 
                     n_censuses = 1, 
                     n_quadrats = 1,
                     n_rows = NULL) {
  result <- .data %>% 
    pick_top(.data$CensusID, n_censuses) %>% 
    pick_top(.data$QuadratID, n_quadrats)
  
  if (is.null(n_rows)) {
    return(result)
  }
  
  dplyr::sample_n(result, n_rows)
}
