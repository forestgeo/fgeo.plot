#' Pick some censuses, quadrats and rows from a ViewFullTable.
#' 
#' This function helps you to pick some rows of a ForestGEO ViewFullTable. It is
#' useful to create small datasets for examples. It defaults to returning the
#' input ViewFullTable "as is", but provides arguments to pick a specific number
#' of censuses, quadrats and rows. 
#' 
#' @param .data A ViewFullTable
#' @param n_censuses,n_quadrats Number of censuses and quadrats to pick from the
#'   front (if positive) or back (if negative) of the unique alpha-sorted values
#'   of `CensusID` and `QuadratID`, respectively (see examples below).
#' @param n_rows Number of rows to sample at random -- after picking
#'   `n_censuses` and `n_quadrats`.
#'
#' @return A tibble (dataframe).
#' 
#' @examples
#' dim(fgeo.x::vft_4quad)
#' # Same
#' dim(pick_vft(fgeo.x::vft_4quad))
#' 
#' dim(pick_vft(fgeo.x::vft_4quad, n_rows = 50))
#' 
#' sort(unique(fgeo.x::vft_4quad$QuadratID))
#' 
#' # Pick from the front
#' unique(pick_vft(fgeo.x::vft_4quad, n_quadrats = 1)$QuadratID)
#' 
#' # Pick from the back
#' unique(pick_vft(fgeo.x::vft_4quad, n_quadrats = -2)$QuadratID)
#' 
#' @family functions to pick or drop rows of a ForestGEO dataframe
#' @family functions for fgeo vft
#' @export
pick_vft <- function(.data, 
                     n_censuses = NULL, 
                     n_quadrats = NULL, 
                     n_rows = NULL) {
  result <- .data %>% 
    pick_top_if_n_is_not_null(var = .data$CensusID, n = n_censuses) %>% 
    pick_top_if_n_is_not_null(var = .data$QuadratID, n = n_quadrats)
  
  if (is.null(n_rows)) {
    return(result)
  }
  
  dplyr::sample_n(result, n_rows)
}

pick_top_if_n_is_not_null <- function(.data, var, n = NULL) {
  if (is.null(n)) {
    return(.data)
  }
  
  pick_top(.data, !! enquo(var), n)
}
