#' Pick ViewFullTables to create small example datasets.
#' 
#' This function helps you to create small datasets for examples. It defaults to
#' returning the input ViewFullTable "as is", but provides arguments to pick a
#' specific number of censuses, quadrats and rows.
#'
#' @return A tibble (dataframe).
#' 
#' @examples
#' # Same
#' dim(fgeo.x::vft_4quad)
#' dim(pick_vft_xxx(fgeo.x::vft_4quad))
#' 
#' dim(
#'   pick_vft(
#'     fgeo.x::vft_4quad, 
#'     n_censuses = 2, 
#'     n_quadrats = 2, 
#'     n_rows = 50
#'  )
#' )
#' @family datasets
#' @export
pick_vft_xxx <- function(.data, 
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

pick_vft <- function(.data, n_censuses = 1, n_quadrats = 1, n_rows = NULL) {
  result <- .data %>% 
    pick_top(.data$CensusID, n_censuses) %>% 
    pick_top(.data$QuadratID, n_quadrats)
  
  if (is.null(n_rows)) {
    return(result)
  }
  
  dplyr::sample_n(result, n_rows)
}
