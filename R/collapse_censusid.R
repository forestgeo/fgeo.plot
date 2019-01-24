#' Collapse the values of CensusID.
#'
#' This function is useful to summarize the status history of a tree tag across
#' censuses. It is particularly useful to avoid duplicated tree tags on plots.
#'
#' @param x A dataframe -- specifically a ViewFullTable.
#'
#' @return A modified version of `x`, most likely with less rows.
#'
#' @examples
#' x <- data.frame(
#'   CensusID = c(1, 1, 1, 1, 2, 2, 2, 2),
#'   Tag = c(1, 1, 2, 2, 1, 1, 2, 2),
#'   Status = c("alive", "dead", "dead", "dead", "alive", "alive", "alive", "dead"),
#'   stringsAsFactors = FALSE
#' )
#' x
#' collapse_censusid(x)
#' @family functions to edit ForestGEO data in place
#' @noRd
collapse_censusid <- function(x) {
  stopifnot(is.data.frame(x))

  old <- names(x)
  names(x) <- tolower(old)
  check_crucial_names(x, "censusid")

  x$censusid <- commas(sort(unique(x$censusid)))

  x <- stats::setNames(x, old)
  unique(x)
}
