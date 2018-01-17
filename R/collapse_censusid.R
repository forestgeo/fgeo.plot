#' Collapse the values of CensusID. 
#' 
#' This function is useful to summarize the status history of a tree tag accross
#' censuses. It is particularly useful to avoid duplicated tree tags on maps.
#'
#' @param x A dataframe with the variable CensusID
#'
#' @return A modified version of `x`, most likely with less rows.
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' x <- tibble::tribble(
#'   ~CensusID, ~Tag, ~Status,
#'           1,    1, "alive",
#'           1,    1,  "dead",
#'           1,    2,  "dead",
#'           1,    2,  "dead",
#'           2,    1, "alive",
#'           2,    1, "alive",
#'           2,    2, "alive",
#'           2,    2,  "dead"
#' )
#' 
#' collapse_censusid(x)
#' 
#' with_status_tree <- fgeo.tool::add_status_tree(x, "alive", "dead")
#' reduced <- unique(select(with_status_tree, -Status))
#' reduced
#' 
#' collapse_censusid(reduced)
collapse_censusid <- function(x) {
  stopifnot(is.data.frame(x))
  x <- fgeo.tool::nms_lowercase(x)
  fgeo.tool::check_crucial_names(x, "censusid")

  x$censusid <- collapse(
    sort(unique(x$censusid))
  )

  unique(fgeo.tool::nms_restore(x))
}

