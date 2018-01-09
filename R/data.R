#' Data sets for examples; adapted from [bciex::bci12vft_mini].
#' @name datasets
#' @examples
#' library(dplyr)
#' all_quad <- bciex::bci12vft_mini %>%
#'   dplyr::rename(QX = x, QY = y) %>%
#'   dplyr::filter(PlotID == 1) %>%
#'   fgeo.utils::top(CensusID)
#'
#' top4quad <- all_quad %>% fgeo.utils::top(QuadratID, 4)
#' identical(top4quad, fgeo.map::top4quad)
#'
#' top1quad <- all_quad %>% fgeo.utils::top(QuadratID, 1)
#' identical(top1quad, fgeo.map::top4quad)
#'
#' top1quad
NULL

#' @rdname datasets
"top4quad"

#' @rdname datasets
"top1quad"
