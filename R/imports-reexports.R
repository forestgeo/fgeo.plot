# Import external functions and avoid R CMD check note --------------------

#' @importFrom dplyr group_by ungroup mutate select
#' @importFrom fgeo.tool is_multiple flag_if check_crucial_names
#' @importFrom ggrepel geom_text_repel
#' @importFrom glue glue
#' @importFrom rlang abort warn inform
#' @importFrom stats setNames
#' @importFrom utils head tail
#' @import ggplot2
NULL

#' @export
ggplot2::autoplot

# Flag inline helpers as global variables so R CMD check doesn't warn
utils::globalVariables(c(".data", "gx", "gy", "qx", "qy", "status_tree",
  "subquadrat", "elev", "..level..", "sp", "level"))

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
