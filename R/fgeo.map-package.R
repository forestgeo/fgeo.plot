#' @keywords internal
"_PACKAGE"

# Import external functions and avoid R CMD check note --------------------

# Not importing dplyr::filter() because it may conflict with stats::filter()
#' @importFrom dplyr group_by ungroup mutate select
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @importFrom stats setNames
#' @importFrom dplyr %>%
NULL

# Flag inline helpers as global variables so R CMD check doesn't warn
utils::globalVariables(c(".data", "gx", "gy", "qx", "qy", "status_tree",
  "subquadrat", "elev", "..level.."))
