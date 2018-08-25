# Import external functions and avoid R CMD check note --------------------

#' @import ggplot2
#' @importFrom dplyr group_by ungroup mutate select %>% 
#' @importFrom rlang abort warn inform
#' @importFrom ggrepel geom_text_repel
#' @importFrom stats setNames
#' @importFrom fgeo.base is_multiple flag_if
NULL

#' @export
dplyr::`%>%`

# Flag inline helpers as global variables so R CMD check doesn't warn
utils::globalVariables(c(".data", "gx", "gy", "qx", "qy", "status_tree",
  "subquadrat", "elev", "..level..", "sp", "level"))
