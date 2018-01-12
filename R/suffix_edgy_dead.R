#' Suffix tags of dead stems that map beyond the quadrat's edge.
#'
#' @param x A ForestGEO table, either a ViewFullTable or a census table.
#' @template status_d
#' @param suffix A character string to suffix tree tags with.
#' @param x_q,y_q Integer; the x and y size in meters of a quadrat.
#'
#' @return A modified version of `x`.
#' @export
#'
#' @examples
#' stem <- bciex::bci12s7mini
#' # Add x and y coordinates relative to each quadrat
#' with_lxly <- fgeo.tool::add_lxly(stem)
#' # Focus on dead stems
#' dead <- with_lxly[with_lxly$status == "D", ]
#' 
#' # Suffix only dead trees that spillover beyond `10`
#' spillover <- 10
#' relevant_vars <- c("tag", "lx", "ly")
#' suffixed_over10 <- suffix_edgy_dead(dead, "D", "*", x_q = spillover)
#' suffixed_over10[1:10, relevant_vars]
#' 
#' # To tag all dead trees, set spillover to 0
#' spillover <- 0
#' suffixed_all <- suffix_edgy_dead(dead, "D", "*", x_q = spillover)
#' suffixed_all[1:10, relevant_vars]
suffix_edgy_dead <- function(x,
                             status_d,
                             suffix,
                             x_q = 20,
                             y_q = x_q) {
  stopifnot(
    is.data.frame(x), 
    is.character(suffix),
    is.numeric(x_q),
    is.numeric(y_q)
  )
  fgeo.tool::check_crucial_names(x, c("status", "tag"))
  
  spillover_status <- assert_spillover(x = x, x_q = x_q, y_q = y_q)
  if (spillover_status == FALSE) {
    return(x)
  } else {
    # Subset edgy
    # Account for tag or Tag
    .tag <- fgeo.tool::names_anycase(x, "tag")
    .status <- fgeo.tool::names_anycase(x, "status")
    are_to_tag <- assert_are_to_tag(x = x, x_q = x_q, y_q = y_q)
    to_tag <- x[are_to_tag, ]
    # Suffix edgy tags
    x[are_to_tag, .tag] <- fgeo.tool::str_suffix_match(
      string = to_tag[[.tag]], 
      to_match = to_tag[[.status]], 
      .match = status_d,
      suffix = suffix
    )
    x
  }
}

assert_spillover <- function(x, x_q, y_q) {
  max_x <- max_coord(x, "x")
  max_y <- max_coord(x, "y")
  
  spillover_status <- !all(max_x <= x_q, max_y <= y_q)
  if (isTRUE(spillover_status)) {
    message("One or more trees spillover.")
  } else {
    message("Trees don't spillover.")
  }
  spillover_status
}

max_coord <- function(x, coord) {
  coord_found <- find_coordinate(x, coord)
  max_coord <- max(x[[coord_found]], na.rm = TRUE)
}

find_coordinate <- function(x, coord) {
  lacks_local_coord <- !any(coord %in% names(x), "lx" %in% names(x))
  if (any(lacks_local_coord)) {
    stop("Missing local coordinate ", coord, ". See ?add_lxly().")
  }
  ifelse(coord %in% names(x), coord, paste0("l", coord))
}

assert_are_to_tag <- function(x, x_q, y_q) {
  # Account for x or ly
  .x <- find_coordinate(x, "x")
  .y <- find_coordinate(x, "y")

  edgy_x <- x[[.x]] > x_q
  edgy_y <- x[[.y]] > y_q
  (edgy_x + edgy_y) != 0
}
