caption_edge_tag <- function(x, x_q, y_q, max_n = 6) {
  edgy <- dplyr::filter(x, qx > x_q | qy > y_q)
  edgy <- unique(select(edgy, .data$tag, .data$qx, .data$qy))
  if (nrow(edgy) == 0) {
    caption <- NULL
    return(caption)
  } else {
    caption_tag_xy(edgy$tag, edgy$qx, edgy$qy, max_n = max_n)
  }
}

#' caption_tag_xy(letters[1:3], 1:3, 11:13, NULL)
#' caption_tag_xy(letters[1:3], 1:3, 11:13, 0)
#' caption_tag_xy(letters[1:3], 1:3, 11:13, 1)
#' caption_tag_xy(letters[1:3], 1:3, 11:13, 2)
#' caption_tag_xy(letters[1:3], 1:3, 11:13, 3)
#' caption_tag_xy(letters[1:3], 1:3, 11:13, 4)
#' @noRd
caption_tag_xy <- function(x, x_coord, y_coord, max_n = NULL) {
  info <- inform_more(x = x, max_n = max_n)
  x <- info$x
  and_more <- info$and_more

  show <- show_max(x = x, max_n = max_n, x_coord = x_coord, y_coord = y_coord)
  to_show <- show %>%
    purrr::pmap(~ paste0(..1, ":(", ..2, ", ", ..3, ")")) %>%
    paste0(collapse = "; ")
  if (all(!is.null(max_n), max_n == 0)) {
    to_show <- NULL
  }

  paste0(to_show, and_more)
}

#' `inform_more()` and `show_max()` help to truncate the number of items of a
#' list, based on the argument `max_n` and/or the length of `x`.
#'
#' inform_more(1:3, NULL)
#' inform_more(1:3, 0)
#' inform_more(1:3, 3)
#' inform_more(1:3, 4)
#' @noRd
inform_more <- function(x, max_n) {
  if (!is.null(max_n)) {
    stopifnot(!is.na(max_n))
  }

  and_more <- NULL

  if (all(!is.null(max_n), length(x) > max_n)) {
    n <- length(x) - max_n
    and_more <- paste0(" ... and ", n, " more.")
    x <- x[1:max_n]
    if (max_n == 0) x <- NULL
  }

  list(x = x, and_more = and_more)
}

#' Subset `max_n` or `length(x)` elements from `...` (whichever is smaller);
#' If max_n is 0 return NULL.
#' @noRd
show_max <- function(x, max_n, ...) {
  n_to_show <- length(x)
  to_show <- list(x, ...) %>%
    purrr::map(~ .[1:n_to_show])
  to_show
}
