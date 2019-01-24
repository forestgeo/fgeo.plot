pick_top <- function(.data, var, n = 1) {
  var <- enquo(var)
  pulled <- dplyr::pull(.data, !!var)
  sorted <- sort(unique(pulled))
  if (n > 0) {
    to_match <- head(sorted, n)
  } else {
    to_match <- tail(sorted, abs(n))
  }
  .data[pulled %in% to_match, ]
}

commas <- function(...) {
  paste0(..., collapse = ", ")
}

maximum <- function(...) {
  max(..., na.rm = TRUE)
}

has_class <- function(x, class) {
  any(grepl(class, class(x)))
}

check_max_print <- function(x, var, times = NULL) {
  nn <- length(unique(x[[var]]))
  if (!is.null(times)) nn <- nn * times

  max_n <- getOption("max.print")
  if (!max_n >= nn) {
    msg <- paste0(
      "Option max.print is low: ", max_n, ". Number of unique values of `",
      var, "`: ", nn, ".\n",
      "* Consider using `options(max.print = HIGHER-NUMBER)`."
    )
    warn(msg)
  }
}

warn_multiple_plotid <- function(x) {
  msg <- "Detected multiple plotid. Remove all but a single plot; then try again"
  flag_if(x, "plotid", is_multiple, abort, msg)
}
