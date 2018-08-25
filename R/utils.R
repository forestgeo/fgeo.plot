# Common functions --------------------------------------------------------

commas <- function(...) {
  paste0(..., collapse = ", ")
}

max0 <- function(...) {
  max(..., na.rm = TRUE)
}

min0 <- function(...) {
  min(..., na.rm = TRUE)
}

has_class <- function(x, class) {
  any(grepl(class, class(x)))
}

# Common checks -----------------------------------------------------------

check_unique_plotid <- function(x) {
  msg <- "Detected multiple plotid. Remove all but a single plot; then try again"
  flag_if(x, "plotid", is_multiple, abort, msg)
  invisible(x)
}

check_unique_censusid <- function(x) {
  msg <- "* Likely you should have removed all but one `censusid`"
  flag_if(x, "censusid", is_multiple, warn, msg)
  invisible(x)
}

check_unique_tag <- function(x) {
  msg <- "* Likely you should have removed all but one `tag` per tree."
  flag_if(x, "tag", is_multiple, warn, msg)
  invisible(x)
}

check_max_print <- function(p) {
  max_n <- getOption("max.print")
  if (!max_n >= length(p)) {
    msg <- paste0(
      "The option max.print is too low: Printing only ", max_n, " plots",
      "\n* To allow printing n plots use `options(max.print = n)`."
    )
    warn(msg)
  }
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
