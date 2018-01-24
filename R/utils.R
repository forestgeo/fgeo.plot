# Common functions --------------------------------------------------------

collapse <- function(...) {
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
  msg <- "* Remove all but a single plot; then try again"
  fgeo.tool::check_unique(x, "plotid", "stop", msg)
  invisible(x)
}

check_unique_censusid <- function(x) {
  msg <- "* Likely you should have removed all but one `censusid`"
  fgeo.tool::check_unique(x, "censusid", "warning", msg)
  invisible(x)
}

check_unique_tag <- function(x) {
  msg <- "* Likely you should have removed all but one `tag` per tree."
  fgeo.tool::check_unique(x, "tag", "warning", msg)
  invisible(x)
}

check_max_print <- function(p) {
  max_n <- getOption("max.print")
  if (!max_n >= length(p)) {
    msg <- paste0(
      "The option max.print is too low: Printing only ", max_n, " plots",
      "\n* To allow printing n plots use `options(max.print = n)`."
    )
    rlang::warn(msg)
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
    rlang::warn(msg)
  }
}

# Manipulate names --------------------------------------------------------

# x <- bciex::bci_elevation
# x %>% 
#   nms_try_rename("gx", "x") %>% 
#   nms_try_rename("gy", "y")
nms_try_rename <- function(x, want, try) {
  nm <- fgeo.tool::nms_extract1(x = x, want = want, try = try)
  if (length(nm) == 0) {
    rlang::abort(
      paste0("Data must have a column named `", want, "` or `", try, "`")
    )
  }
  names(x)[grepl(nm, names(x))] <- want
  x
}
