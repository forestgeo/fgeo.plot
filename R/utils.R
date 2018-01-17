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
