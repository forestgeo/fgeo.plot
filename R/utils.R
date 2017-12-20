check_crucial_names <- function(x, nms) {
  are_names_expected <- all(nms %in% names(x))
  if (are_names_expected) {
    return(invisible())
  } else {
    stop(
      "Ensure your data set has these variables (regardles of the case):\n",
      paste0(nms, collapse = ", "),
      call. = FALSE
    )
  }
}

check_single_plotid <- function(x) {
  plots <- unique(x$plotid)
  number_of_plots <- length(plots)
  if (number_of_plots > 1) {
    stop(
      "`plotid` contains these plots: ", paste(plots, collapse = ", "), "\n",
      "  * Filter your data to keep a single plot; then try again",
      call. = FALSE
    )
  } else {
    invisible()
  }
}

check_single_censusid <- function(x) {
  if (length(unique(x$censusid)) > 1) {
    warning(
      "Multiple censuses were detected",
      "  * Likely you should filter the data to keep only the last `CensusID`"
    )
  }
}
