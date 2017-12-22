check_crucial_names <- function(x, nms) {
  are_names_expected <- all(nms %in% names(x))
  if (are_names_expected) {
    return(invisible(x))
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
  }
  invisible(x)
}

check_single_censusid <- function(x) {
  if (length(unique(x$censusid)) > 1) {
    warning(
      "Multiple censuses were detected\n",
      "  * Likely you should filter the data to keep only the last `CensusID`"
    )
  }
  invisible(x)
}

#' Test if an object existe in the namespace of a package.
#'
#' @param object String giving the object to find.
#' @param package String giving the package which namespace to search.
#'
#' @return Logical.
#' @noRd
exists_in_pkg <- function(object, package){
  any(grepl(object, ls(paste0("package:", package))))
}

#' Filter a data set by matching n values from the head (or tail) of a variable.
#'
#' Filters a data frame by matching `n` values from the head (or tail) of the
#' unique values of a variable.
#'
#' Similar to [dplyr::top_n()], but instead of using `min_rank()` or
#' `max_rank()`, it uses [utils::head()] or [utils::tail()]; and `var` is
#' flexible as in [dplyr::pull()].
#'
#' @inheritParams dplyr::pull
#' @param n Number of values used for matching, from the head (or tail) of `var`.
#' @seealso [dplyr::pull], [dplyr::top_n], [utils::head()], [utils::tail()].
#'
#' @return
#' @export
#'
#' @examples
#' df <- data.frame(x = 1:9, y = letters[1:3], stringsAsFactors = FALSE)
#'
#' # `var` can be bare or quoted
#' (result <- top(df, "y"))
#' identical(top(df, y), result)
#'
#' # matching `var` by position starting from the left
#' identical(top(df, var = y), top(df, var = 2))
#' # matching `var` by position starting from the right
#' identical(top(df, var = y), top(df, var = -1))
#'
#' top(df, y, n = 2)
#' # Negative values select from the tail
#' top(df, y, n = -2)
top <- function(.data, var, n = 1) {
  var <- enquo(var)
  pulled <- dplyr::pull(.data, !!var)
  sorted <- sort(unique(pulled))
  if (n > 0 ) {
    to_match <- head(sorted, n)
  } else {
    to_match <- tail(sorted, abs(n))
  }
  .data[pulled %in% to_match, ]
}

