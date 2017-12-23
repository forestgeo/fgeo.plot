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



#' Report if a vector or a variable of a dataframe is duplicated.
#'
#' @param x A dataframe.
#' @param x_var String; the name of a variable of `x`.
#' @param v A vector.
#' @param cond String; the name of a function that outputs a condition: one of
#'   "warning", "stop", "message".
#' @param msg String; a custom message.
#'
#' @return Invisible `v` or a condition and a message.
#' @export
#'
#' @examples
#' # On a vector
#' unique_v <- rep(1, 3)
#' num <- c(1:3)
#' chr <- c(letters[1:3])
#' check_unique_vector(unique_v, "warning")
#' check_unique_vector(num, "warning")
#' check_unique_vector(chr, "message", "Do something")
#'
#' # On a dataframe
#' .df <- data.frame(a = 1:3, b = 1, stringsAsFactors = FALSE)
#'
#' check_unique(.df, "a")
#' check_unique(.df, "a", "message", "do this")
#' # Silent
#' check_unique(.df, "b", "warning", "do this")
check_unique <- function(x, x_var, cond = "warning", msg = NULL) {
  stopifnot(is.data.frame(x))
  if (!x_var  %in% names(x)) stop(x_var, " is an invalid name")

  x_var <- x[[x_var]]
  check_unique_vector(v = x_var, cond = cond, msg = msg)
  invisible(x)
}

#' @rdname check_duplicated
#' @export
check_unique_vector <- function(v, cond, msg = NULL) {
  stopifnot(length(cond) == 1)
  stopifnot(cond %in% c("warning", "stop", "message"))

  customized <- c("The variable is not unique\n", msg)
  if (length(unique(v)) > 1) {
    do.call(cond, list(customized))
  }
  invisible(v)
}

check_unique_plotid <- function(x) {
  msg <- "  * Filter your data to keep a single plot; then try again"
  check_unique(x, "plotid", "stop", msg)
  invisible(x)
}

check_unique_censusid <- function(x) {
  msg <- "  * Likely you should have filtered only the last `censusid`"
  check_unique(x, "censusid", "warning", msg)
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

