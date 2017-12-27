# functions to filter dataframes. -----------------------------------------

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
#' @return A filtered version of the input dataset.
#' @export
#' @family functions to filter dataframes.
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



#' Remove trees found dead in both the last and previous last censuses.
#'
#' Removes trees that were found dead both in the last and previous last
#' censuses. Thus the resulting data set contains trees that in the last census
#' were found either alive or dead for the first time.
#'
#' @template vft
#'
#' @return A modified version of the input data set:
#'     * With an additional variable indicating the status of each tree.
#'     * With the rows removed of all censuses except the last two.
#'     * With the rows removed of trees found dead on both the last and previous
#'       last censuses.
#' @export
#' @family functions to filter dataframes.
#'
#' @examples
#' vft <- tibble::tribble(
#'    ~PlotCensusNumber, ~Tag,  ~Status,
#'    1,    1,   "alive",
#'    1,    1,    "dead",
#'    1,    2,   "alive",
#'    1,    2,   "alive",
#'
#'    2,    1,   "alive",
#'    2,    1,    "dead",
#'    2,    2,    "dead",
#'    2,    2,    "dead",
#'
#'    3,    1,   "alive",
#'    3,    1,    "dead",
#'    3,    2,    "dead",
#'    3,    2,    "dead"
#'  )
#'
#' # Notice the rows where `status_tree` in census 3 and 2 is "dead"
#' # (The variable `status` refers to stems, while `status_tree` refers to
#' #  trees.)
#' add_status_tree(vft)
#'
#' #' * Remove all censuses except the last two.
#' #' * Remove trees found dead on both the last and previous last censuses.
#' rm_dead_twice(vft)
rm_dead_twice <- function(vft) {
  stopifnot(is.data.frame(vft))
  check_crucial_names(vft, c("PlotCensusNumber", "Tag", "Status"))

  if (!length(unique(vft$PlotCensusNumber)) >= 2) {
    warning("`The data set has less than two censuses; Keeping all trees")
    return(vft)
  }

  last <- max(vft$PlotCensusNumber, na.rm = TRUE)
  last2 <- vft[vft$PlotCensusNumber %in% c(last, last - 1), ]
  last2 <-  add_status_tree(last2)
  grouped <- dplyr::group_by(last2, .data$PlotCensusNumber, .data$Tag)
  to_filter <- dplyr::ungroup(
    dplyr::mutate(
      grouped, is_to_keep = !identical(.data$status_tree, c("dead", "dead"))
    )
  )
  to_filter[to_filter$is_to_keep, setdiff(names(to_filter), "is_to_keep")]
}



# Other functions ---------------------------------------------------------

#' Define the status of a tree based on the status of its stems.
#'
#' @param x A ViewFullTable or a fgeo table.
#'
#' @return The input data set with lowercase names, and with the additional
#'   variable status_tree.
#' @export
#'
#' @examples
#' df <- tibble::tribble(
#'   ~PlotCensusNumber, ~Tag, ~Status,
#'                   1,    1, "alive",
#'                   1,    1,  "dead",
#'                   1,    2,  "dead",
#'                   1,    2,  "dead",
#'
#'                   2,    1, "alive",
#'                   2,    1, "alive",
#'                   2,    2, "alive",
#'                   2,    2,  "dead"
#' )
#' add_status_tree(df)
add_status_tree <- function(x) {
  old <- names(x)
  x <- rlang::set_names(x, tolower)
  check_add_status_tree(x)
  grouped <- dplyr::group_by(x, .data$plotcensusnumber, .data$tag)
  mutated <- dplyr::mutate(
    grouped,
    status_tree = ifelse(all(.data$status == "dead"), "dead", "alive")
  )
  # Restoring names
  if (any(grepl("status_tree", old))) {
    rlang::set_names(dplyr::ungroup(mutated), old)
  } else {
    rlang::set_names(dplyr::ungroup(mutated), c(old, "status_tree"))
  }
}

check_add_status_tree <- function(x) {
  is_vft <- "plotid"  %in% names(x)
  if (is_vft) check_unique_plotid(x)
  crucial_vars <- c("tag", "status", "plotcensusnumber")
  check_crucial_names(x, crucial_vars)
  invisible(x)
}






# functions for developers ------------------------------------------------

#' Test if an object exists in the namespace of a package.
#'
#' Test if an object exists in the namespace of a package. Useful inside an
#' `if` statement.
#'
#' @param object String giving the object to find.
#' @param package String giving the package which namespace to search.
#'
#' @return Logical.
#' @export
#' @family functions for developers.
#' @examples
#' library(map)
#' exists_in_pkg("one_quadrat", "map")
exists_in_pkg <- function(object, package){
  any(grepl(object, ls(paste0("package:", package))))
}



# functions to check inputs -----------------------------------------------

#' Check if an object contains specific names.
#'
#' @param x A named object.
#' @param nms String; names expected to be found in `x`.
#'
#' @family functions to check inputs.
#' @family functions for developers.
#' @return Invisible `x`, or an error with informative message.
#' @export
#'
#' @examples
#' v <- c(x = 1)
#' check_crucial_names(v, "x")
#'
#' dfm <- data.frame(x = 1)
#' check_crucial_names(dfm, "x")
check_crucial_names <- function(x, nms) {
  stopifnot(rlang::is_named(x))
  stopifnot(is.character(nms))

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
#' @family functions to check inputs.
#' @family functions for developers.
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

#' @rdname check_unique
#' @export
check_unique_vector <- function(v, cond, msg = NULL) {
  stopifnot(length(cond) == 1)
  stopifnot(cond %in% c("warning", "stop", "message"))

  customized <- c("Duplicated values were detected\n", msg)
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

check_unique_plotcensusnumber <- function(x) {
  msg <- "  * Likely you should have filtered only the last `plotcensusnumber`"
  check_unique(x, "plotcensusnumber", "warning", msg)
  invisible(x)
}


