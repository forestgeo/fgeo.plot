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

#' Test if an object existe in the namespace of a package.
#'
#' @param object String giving the object to find.
#' @param package String giving the package which namespace to search.
#'
#' @return Logical.
#'
#' @examples
#' exists_in_pkg("cars", "datasets")
exists_in_pkg <- function(object, package){
  any(grepl(object, ls(paste0("package:", package))))
}


#' Print one element of a list and return the list invisibly.
#'
#' Printing to screen all elements of a list may be time consuming. This
#' function saves time by printing only the first element of the list.
#'
#' The element printed is extracted with `[` -- not with `[[` as in
#' [dplyr::first()] -- to preserve the name of the element (if any).
#'
#' @param .x A list.
#' @param element A string describing each element of the input list.
#' @seealso [dplyr::first()]
#'
#' @return Returns the input invisibly.
#' @export
#'
#' @examples
#' print_first(list(1, 2))
#' print_first(list(1, 2), "plot")
print_first <- function(.x, element = "element") {
  stopifnot(is.list(.x))
  stopifnot(is.character(element))

  message(
    crayon::red("Output is a list of", length(.x), "\n"),
    crayon::black("* Showing only the first", element, "\n"),
    crayon::black("* Returning the entire list invisibly")
  )

  print(.x[1])
  invisible(.x)
}
