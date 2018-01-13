#' Suffix tags of dead stems that map beyond the quadrat's edge.
#'
#' @param x A ViewFullTable.
#' @template status_d
#' @param suffix A character string to suffix tree tags with.
#' @param x_q,y_q Integer; the x and y size in meters of a quadrat.
#'
#' @return A modified version of `x`.
#' @export
#'
#' @examples
#' vft <- tibble::tibble(QX = 21, QY = 21, Tag = "01", Status = "dead")
#' vft
#' 
#' suffix_edgy_dead(
#'   x = vft, 
#'   status_d = "dead",
#'   suffix = "_d", 
#'   x_q = 20
#' ) 
suffix_edgy_dead <- function(x, status_d, suffix, x_q = 20, y_q = x_q) {
  stopifnot(
    is.data.frame(x), 
    is.character(status_d),
    is.character(suffix),
    is.numeric(x_q),
    is.numeric(y_q)
  )
  x <- fgeo.tool::names_lowercase(x)
  fgeo.tool::check_crucial_names(x, c("status", "tag", "qx", "qy"))

  spillover_status <- detect_spillover(x = x, x_q = x_q, y_q = y_q)
  if (spillover_status == FALSE) {
    return(x)
  } else {
    # Subset edgy
    are_to_tag <- detect_to_tag(x = x, x_q = x_q, y_q = y_q)
    to_tag <- x[are_to_tag, ]
    # Suffix edgy tags
    x[are_to_tag, "tag"] <- fgeo.tool::str_suffix_match(
      string = to_tag[["tag"]], 
      to_match = to_tag[["status"]], 
      .match = status_d,
      suffix = suffix
    )
    fgeo.tool::names_restore(x)
  }
}

detect_spillover <- function(x, x_q, y_q) {
  x_max <- max0(x[["qx"]])
  y_max <- max0(x[["qy"]])
  
  spillover_status <- !all(x_max <= x_q, y_max <= y_q)
  if (isTRUE(spillover_status)) {
    message("One or more trees spillover.")
  } else {
    message("Trees don't spillover.")
  }
  spillover_status
}

detect_to_tag <- function(x, x_q, y_q) {
  edgy_x <- x[["qx"]] > x_q
  edgy_y <- x[["qy"]] > y_q
  (edgy_x + edgy_y) != 0
}



# Move to fgeo.tool -------------------------------------------------------

#' Functions to detect and extract names.
#' 
#' These functions are handy to work with fgeo's data structures because the
#' same variablel may be named differently in different data sets. For example,
#' the variable status is called `Status` or `status` in viewfull or census
#' (tree and stem) tables.
#' 
#' `nms_has_any()`: Checks if an object has any of the provided names.
#' * Returns a logical value.
#' `nms_detect()`: Checks if an object has the provided names. 
#' * Returns a logical vector.
#' `nms_extract_all()`: Finds the names that match the provided names. 
#' * Returns a character vector.
#' `nms_extract1()`: Finds the first name that matchs the provided names.
#' * Returns a character string.
#'
#' @param x A named object.
#' @param ... Strings; each of the names that need to be checked.
#' 
#' @keywords internal
## examples
## v <- c(a = 1, b = 1)
## nms_has_any(v, "a", "B")
## nms_has_any(v, "A", "B")
## nms_has_any(v, "A", "b")
## 
## nms_detect(v, "a", "B", "b")
## 
## nms_extract_all(v, "a", "B")
## nms_extract_all(v, "a", "a", "b")
## 
## nms_extract1(v, "a", "a", "b")
nms_has_any <- function(x, ...) {
  any(nms_detect(x, ...))
}

nms_detect <- function(x, ...) {
  purrr::map_lgl(list(...), ~rlang::has_name(x, .))
}

nms_extract_all <- function(x, ...) {
  is_detected <- nms_detect(x, ...)
  nms <- unlist(list(...))
  unique(nms[is_detected])
}

nms_extract1 <- function(x, ...) {
  extracted <- nms_extract_all(x, ...)
  if (length(extracted) == 0) {
    extracted
  } else {
    extracted[[1]]
  }
}

