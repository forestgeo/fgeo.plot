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
  x <- fgeo.tool::nms_lowercase(x)
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
    fgeo.tool::nms_restore(x)
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

