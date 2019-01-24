#' Pre-made headers.
#'
#' @param lang String; Language of the header: Either "english" or "spanish".
#'
#' @name headers
#' @keywords internal
NULL

#' @inherit headers
#'
#' @examples
#' cat(header_tag_status())
#' @family functions to plot tag status
#' @keywords internal
#' @export
header_tag_status <- function() {
  paste(
    "",
    "Checking: _______________ Checked date: _______________",
    "Recording: _____________",
    "Measuring: _____________ Measurement date: _____________",
    sep = "\n"
  )
}

#' @inherit headers
#'
#' @examples
#' cat(header_dbh_bubbles())
#' @family functions to plot dbh bubbles
#' @keywords internal
#' @export
header_dbh_bubbles <- function(lang = "english") {
  stopifnot(lang %in% c("english", "spanish"))
  stopifnot(length(lang) == 1)

  if (lang == "english") {
    description <- enter_line(
      "Names and date: ", "Reviewed by:        ", "Entered on PC by: ",
      times = 25
    )
  }
  if (lang == "spanish") {
    description <- enter_line(
      "Nombres y Fecha:", "Revisado por:       ", "Entrado en PC por:",
      times = 25
    )
  }
  blank <- enter_line(rep("_________________________", 3))

  paste(
    "",
    description,
    "",
    blank,
    blank,
    blank,
    "",
    sep = "\n"
  )
}

enter_line <- function(..., sep = " ", times = 4) {
  paste(c(...), collapse = paste(rep(sep, times), collapse = ""))
}
