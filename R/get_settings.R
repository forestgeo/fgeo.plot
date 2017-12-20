#' A theme for `map_tag()`.
#'
#' You can further customize the theme by passing more arguments to
#' [ggplot2::theme()] (via `...`) or by replacing this theme completely with a
#' new [ggplot2::theme()].
#'
#' @inheritDotParams ggplot2::theme
#'
#' @seealso [ggplot2::theme()].
#'
#' @return A [ggplot2::theme()] for [map_tag()].
#'
#' @export
#' @examples
#' get_theme()
get_theme <- function(panel.grid.major = element_line(colour =  "black"),
                      panel.grid.minor = element_line(
                        colour =  "black",
                        linetype = "dotted"
                      ),
                      panel.background = element_rect(fill = "white"),
                      plot.title = element_text(size = 20),
                      plot.subtitle = element_text(size = 12),
                      legend.position = "top",
                      legend.title = element_blank(),
                      axis.ticks = element_blank(),
                      axis.text = element_text(size = 12),
                      ...){
  theme(
    panel.grid.major = panel.grid.major,
    panel.grid.minor = panel.grid.minor,
    panel.background = panel.background,
    plot.title = plot.title,
    plot.subtitle = plot.subtitle,
    legend.position = legend.position,
    legend.title = legend.title,
    axis.ticks = axis.ticks,
    axis.text = axis.text,
    ...
  )
}

#' Pre-made headers.
#'
#' @name headers
#' @examples
#' header_map_tag()
#' header_map_quad()
NULL

#' @rdname headers
#' @export
header_map_tag <- function() {
  paste(
    "",
    "Checking: _______________ Checked date: _______________",
    "Recording: _____________",
    "Measuring: _____________ Measurement date: _____________",
    sep = "\n"
  )
}

#' @rdname headers
#' @export
header_map_quad <- function() {
  fill_blank <- "__________________  __________________  __________________"
  paste(
    "Nombres y fecha:           Revisado por:                Entrado en PC por:",
    "",
    fill_blank,
    fill_blank,
    fill_blank,
    sep = "\n"
  )
}
