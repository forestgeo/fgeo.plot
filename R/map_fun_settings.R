# Themes ------------------------------------------------------------------

#' Themes.
#'
#' These functions output themes for specific functions. You can customize any
#' theme by changing the default arguments and also by passing more arguments to
#' [ggplot2::theme()] (via `...`).
#'
#' @inheritParams ggplot2::theme
#' @param ... Additional arguments passed to  [ggplot2::theme()].
#' @seealso [ggplot2::theme()].
#' @return A [ggplot2::theme()].
#' @examples
#' theme_map_tag()
#' theme_map_quad()
#' @name themes
NULL

#' @rdname themes
#' @export
theme_map_tag <- function(panel.grid.major = element_line(colour =  "black"),
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

#' @rdname themes
#' @export
theme_map_quad <- function(axis.text = element_blank(),
  panel.background = element_rect(fill = "white"),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  legend.position = "none",
  plot.title = element_text(size = 20),
  plot.subtitle = element_text(size = 12),
  panel.border = element_rect(colour = "black", fill = NA),
  axis.ticks.length = unit(-0.1, "cm"),
  ...) {
  theme(
    panel.background = panel.background,
    panel.grid.minor = panel.grid.minor,
    panel.grid.major = panel.grid.major,
    legend.position = legend.position,
    plot.title = plot.title,
    plot.subtitle = plot.subtitle,
    axis.text = axis.text,
    panel.border = panel.border,
    axis.ticks.length = axis.ticks.length,
    ...
  )
}



# Headers -----------------------------------------------------------------

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
