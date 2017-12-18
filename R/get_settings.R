#' A theme that is customized for `map_tag()`.
#'
#' Theme customized for [map_tag()]. You can further customize the
#' theme by passing more arguments to [ggplot2::theme()] (via `...`) or by
#' replacing this theme completely with a new [ggplot2::theme()].
#'
#' @param panel_grid_major_colour Colour of major grid
#'   lines.
#' @param panel_grid_minor_colour,panel_grid_minor_linetype Colour and line type
#'   of minor grid lines.
#' @param panel_background_fill Fill colour of the background of plotting area,
#'   drawn underneath plot.
#' @param plot_title_size,plot_subtitle_size Size of the plot title and
#'   subtitle.
#' @param legend_position The position of legends ("none", "left", "right",
#'   "bottom", "top", or two-element numeric vector).
#' @param axis_ticks Tick marks along axes.
#' @param axis_text_size Size of tick labels along axes.
#' @param legend_title title of legend.
#' @param ... Other arguments passed to [ggplot2::theme()].
#'
#' @seealso [ggplot2::theme()].
#' @family functions to fine tune a plot of repulsive tags.
#'
#' @return A [ggplot2::theme()] customized for [map_tag()].
#'
#' @export
#' @examples
#' ggplot2::theme()
#' map::get_theme()
get_theme <- function(panel_grid_major_colour = "black",
                      panel_grid_minor_colour = "black",
                      panel_grid_minor_linetype = "dotted",
                      panel_background_fill = "white",
                      plot_title_size = 20,
                      plot_subtitle_size = 12,
                      legend_position = "top",
                      axis_ticks = element_blank(),
                      axis_text_size = 12,
                      legend_title = element_blank(),
                      ...) {
  theme(
    panel.grid.major = element_line(colour = panel_grid_major_colour),
    panel.grid.minor = element_line(
      colour = panel_grid_minor_colour,
      linetype = panel_grid_minor_linetype
    ),
    panel.background = element_rect(fill = panel_background_fill),
    plot.title = element_text(size = plot_title_size),
    plot.subtitle = element_text(size = plot_subtitle_size),
    legend.position = legend_position,
    legend.title = legend_title,
    axis.ticks = axis_ticks,
    axis.text = element_text(size = axis_text_size),
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
