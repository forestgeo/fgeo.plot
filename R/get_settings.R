#' A theme that is customized for plot_repulsive_tags().
#'
#' Theme customized for [plot_repulsive_tags()]. You can further customize the
#' theme by passing more arguments to [ggplot2::theme()] (via `...`) or by
#' replacing this theme completely with a new [ggplot2::theme()].
#'
#' @param panel_grid_major_colour,panel_grid_minor_colour Colour of major grid
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
#' @return A [ggplot2::theme()] customized for [plot_repulsive_tags()].
#'
#' @export
#' @examples
#' ggplot2::theme()
#' get_theme()
get_theme <- function(panel_grid_major_colour = "grey",
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

#' A simple way to produce a three-lines header for a plot of repulsive tags.
#'
#' @param line1,line2,line3 Strings to use on the first, second and third line
#'  of the header.
#'
#' @return A three-lines string to be used as the header on a plot of repulsive
#'   tags.
#'
#' @family functions to fine tune a plot of repulsive tags.
#' @export
#'
#' @examples
#' # Use common defaults
#' get_header()
#'
#' # Or make your own strings.
#' get_header(line1 = "This is", line2 = "a customized", line3 = "header")
#' get_header(line1 = "Only", line2 = "two lines", line3 = NULL)
get_header <- function(line1 = pad(c("Checking: ", "Checked date: ")),
                       line2 = pad(c("Recording: "), total_width = 24),
                       line3 = pad(c("Measuring: ", "Measurement date: "))) {
  paste0(
    "\n",
    line1, "\n",
    line2, "\n",
    line3
  )
}

#' Pad strings.
#'
#' `pad` provides a convenient way to create each line of the headers produced
#' with [get_header()].
#'
#' This function may output strings a little shorter or a little longer than
#' expected based on `total_width`. This is a bug, and appears sometimes when
#' the strings contain spaces. Because  this bug is not too serious, and I leave
#' it for now.
#'
#' @param total_width Total width of the string after pasting each string in the
#'   vector of strings.
#' @param string A character vector.
#' @param width Minimum width of padded strings.
#' @param side Side on which padding character is added (left, right or both).
#' @param pad Single padding character (default is a space).
#' @family functions to fine tune a plot of repulsive tags.
#'
#' @return A padded string that is convenient to produce each line of
#'   [get_header()]
#' @export
#'
#'
#' @examples
#' pad(c("Hello", "world"))
#' pad(c("Different pad", "and shorter"), total_width = 50, pad = ".")
#' pad(c("Padding", "to the left"), total_width = 70, pad = ".", side = "left")
pad <- function(string, total_width = 55, side = "right", pad = "_") {
  string_length <- length(string)
  max_characters_n <- total_width
  string_characters_n <- sum(nchar(string))

  space_to_fill <- (max_characters_n - string_characters_n) / string_length

  filler <- stringr::str_pad(
    string = pad,
    width = space_to_fill,
    side = side,
    pad = pad
  )

  if (side == "right") {
    paste0(string, filler, collapse = " ")
  } else {
    paste0(filler, string, collapse = " ")
  }
}
