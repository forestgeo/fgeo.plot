#' A theme that is customized for plot_repulsive_tags().
#'
#' Theme customized for [plot_repel()]. You further customize passing more
#' arguments to [ggplot2::theme()] via `...` or replace this theme by a
#' completely new [ggplot2::theme()].
#'
#' @param panel_grid_major_colour,panel_grid_minor_colour,panel_grid_minor_linetype,panel_background_fill,plot_title_size,plot_subtitle_size,legend_position,axis_ticks,axis_text_size,legend_title,... Arguments passedto
#'   [ggplot2::theme()].
#'
#' @seealso [ggplot2::theme()].
#' @family functions to fine tune the [plot_repulsive_tags()].
#'
#' @return A customized [ggplot2::theme()].
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
#' get_header()
#' get_header(line1 = "Hello")
get_header <- function(line1 = pad(c("Checking: ", "Checked date: ")),
                       line2 = pad(c("Recording: "), 24),
                       line3 = pad(c("Measuring: ", "Measurement date: "))) {
  paste0(
    "\n",
    line1, "\n",
    line2, "\n",
    line3
  )
}

#' Right-pad strings.
#'
#' @param total_width Total width of the string after pasting each string in the
#'   vector of strings.
#' @param pad,string,side, ... Arguments passed to [stringr::str_pad()].
#'
#' @return A string which total number of characters is `total_width`.
#' @export
#'
#' @examples
#' pad(c("Hello", "world"))
#' pad(c("Hello", "world"), total_width = 70, pad = ".")
#' pad(c("Hello", "world"), total_width = 70, pad = ".", side = "left")
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
    paste0(filler, " ", string, collapse = " ")
  }
}
