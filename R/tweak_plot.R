# Accesor functions that pass important settings to plot_repel and friends.



#' @export
#' @keywords internal
get_subtitle <- function(line1 = get_subtitle_line1(),
                         line2 = get_subtitle_line2(),
                         line3 = get_subtitle_line3()){
  paste0(
    "\n",
    paste0(line1, "\n"),
    paste0(line2, "\n"),
    line3
  )
}
get_subtitle_line1 <- function() {
  paste0(
    "Measuring: _________________________ ",
    "Measurement date: _________________________"
  )
}
get_subtitle_line2 <- function() {
  "Recording: _________________________"
}
get_subtitle_line3 <- function() {
  paste0(
    "Checking: __________________________ ",
    "Checked date: _____________________________"
  )
}



#' Theme passed to plot_repel().
#'
#' Theme passed to [plot_repel()]. This function wraps multiple functions that
#' change the looks of a plot. To change the looks of a plot, change the
#' relevant `get_*()` function.
#'
#' @seealso [ggplot2::theme()].
#'
#' @family functions to change plot layout.
#'
#' @export
get_theme <- function() {
  theme(
    panel.grid.major = element_line(colour = get_panel_grid_major_colour()),
    panel.grid.minor = element_line(
      linetype = get_panel_grid_minor_linetype(),
      colour = get_panel_grid_minor_colour()
    ),
    panel.background = element_rect(fill = get_panel_background_fill()),
    plot.title = element_text(size = get_size_plot_title()),
    plot.subtitle = element_text(size = get_size_plot_subtitle()),
    legend.position = get_legend_position(),
    axis.ticks = get_axis_ticks(),
    axis.text = element_text(size = get_size_axis_text()),
    legend.title = element_blank()
  )
}

#' @family functions to change plot layout.
#' @export
get_site_name <- function(x = "Sinharaja 2017") {x}

#' @family functions to change plot layout.
#' @export
get_size_plot_title <- function(x = 20) {x}

#' @family functions to change plot layout.
#' @export
get_size_plot_subtitle <- function(x = 12) {x}

#' @family functions to change plot layout.
#' @export
get_size_axis_text <- function(x = 12) {x}

#' @family functions to change plot layout.
#' @export
get_size_tag <- function(x = 3) {x}

#' @family functions to change plot layout.
#' @export
get_size_point <- function(x = 1.5) {x}

#' @seealso [graphics::points()].
#' @family functions to change plot layout.
#' @export
get_shape_point <- function(x = c(19, 4)) {x}

#' @family functions to change plot layout.
#' @export
get_panel_grid_major_colour <- function(x = "grey") {x}

#' @family functions to change plot layout.
#' @export
get_panel_grid_minor_colour <- function(x = "black") {x}

#' @family functions to change plot layout.
#' @export
get_panel_grid_minor_linetype <- function(x = "dotted") {x}

#' @family functions to change plot layout.
#' @export
get_panel_background_fill <- function(x = "white") {x}

#' @family functions to change plot layout.
#' @export
get_legend_position <- function(x = "top") {x}

#' @family functions to change plot layout.
#' @export
get_legend_title <- function(x = element_blank()) {x}

#' @family functions to change plot layout.
#' @export
get_axis_ticks <- function(x = element_blank()) {x}
