# Accesor functions that pass important settings to plot_repel and friends.



#' Theme passed to plot_repel().
#'
#' Theme passed to [plot_repel()]. This function wraps multiple functions that
#' change the looks of a plot. To change the looks of a plot, change the
#' relevant `get_*()` function.
#'
#' @seealso [ggplot2::theme()] [graphics::points()].
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





#' Functions to change plot layout.
#'
#' @param line1,line2,line3 String. First, second and third subtitle lines.
#' @param x A dafault setting.
#'
#' @family functions to change plot layout.
#' @export
#' @name get_settings
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

#' @export
#' @rdname get_settings
get_subtitle_line1 <- function() {
  paste0(
    "Measuring: _________________________ ",
    "Measurement date: _________________________"
  )
}

#' @export
#' @rdname get_settings
get_subtitle_line2 <- function() {
  "Recording: _________________________"
}

#' @export
#' @rdname get_settings
get_subtitle_line3 <- function() {
  paste0(
    "Checking: __________________________ ",
    "Checked date: _____________________________"
  )
}

#' @export
#' @rdname get_settings
get_site_name <- function(x = "Sinharaja 2017") {x}

#' @export
#' @rdname get_settings
get_size_plot_title <- function(x = 20) {x}

#' @export
#' @rdname get_settings
get_size_plot_subtitle <- function(x = 12) {x}

#' @export
#' @rdname get_settings
get_size_axis_text <- function(x = 12) {x}

#' @export
#' @rdname get_settings
get_size_tag <- function(x = 3) {x}

#' @export
#' @rdname get_settings
get_size_point <- function(x = 1.5) {x}


#' @export
#' @rdname get_settings
get_shape_point <- function(x = c(19, 4)) {x}

#' @export
#' @rdname get_settings
get_panel_grid_major_colour <- function(x = "grey") {x}

#' @export
#' @rdname get_settings
get_panel_grid_minor_colour <- function(x = "black") {x}

#' @export
#' @rdname get_settings
get_panel_grid_minor_linetype <- function(x = "dotted") {x}

#' @export
#' @rdname get_settings
get_panel_background_fill <- function(x = "white") {x}

#' @export
#' @rdname get_settings
get_legend_position <- function(x = "top") {x}

#' @export
#' @rdname get_settings
get_legend_title <- function(x = element_blank()) {x}

#' @export
#' @rdname get_settings
get_axis_ticks <- function(x = element_blank()) {x}
