#' Get settings for plot_repel().
#'
#' @param site_string A string giving the name of the site. E.g. "Sinharaja".
#'
#' @export
#' @keywords internal
get_subtitle <- function(
  line1 = paste0(
    "Measuring: _________________________ ",
    "Measurement date: _________________________"
  ),
  line2 = "Recording: _________________________",
  line3 = paste0(
    "Checking: __________________________ ",
    "Checked date: _____________________________"
  )
  ){
  paste0(
    "\n",
    paste0(line1, "\n"),
    paste0(line2, "\n"),
    line3
  )
}









# Size
get_size_plot_title <- function(x = 20) {x}
get_size_plot_subtitle <- function(x = 12) {x}
get_size_axis_text <- function(x = 12) {x}
get_size_tag <- function(x = 2.5) {x}


#' Get the theme of plot_repel().
#'
#' @export
#' @keywords internal
get_theme <- function() {
  theme(
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(linetype = "dotted", colour = "black"),
    panel.background = element_rect(fill = 'white'),
    plot.title = element_text(size = get_size_plot_title()),
    plot.subtitle = element_text(size = get_size_plot_subtitle()),
    legend.position = "top",
    axis.ticks = element_blank(),
    axis.text = element_text(size = get_size_axis_text()),
    legend.title = element_blank()
  )
}
