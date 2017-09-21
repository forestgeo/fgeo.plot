#' Get settings for plot_repel().
#'
#' @param site_string A string giving the name of the site. E.g. "Sinharaja".
#'
#' @export
#' @keywords internal
get_subtitle <- function(site_string = "(Sinharaja)") {
  paste0(
    site_string,
    "\n",
    "Measuring: _______ Measurement date: _______\n",
    "Recording: _______ Checking: _______________\n",
    "Checked date: _______"
  )
}

#' Get the theme of plot_repel().
#'
#' @export
#' @keywords internal
get_theme <- function(text_size) {
  theme(
    panel.grid.major = element_line(colour = "black"),
    panel.grid.minor = element_line(linetype = "dotted", colour = "black"),
    panel.background = element_rect(fill = 'white'),
    # panel.border = element_rect(linetype = "dashed"),
    legend.position = "top",
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    text = element_text(size = text_size)
  )
}
