#' Themes.
#'
#' This function outputs a theme for a specific function. You can customize any
#' theme by changing the default arguments and also by passing more arguments to
#' [ggplot2::theme()] (via `...`).
#'
#' @inheritParams ggplot2::theme
#' @param ... Additional arguments passed to  [ggplot2::theme()].
#'
#' @seealso [ggplot2::theme()].
#'
#' @return A [ggplot2::theme()].
#'
#' @keywords internal
#' @name themes
NULL

#' Theme for `plot_base_census()`.
#'
#' @inheritParams ggplot2::theme
#'
#' @examples
#' class(theme_tag_status())
#' @family functions to plot tag status
#' @keywords internal
#' @export
theme_tag_status <- function(panel.grid.major = element_line(colour = "black"),
                             panel.grid.minor = element_line(
                               colour = "black",
                               linetype = "dotted"
                             ),
                             panel.background = element_rect(fill = "white"),
                             plot.title = element_text(size = 20),
                             plot.subtitle = element_text(size = 12),
                             plot.caption = element_text(size = 8),
                             legend.position = "top",
                             legend.title = element_blank(),
                             axis.ticks = element_blank(),
                             axis.text = element_text(size = 12),
                             ...) {
  theme(
    panel.grid.major = panel.grid.major,
    panel.grid.minor = panel.grid.minor,
    panel.background = panel.background,
    plot.title = plot.title,
    plot.subtitle = plot.subtitle,
    plot.caption = plot.caption,
    legend.position = legend.position,
    legend.title = legend.title,
    axis.ticks = axis.ticks,
    axis.text = axis.text,
    ...
  )
}

#' Theme for `theme_dbh_bubbles()`.
#'
#' @inheritParams ggplot2::theme
#'
#' @examples
#' class(theme_dbh_bubbles())
#' @family functions to plot dbh bubbles
#' @keywords internal
#' @export
theme_dbh_bubbles <- function(axis.text = element_blank(),
                              panel.background = element_rect(fill = "white"),
                              panel.grid.minor = element_blank(),
                              panel.grid.major = element_blank(),
                              legend.position = "none",
                              plot.title = element_text(size = 20),
                              plot.subtitle = element_text(size = 12),
                              panel.border = element_rect(
                                colour = "black", fill = NA
                              ),
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

#' Default plot theme.
#'
#' @inheritParams ggplot2::theme
#' @template p
#'
#' @examples
#' class(theme_default(ggplot2::ggplot(iris)))
#' @family functions to create or modify plot layers
#' @keywords internal
#' @export
theme_default <- function(p,
                          panel.grid.minor = element_line(linetype = "dashed"),
                          ...) {
  p +
    theme_bw() +
    theme(panel.grid.minor = panel.grid.minor, ...)
}
