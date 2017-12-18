#' Title
#'
#' @param vft
#' @param lim_min
#' @param lim_max
#' @param subquadrat_side
#' @param size_label
#' @param offset
#' @param title
#' @param header
#' @param .theme
#'
#' @return
#' @export
#'
#' @examples
map_quad <- function(vft,
                    lim_min = 0,
                    lim_max = 20,
                    subquadrat_side = 5,
                    size_label = 2,
                    offset = 1,
                    title = "My Title",
                    header = header_map_quad(),
                    .theme = theme_map_quad()) {
  # xxx cont. checks; tests; document













  map_quad_each <- function(df) {
    .title <- paste(title, unique(one_quadrat$QuadratName), sep = ", ")
    ggplot(df, aes(x = x, y = y)) +
    geom_text_repel(aes(label = tagged_tag), size = size_label) +
    geom_point(aes(size = dbh_standarized), shape = 1) +
    labs(title = .title, subtitle = header, x = NULL, y = NULL) +
    geom_vline(
      xintercept = seq(lim_min, lim_max, by = subquadrat_side),
      linetype = "dashed"
    ) +
    geom_hline(
      yintercept = seq(lim_min, lim_max, by = subquadrat_side),
      linetype = "dashed"
    ) +
    coord_fixed(
      xlim = c(lim_min + offset, lim_max - offset),
      ylim = c(lim_min + offset, lim_max - offset)
    ) +
    scale_x_continuous(breaks = lim_min:lim_max, sec.axis = dup_axis()) +
    scale_y_continuous(breaks = lim_min:lim_max, sec.axis = dup_axis()) +
    .theme
  }

  df_list <- split(vft, vft$QuadratName)
  lapply(df_list, map_quad_each)
}

# Helpers -----------------------------------------------------------------

#' Theme for map_quad
#'
#' @return A ggplot2 theme.
#' @export
#'
#' @examples
#' theme_map_quad()
theme_map_quad <- function() {
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 12),
    axis.text = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.ticks.length = unit(-0.1, "cm")
  )
}

#' Add the ending ".d" to the `Tag` of dead stems.
#'
#' @param x A character vector.
#' @param y A character vector giving the variable Status of a fgeo-table.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' tag_dead(
#'   x = c("tag1", "tag2"),
#'   y = c("dead", "whatever")
#' )
tag_dead <- function(x, y) {
  if (!all(is.character(x), is.character(y))) {
    stop("Both `x` and `y` must be character vectors", call. = FALSE)
  }
  if (!"dead" %in% y) {warning("No stem is dead. Is that what you expect?")}

  is_dead <- y == "dead"
  x[is_dead] <- paste0(x[is_dead], ".", sub("^(.).*$", "\\1", y[is_dead]))
  x
}

