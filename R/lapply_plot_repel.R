#' Plot trees in subquadrats, avoiding tree tags to overlap.
#'
#' @param df_list A list of data frames.
#' @param ... Arguments passed to `plot_repel()`.
#' @param df A data frame as wranglec by `lapply_plot_repel()`.
#' @param point_size Point size passed to [ggplot2::geom_point()].
#' @param id_size Size of tree tags, passed to [geom_text_repel()].
#' @param text_size Text size passed to [ggplot2::element_text()].
#'
#' @return A list of plots that can be wrapped around pdf(onefile = TRUE).
#' @export
#'
#' @examples
lapply_plot_repel <- function(df_list,
                              point_size = 1.5,
                              id_size = 3,
                              text_size = 15) {
  prepared <- prepare_for_plot_repel(df_list)
  splitted <- split(prepared, prepared$id)

  lapply(X = splitted, FUN = plot_repel,
    point_size = point_size, id_size =id_size, text_size = text_size
  )
}

#' @export
#' @rdname lapply_plot_repel
plot_repel <- function(df, point_size, id_size, text_size) {
  title_string <- unique(df$id)
  ggplot(df, aes(x = lx, y = ly, shape = sym2)) +
    scale_shape_manual(values = c(19, 4)) +
    geom_point(size = point_size) +
    geom_text_repel(aes(label = tag), size = id_size) +
    scale_x_continuous(minor_breaks = seq(1, 20, 1), breaks = seq(0, 20, 5)) +
    scale_y_continuous(minor_breaks = seq(1, 20, 1), breaks = seq(0, 20, 5)) +
    coord_fixed(xlim = c(unique(df$x1), unique(df$x2)), ylim = c(unique(df$y1), unique(df$y2))) +
    labs(x = NULL, y = NULL) +
    get_theme(text_size = text_size) +
    labs(title = paste("Quadrat:", title_string), subtitle = get_subtitle())
}

#' Wrap a number of functions that prepare a list of data frames for plotting.
#'
#' @export
#' @keywords internal
#'
#' @examples
prepare_for_plot_repel <- function(df_list) {
  explicit <- purrr::map(df_list, codify_explicitely)
  identified <- identify_subquadrat(explicit)
  add_limits_shrinked(identified)
}
