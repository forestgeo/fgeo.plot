#' Plot trees in subquadrats, avoiding tree tags to overlap.
#'
#' @param df_list A list of data frames.
#' @param df A data frame as wranglec by `lapply_plot_repel()`.
#'
#' @return A list of plots that can be wrapped around pdf(onefile = TRUE).
#' @export
#'
#' @examples
lapply_plot_repel <- function(df_list, site_name = get_site_name()) {
  prepared <- prepare_for_plot_repel(df_list)
  splitted <- split(prepared, prepared$id)

  lapply(X = splitted, FUN = plot_repel, site_name = site_name)
}

#' @export
#' @rdname lapply_plot_repel
plot_repel <- function(df, site_name) {
  id_quadrat_subquadrat <- unique(df$id)
  ggplot(df, aes(x = lx, y = ly, shape = sym2)) +
    scale_shape_manual(values = get_shape_point()) +
    geom_point(size = get_size_point()) +
    geom_text_repel(aes(label = tag), size = get_size_tag()) +
    scale_x_continuous(minor_breaks = seq(1, 20, 1), breaks = seq(0, 20, 5)) +
    scale_y_continuous(minor_breaks = seq(1, 20, 1), breaks = seq(0, 20, 5)) +
    coord_fixed(
      xlim = c(unique(df$x1), unique(df$x2)),
      ylim = c(unique(df$y1), unique(df$y2))
    ) +
    labs(x = NULL, y = NULL) +
    get_theme() +
    labs(
      title = paste0(site_name, ". Quadrat ", id_quadrat_subquadrat),
      subtitle = get_subtitle()
    )
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
