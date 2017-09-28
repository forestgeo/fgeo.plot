# Plot data ----

#' Plot trees in subquadrats, avoiding tree tags to overlap.
#'
#' @param prep_df_list A list of data frames, each prapared specifically for
#'   [plot_repulsive_tags].
#' @param prep_df A data frames prapared specifically for [plot_repulsive_tags].
#' @param site_name A sting of the site name for the plot title.
#' @param point_shape Point shape. Passed to `value` in
#'   [ggplot2::scale_shape_manual()]. See `pch` values in [graphics::points()].
#' @param point_size Point size. Passed to `size` in [ggplot2::geom_point()].
#' @param tag_size Tag size. Passed to `size` in [ggrepel::geom_text_repel()].
#' @param header A string giving the plot header. An easy way to make a
#'   three-line header is with [get_header()].
#' @param theme A [ggplot2::theme()]. A theme customized for
#'   [plot_repulsive_tags()] is available via [get_theme()], which you can
#'   further customize or you can create a complete new theme with
#'   [ggplot2::theme()].
#'
#' @return A list of plots that can be wrapped around pdf(onefile = TRUE).
#' @export
#'
#' @examples
#' list_of_dataframes <- toy_list
#' prepared <- prep_repulsive_tags(list_of_dataframes)
#' plot_list <- lapply_plot_repulsive_tags(prepared, site_name = "Toy")
#' \dontrun{
#' plot_list[[1]]
#' }
#'
#' # Print each plot on a plage of a single .pdf file
#' \dontrun{
#' pdf(onefile = TRUE, paper = "a4", width = 11, height = 11)
#' plot_list
#' dev.off()
#' }
#' @name plot_repulsive_tags

#' @rdname plot_repulsive_tags
#' @export
lapply_plot_repulsive_tags <- function(prep_df_list,
                                       site_name = site_name,
                                       point_shape = c(19, 4),
                                       point_size = 1.5,
                                       tag_size = 3,
                                       header = get_header(),
                                       theme = get_theme()) {
  # Check that the data frame is in a list.
  assertive::assert_is_data.frame(prep_df_list[[1]])

  assertive::assert_is_character(site_name)

  lapply(
    X = prep_df_list,
    FUN = plot_repulsive_tags,
    site_name = site_name,
    point_shape = point_shape,
    point_size = point_size,
    tag_size = tag_size,
    header = header,
    theme = theme
  )
}

#' @rdname plot_repulsive_tags
#' @export
plot_repulsive_tags <- function(prep_df,
                                site_name,
                                point_shape,
                                point_size,
                                tag_size,
                                header,
                                theme) {
  assertive::assert_is_data.frame(prep_df)
  assertive::assert_is_character(site_name)

  id_quadrat_subquadrat <- unique(prep_df$id)
  ggplot(prep_df, aes(x = lx, y = ly, shape = latest_tree_status)) +
    scale_shape_manual(values = point_shape) +
    geom_point(size = point_size) +
    ggrepel::geom_text_repel(aes(label = tag), size = tag_size) +
    scale_x_continuous(minor_breaks = seq(1, 20, 1), breaks = seq(0, 20, 5)) +
    scale_y_continuous(minor_breaks = seq(1, 20, 1), breaks = seq(0, 20, 5)) +
    coord_fixed(
      xlim = c(unique(prep_df$x1), unique(prep_df$x2)),
      ylim = c(unique(prep_df$y1), unique(prep_df$y2))
    ) +
    # coord_fixed() +
    labs(x = NULL, y = NULL) +
    labs(
      title = paste0(site_name, ". ", id_quadrat_subquadrat),
      subtitle = header
    ) +
    theme
}
