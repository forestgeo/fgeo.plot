#' Map tree tags by status, showing four subquadrats per plot-page.
#'
#' This function maps tree tags by status. Each map shows four subquadrats
#' within a quadrat.
#'
#' @param vft A ViewFullTable.
#' @param site_name A string to use as a title.
#' @param point_shape A vector of two numbers giving the shape of the points to
#'   plot (see possible shapes in the documentation of ?[graphics::points()],
#'   under the section entitled _'pch' values_).
#' @param point_size A number giving points size. Passed to
#'   [ggplot2::geom_point()].
#' @param tag_size A number giving tag size. Passed to [ggplot2::geom_point()].
#' @param header A string to use as a header (subtitle). To conveniently create
#'   this header use [get_header()].
#' @param theme A [ggplot2::theme()]. To conveniently create this theme
#'   use [get_theme()].
#' @inheritParams add_subquadrat
#'
#' @seealso [graphics::points()], [ggplot2::geom_point()], [ggplot2::theme()]
#'   [get_header()], [get_theme()],
#'
#' @return A list of ggplots, where each element of the list is a map of tree
#'   tags by status, showing four subquadrats.
#' @export
#'
#' @examples
#' \dontrun{
#' }
map_tag <- function(vft,
                    x_q = 20,
                    x_sq = 5,
                    y_q = x_q,
                    y_sq = x_sq,
                    site_name = "Site Name, YYYY",
                    point_shape = c(19, 4),
                    point_size = 1.5,
                    tag_size = 3,
                    header = get_header(),
                    theme = get_theme()) {
  # Lowercase names (easier to handle), and check important variables
  vft_lower_nms <- stats::setNames(vft, tolower(names(vft)))
  crucial_vars_only <- c("tag", "qx", "qy", "status", "quadratname", "censusid")
  check_crucial_names(vft_lower_nms, crucial_vars_only)

  # Keep only: rows of last census, and variables that are important
  last_census <- max(unique(vft_lower_nms$censusid))
  is_last_census <- vft_lower_nms$censusid == last_census
  subset_with_lower_nms <- vft_lower_nms[is_last_census, crucial_vars_only]

  # Prepare data to plot: add important variables and remove duplicated tags
  with_subquadrat <- add_subquadrat(
    df = subset_with_lower_nms,
    x_q = x_q, y_q = y_q, x_sq = x_sq, y_sq = y_sq
  )
  with_crucial_vars <- add_status_tree_page_x1_x2_y1_y2_split_quad_id(
    with_subquadrat
  )
  data_to_plot <- unique(with_crucial_vars)

  # Iterate over each quadrat to produce one map per page (4 subquadrats/page)
  list_of_data_to_plot <- split(data_to_plot, data_to_plot$split)
  list_of_plots <- lapply_plot_repulsive_tags(
    list_of_data_to_plot,
    site_name = site_name, point_shape = point_shape, point_size = point_size,
    tag_size = tag_size, header = header, theme = theme
  )
  list_of_plots
}

#' Help lower_names_then_check()
#' @noRd
check_crucial_names <- function(x, nms) {
  are_names_expected <- all(nms %in% names(x))
  if (are_names_expected) {
    return(invisible())
  } else {
    stop("Ensure your data has names ", paste0(nms, collapse = ", "))
  }
}

#' Add a quadrat variable to a dataframe based based on qx and qy coordinates.
#'
#' @param df A ViewFullTable dataframe.
#' @param x_q,y_q Size in meters of a quadrat's side. For ForestGEO sites, a
#'   common value is 20.
#' @param x_sq,y_sq Size in meters of a subquadrat's side. For ForestGEO-CTFS sites, a
#'   common value is 5.
#' @return A dataframe with the additional variable `subquadrat`.
#' @author Anudeep Singh.
#' @export
#'
#' @examples
#' \dontrun{
#' # ngelnyaki is a private dataset
#' df <- ngelnyaki::ngelnyaki_vft_unid
#' with_subquadrat <- add_subquadrat(df, 20, 20, 5, 5)
#' head(with_subquadrat[c("qx", "qy", "subquadrat")])
#' }
add_subquadrat <- function(df, x_q, y_q, x_sq, y_sq) {
  message("Lowering names case")
  df <- stats::setNames(df, tolower(names(df)))
  check_crucial_names(df, c("qx", "qy"))

  check_add_subquadrat(
    df = df, x_q = x_q, y_q = y_q, x_sq = x_sq, y_sq = y_sq
  )

  # Simplify nested parentheses
  x_q_mns.1 <- x_q - 0.1
  y_q_mns.1 <- y_q - 0.1

  # Conditions (odd means that the coordinate goes beyond normal limits)
  is_odd_both <- df$qx >=  x_q & df$qy >=  y_q
  is_odd_x <- df$qx >=  x_q
  is_odd_y <- df$qy >=  y_q
  is_not_odd <- TRUE

  # Cases
  with_subquadrat <- dplyr::mutate(df,
    subquadrat = dplyr::case_when(
      is_odd_both ~ paste0(
        (1 + floor((x_q_mns.1 - x_q * floor(x_q_mns.1 / x_q)) / x_sq)),
        (1 + floor((y_q_mns.1- y_q * floor(y_q_mns.1/ y_q)) / y_sq))
      ),
      is_odd_x ~ paste0(
        (1 + floor((x_q_mns.1 - x_q * floor(x_q_mns.1 / x_q)) / x_sq)),
        (1 + floor((df$qy - y_q * floor(df$qy/ y_q)) / y_sq))
      ),
      is_odd_y ~ paste0(
        (1 + floor((df$qx - x_q * floor(df$qx/ x_q)) / x_sq)),
        (1 + floor((y_q_mns.1- y_q * floor(y_q_mns.1 / y_q)) / y_sq))
      ),
      is_not_odd ~ paste0(
        (1 + floor((df$qx - x_q * floor(df$qx/ x_q)) / x_sq)),
        (1 + floor((df$qy - y_q * floor(df$qy/ y_q)) / y_sq))
      )
    )
  )
  with_subquadrat
}

#' Help add_subquadrat()
#' @noRd
check_add_subquadrat <- function(df,
                                 x_q,
                                 y_q,
                                 x_sq,
                                 y_sq) {
  assertive::assert_is_data.frame(df)
  remaining_args <- list(x_q, y_q, x_sq, y_sq)

lapply(remaining_args, assertive::assert_is_numeric)
lapply(remaining_args, assertive::assert_is_of_length, 1)
lapply(remaining_args, assertive::assert_all_are_positive)
lapply(remaining_args, assertive::assert_all_are_finite)
}

#' Help map_tag()
#' Ensure that the status refers to the tree, not to the stem.
#' @noRd
add_status_tree <- function(df) {
  grouped <- dplyr::group_by(df, .data$tag)
  mutated_grouped <- dplyr::mutate(
    grouped,
    status_tree = ifelse(any(.data$status == "alive"), "alive", "dead")
  )
  dplyr::ungroup(mutated_grouped)
}

#' Help map_tag()
#' Prepare dataset for plotting, by adding a number of useful variables
#' @noRd
add_status_tree_page_x1_x2_y1_y2_split_quad_id <- function(with_subquadrat) {
    with_status_tree <- add_status_tree(df = with_subquadrat)
    paginated <- dplyr::ungroup(  # restore flat data
      paginate(dplyr::group_by(with_status_tree, .data$quadratname))
    )
    with_limits <- add_subquad_limits(paginated)
    with_split_and_quad_id <- dplyr::mutate(
      with_limits,
      split = paste(.data$quadratname, .data$page, sep = "_"),
      quad_id = paste0("Q. ", .data$quadratname)
    )
    # Remove variable with redudndant information
    dplyr::select(
      # Can't remove a grouping variable. Also, best to restore df to flat
      dplyr::ungroup(with_split_and_quad_id),
      -.data$quadratname
    )
  }

#' Paginate a ViewFullTable. Add a variable indicating page to map on.
#' @noRd
paginate <- function(df_with_subquadrat) {
  dplyr::mutate(
    df_with_subquadrat,
    page = dplyr::case_when(
        subquadrat == 11 ~ 1,
        subquadrat == 12 ~ 1,
        subquadrat == 21 ~ 1,
        subquadrat == 22 ~ 1,

        subquadrat == 31 ~ 2,
        subquadrat == 32 ~ 2,
        subquadrat == 41 ~ 2,
        subquadrat == 42 ~ 2,

        subquadrat == 34 ~ 3,
        subquadrat == 33 ~ 3,
        subquadrat == 44 ~ 3,
        subquadrat == 43 ~ 3,

        subquadrat == 14 ~ 4,
        subquadrat == 13 ~ 4,
        subquadrat == 24 ~ 4,
        subquadrat == 23 ~ 4,
      )
    )
}

#' Help map_tag()
#' @noRd
lapply_plot_repulsive_tags <- function(list_of_data_to_plot,
                                       site_name = site_name,
                                       point_shape = c(19, 4),
                                       point_size = 1.5,
                                       tag_size = 3,
                                       header = get_header(),
                                       theme = get_theme()) {
  check_lapply_plot_repulsive_tags(
    list_of_data_to_plot = list_of_data_to_plot, site_name = site_name,
    point_shape = point_shape, point_size = point_size, tag_size = tag_size,
    header = header, theme = theme
  )

  plot_list <- lapply(
    X = list_of_data_to_plot,
    FUN = plot_repulsive_tags,
    site_name = site_name, point_shape = point_shape, point_size = point_size,
    tag_size = tag_size, header = header, theme = theme
  )
  invisible(plot_list)
}

#' Help lapply_plot_repulsive_tags() by checking inputs.
#' @noRd
check_lapply_plot_repulsive_tags <- function(list_of_data_to_plot,
                                             site_name,
                                             point_shape,
                                             point_size,
                                             tag_size,
                                             header,
                                             theme) {
  assertive::assert_is_data.frame(list_of_data_to_plot[[1]])

  assertive::assert_is_character(header)
  assertive::assert_is_character(site_name)

  assertive::assert_is_numeric(point_shape)
  assertive::assert_is_numeric(point_size)
  assertive::assert_is_numeric(tag_size)

  assertive::assert_is_of_length(point_shape, 2)
  assertive::assert_is_of_length(point_size, 1)
  assertive::assert_is_of_length(tag_size, 1)
}

#' Help map_tag()
#' This funciton does the actual mapping.
#'
#' @param prep_df A data frame specifically prepared for this function.
#' @inheritParams map_tag
#' @noRd
plot_repulsive_tags <- function(prep_df,
                                site_name,
                                point_shape,
                                point_size,
                                tag_size,
                                header,
                                theme,
                                x_q = 20,
                                y_q = 20,
                                x_sq = 5,
                                y_sq = 5) {
  # Data to plot labels on map
  lab_df <- df_labels(x_q = x_q, y_q = y_q, x_sq = x_sq, y_sq = y_sq)
  # Allow plotting labels on a ggplot mapping to shape = status_tree
  lab_df$status_tree <- NA

  quad_id_label <- unique(prep_df$quad_id)
  ggplot2::ggplot(
    data = prep_df,
    ggplot2::aes(
      x = prep_df$qx, y = prep_df$qy, shape = unique(prep_df$status_tree)
    )
  ) +
    ggplot2::scale_shape_manual(values = point_shape) +
    ggplot2::geom_label(
      data = lab_df,
      ggplot2::aes(lab_df$qx, lab_df$qy, label = lab_df$subquadrat),
      colour = "white", fill = "#f4f2f2", fontface = "bold", size = 12
    ) +
    ggplot2::geom_point(size = point_size) +
    ggrepel::geom_text_repel(ggplot2::aes(label = prep_df$tag), size = tag_size) +
    ggplot2::scale_x_continuous(
      minor_breaks = seq(1, x_q, 1), breaks = seq(0, x_q, x_sq)
    ) +
    ggplot2::scale_y_continuous(
      minor_breaks = seq(1, y_q, 1), breaks = seq(0, y_q, y_sq)
    ) +
    ggplot2::coord_fixed(
      xlim = c(unique(prep_df$x1), unique(prep_df$x2)),
      ylim = c(unique(prep_df$y1), unique(prep_df$y2))
    ) +
    ggplot2::labs(
      title = paste0(site_name, ". ", quad_id_label),
      subtitle = header,
      x = NULL, y = NULL
    ) +
    theme
}

#' Help plot_repulsive_tags().
#' Create data to plot labels in each subquadrat.
#'
#' @examples
#' # dummy data
#' tags <- tibble(
#'   tag = sample(1:10000, 100),
#'   qx = sample(1:20, 100, replace = TRUE),
#'   qy = sample(1:20, 100, replace = TRUE)
#' )
#'
#' df_labs <- df_labels(x_q = 20, y_q = 20, x_sq = 5, y_sq = 5)
#'
#' ggplot(data = tags, aes(qx, qy)) +
#'   geom_label(data = df_labs, aes(qx, qy, label = subquadrat),
#'     colour = "white", fill = "grey", fontface = "bold") +
#'   ggrepel::geom_text_repel(aes(label = tag))
#'
#' @noRd
df_labels <- function(...) {
  pos <- position_labels(...)
  add_subquadrat(df = pos, ...)
}

#' Help df_labels()
#' Create a data set of positoin to which later add subquadrats.
#' @noRd
position_labels <- function(x_q, y_q, x_sq, y_sq) {
  # Center labels in each subquadrat
  # x
  xoffset <- x_sq / 2
  xcentered <- seq(0, x_q, x_sq) + xoffset
  xtrimed <- xcentered[xcentered < x_q]  # remove tags beyond the range
  # y
  yoffset <- y_sq / 2
  ycentered <- seq(0, y_q, y_sq) + yoffset
  ytrimed <- ycentered[ycentered < y_q]  # remove tags beyond the range

  expand.grid(qx = xtrimed, qy = ytrimed, stringsAsFactors = FALSE)
}


#' Help map_tag()
#' Add plot limits to a dataframe with `subquadrat` variable. Plots produced
#' with __ggplot__ by default print with a margin around the limits set by the
#' user. To remove that extra margin and maximize space, this function shrinks
#' the limits a little.
#' @param srink A number. By experience, `shrink = 0 ` maps default margins
#'   beyond the limits; `shink = 0.45` chops the margins. Anything in between
#'   should map a margin smaller than the default margin.
#' @noRd
add_subquad_limits <- function(df_with_page, quad_size = 20, shrink = 0.45) {
  dplyr::mutate(df_with_page,
    x1 = dplyr::case_when(
      page == 1 ~ 0 + shrink,
      page == 2 ~ (quad_size / 2) + shrink,
      page == 3 ~ (quad_size / 2) + shrink,
      page == 4 ~ 0 + shrink
    ),
    x2 = dplyr::case_when(
      page == 1 ~ (quad_size / 2) - shrink,
      page == 2 ~ quad_size - shrink,
      page == 3 ~ quad_size - shrink,
      page == 4 ~ (quad_size / 2) - shrink
    ),
    y1 = dplyr::case_when(
      page == 1 ~ 0 + shrink,
      page == 2 ~ 0 + shrink,
      page == 3 ~ (quad_size / 2) + shrink,
      page == 4 ~ (quad_size / 2) + shrink
    ),
    y2 = dplyr::case_when(
      page == 1 ~ (quad_size / 2) - shrink,
      page == 2 ~ (quad_size / 2) - shrink,
      page == 3 ~ quad_size  - shrink,
      page == 4 ~ quad_size - shrink
    )
  )
}
