#' Map tree tags
#'
#' @param vft A ViewFullTable.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
map_tag <- function(vft,
                    dim_x = 20,
                    div_x = 5,
                    dim_y = dim_x,
                    div_y = div_x,
                    site_name = "Site Name, YYYY",
                    point_shape = c(19, 4),
                    point_size = 1.5,
                    tag_size = 3,
                    header = get_header(),
                    theme = get_theme()) {
  # Lowercase names (easier to handle), check and keep important variables
  vft_lower_nms <- setNames(vft, tolower(names(vft)))
  crucial_vars_only <- c("tag", "qx", "qy", "status", "quadratname")
  check_crucial_names(vft_lower_nms, crucial_vars_only)
  subset_with_lower_nms <- vft_lower_nms[crucial_vars_only]

  # Prepare data to plot: add important variables and remove duplicated tags
  with_subquadrat <- add_subquadrat(
    df = subset_with_lower_nms,
    dim_x = dim_x, dim_y = dim_y, div_x = div_x, div_y = div_y
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
    stop("Ensure your data has names ",
      glue::collapse(nms, ", ", last = ", and "), call. = FALSE)
  }
}

#' Add a quadrat variable to a dataframe based based on qx and qy coordinates.
#'
#' @param df A ViewFullTable dataframe.
#' @param dim_x,dim_y Quadrat dimension (side) for the plot. Commonly both are
#'   20.
#' @param div_x,div_y Total number of divisions of each quadrat dimension
#'   (side). For most plots, the value of these arguments is 5, which results in
#'   a 4x4 grid of subquadrats within each quadrat.
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
add_subquadrat <- function(df, dim_x, dim_y, div_x, div_y) {
  message("Lowering names case")
  df <- setNames(df, tolower(names(df)))
  check_crucial_names(df, c("qx", "qy"))

  check_add_subquadrat(
    df = df, dim_x = dim_x, dim_y = dim_y, div_x = div_x, div_y = div_y
  )

  # Simplify nested parentheses
  dim_x_mns.1 <- dim_x - 0.1
  dim_y_mns.1 <- dim_y - 0.1

  # Conditions (odd means that the coordinate goes beyond normal limits)
  is_odd_both <- df$qx >=  dim_x & df$qy >=  dim_y
  is_odd_x <- df$qx >=  dim_x
  is_odd_y <- df$qy >=  dim_y
  is_not_odd <- TRUE

  # Cases
  with_subquadrat <- dplyr::mutate(df,
    subquadrat = dplyr::case_when(
      is_odd_both ~ paste0(
        (1 + floor((dim_x_mns.1 - dim_x * floor(dim_x_mns.1 / dim_x)) / div_x)),
        (1 + floor((dim_y_mns.1- dim_y * floor(dim_y_mns.1/ dim_y)) / div_y))
      ),
      is_odd_x ~ paste0(
        (1 + floor((dim_x_mns.1 - dim_x * floor(dim_x_mns.1 / dim_x)) / div_x)),
        (1 + floor((df$qy - dim_y * floor(df$qy/ dim_y)) / div_y))
      ),
      is_odd_y ~ paste0(
        (1 + floor((df$qx - dim_x * floor(df$qx/ dim_x)) / div_x)),
        (1 + floor((dim_y_mns.1- dim_y * floor(dim_y_mns.1 / dim_y)) / div_y))
      ),
      is_not_odd ~ paste0(
        (1 + floor((df$qx - dim_x * floor(df$qx/ dim_x)) / div_x)),
        (1 + floor((df$qy - dim_y * floor(df$qy/ dim_y)) / div_y))
      )
    )
  )
  with_subquadrat
}

#' Help add_subquadrat()
#' @noRd
check_add_subquadrat <- function(df,
                                 dim_x,
                                 dim_y,
                                 div_x,
                                 div_y) {
  assertive::assert_is_data.frame(df)
  remaining_args <- list(dim_x, dim_y, div_x, div_y)

lapply(remaining_args, assertive::assert_is_numeric)
lapply(remaining_args, assertive::assert_is_of_length, 1)
lapply(remaining_args, assertive::assert_all_are_positive)
lapply(remaining_args, assertive::assert_all_are_finite)
}

#' Help map_tag()
#' @noRd
add_status_tree <- function(df) {
  # ensure that the status refers to the tree, not to the stem
  dplyr::mutate(
    dplyr::group_by(df, tag),
    status_tree = ifelse(any(status == "alive"), "alive", "dead")
  )
}

#' Help map_tag()
#' Prepare dataset for plotting, by adding a number of useful variables
#' @noRd
add_status_tree_page_x1_x2_y1_y2_split_quad_id <- function(with_subquadrat) {
    with_status_tree <- add_status_tree(df = with_subquadrat)
    paginated <- paginate(dplyr::group_by(with_status_tree, quadratname))
    with_limits <- add_subquad_limits(paginated)
    with_split_and_quad_id <- dplyr::mutate(
      with_limits,
      split = paste(quadratname, page, sep = "_"),
      quad_id = paste0("Q. ", quadratname)
    )
    # Remove variable with redudndant information
    dplyr::select(
      # Can't remove a grouping variable. Also, best to restore df to flat
      ungroup(with_split_and_quad_id),
      -quadratname
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
#' @noRd
plot_repulsive_tags <- function(prep_df,
                                site_name,
                                point_shape,
                                point_size,
                                tag_size,
                                header,
                                theme,
                                dim_x = 20,
                                dim_y = 20,
                                div_x = 5,
                                div_y = 5) {
  # Data to plot labels on map
  lab_df <- df_labels(dim_x = dim_x, dim_y = dim_y, div_x = div_x, div_y = div_y)
  # Allow plotting labels on a ggplot mapping to shape = status_tree
  lab_df$status_tree <- NA

  quad_id_label <- unique(prep_df$quad_id)
  ggplot2::ggplot(
    prep_df, ggplot2::aes(x = qx, y = qy, shape = status_tree)
  ) +
    ggplot2::scale_shape_manual(values = point_shape) +
    ggplot2::geom_label(data = lab_df, ggplot2::aes(qx, qy, label = subquadrat),
      colour = "white", fill = "#f4f2f2", fontface = "bold", size = 12
    ) +
    ggplot2::geom_point(size = point_size) +
    ggrepel::geom_text_repel(ggplot2::aes(label = tag), size = tag_size) +
    ggplot2::scale_x_continuous(
      minor_breaks = seq(1, dim_x, 1), breaks = seq(0, dim_x, div_x)
    ) +
    ggplot2::scale_y_continuous(
      minor_breaks = seq(1, dim_y, 1), breaks = seq(0, dim_y, div_y)
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
#' @noRd
#'
#' # Example
#' # dummy data
#' tags <- tibble(
#'   tag = sample(1:10000, 100),
#'   qx = sample(1:20, 100, replace = TRUE),
#'   qy = sample(1:20, 100, replace = TRUE)
#' )
#'
#' df_labs <- df_labels(dim_x = 20, dim_y = 20, div_x = 5, div_y = 5)
#'
#' ggplot(data = tags, aes(qx, qy)) +
#'   geom_label(data = df_labs, aes(qx, qy, label = subquadrat),
#'     colour = "white", fill = "grey", fontface = "bold") +
#'   ggrepel::geom_text_repel(aes(label = tag))
df_labels <- function(...) {
  pos <- position_labels(...)
  add_subquadrat(df = pos, ...)
}

#' Help df_labels()
#' Create a data set of positoin to which later add subquadrats.
#' @noRd
position_labels <- function(dim_x, dim_y, div_x, div_y) {
  # Center labels in each subquadrat
  # x
  xoffset <- div_x / 2
  xcentered <- seq(0, dim_x, div_x) + xoffset
  xtrimed <- xcentered[xcentered < dim_x]  # remove tags beyond the range
  # y
  yoffset <- div_y / 2
  ycentered <- seq(0, dim_y, div_y) + yoffset
  ytrimed <- ycentered[ycentered < dim_y]  # remove tags beyond the range

  df <- data.frame(qx = xtrimed, qy = ytrimed, stringsAsFactors = FALSE)
  tidyr::expand(df, qx, qy)
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
