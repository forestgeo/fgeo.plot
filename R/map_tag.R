#' Title
#'
#' @param vft A ViewFullTable -- a subclass of a dataframe.
#'
#' @return
#' @export
#'
#' @examples
map_tag <- function(vft,
                    dim_x = 20,
                    div_x = 5,
                    dim_y = dim_x,
                    div_y = div_x,
                    site_name = "Site Name, YYYY") {
  # Lowercase names
  vft2 <- setNames(vft, tolower(names(vft)))
  # Check names
  nms <- c("tag", "qx", "qy", "status")
  check_crucial_names(vft2, nms)

  with_subquadrat <- add_subquadrat(df = vft2,
    dim_x = dim_x, dim_y = dim_y, div_x = div_x, div_y = div_y
  )


  # xxx continue refactoring here

  renamed <- dplyr::rename(
    with_subquadrat,
    subquadrat_vftbl = subquadrat,
    quadrat_vftbl = quadratname,
    lx = qx,
    ly = qy,
  )

  with_status <- renamed %>%
    dplyr::group_by(tag) %>%
    dplyr::arrange(.by_group = TRUE) %>%
    # ensure that the status refers to the tree, not to the stem
    dplyr::mutate(status_tree = ifelse(any(status == "alive"), "alive", "dead")) %>%
    dplyr::select(dbhid, tag, status, status_tree, everything())

  with_symbol <- with_status %>%
    mutate(symbol = ifelse(status_tree == "alive", 19, 4)) %>%
    select(status_tree, symbol, everything())

  # xxx maybe I can avoid splitting and work with df?
  splitted <- with_symbol %>% split(.$quadrat_vftbl)

  # Prepare.
  # xxx maybe I can remove useless variables considering the next step
  # maybe I can remove duplicated tabs, considering the next step
  prep_list <- prep_repulsive_tags(splitted)

  # Keep minimum data and remove duplicates
  unique_tags <- prep_list %>%
    purrr::map(select, tag, lx, ly, status, id, latest_tree_status, x1, x2, y1, y2) %>%
    purrr::map(unique)

  plot_list <- unique_tags %>%
  lapply_plot_repulsive_tags(site_name = site_name)
  plot_list
}

check_crucial_names <- function(x, nms) {
  are_names_expected <- all(nms %in% names(x))
  if (are_names_expected) {
    return(invisible())
  } else {
    stop("Ensure your data has names ",
      glue::collapse(nms, ", ", last = ", and "), call. = FALSE)
  }
}

# From add_subquadrat.R ---------------------------------------------------

#' Add a quadrat variable to a dataframe based based on qx and qy coordinates.
#'
#' @param df A dataframe.
#' @param dim_x,dim_y Quadrat dimension for the plot. For example, in Sinharaja
#'   both are 20.
#' @param div_x,div_y Total number of divisions of each quadrat side. For
#'   example, For Sinharaja and most other plots, the value of these arguments
#'   is 5, which results in a 4x4 grid of subquadrats within each quadrat.
#' @return A dataframe with the additional variable `subquadrat`.
#' @author Anudeep Singh.
#' @export
#'
#' @examples
#' \dontrun{
#' # sinharaja is a private package
#' df <- sinharaja::sinh_vftbl_selected
#' with_subquadrat <- add_subquadrat(df, 20, 20, 5, 5)
#' head(with_subquadrat)
#' }
add_subquadrat <- function(df, dim_x, dim_y, div_x, div_y) {
  # Simplify nested parentheses
  dim_x_mns.1 <- dim_x - 0.1
  dim_y_mns.1 <- dim_y - 0.1

  # Conditions
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


#' Paginate a ViewFullTable. Add a variable indicating page to map on.
#'
#' @param x A ViewFullTable dataframe.
#'
#' @return A modified ViewFullTable dataframe..
#' @export
#' @keywords internal
#' @noRd
paginate <- function(x) {
  dplyr::mutate(x, subquadrat =
      case_when(
        subquadrat_vftbl == 11 ~ 1,
        subquadrat_vftbl == 12 ~ 1,
        subquadrat_vftbl == 21 ~ 1,
        subquadrat_vftbl == 22 ~ 1,

        subquadrat_vftbl == 31 ~ 2,
        subquadrat_vftbl == 32 ~ 2,
        subquadrat_vftbl == 41 ~ 2,
        subquadrat_vftbl == 42 ~ 2,

        subquadrat_vftbl == 34 ~ 3,
        subquadrat_vftbl == 33 ~ 3,
        subquadrat_vftbl == 44 ~ 3,
        subquadrat_vftbl == 43 ~ 3,

        subquadrat_vftbl == 14 ~ 4,
        subquadrat_vftbl == 13 ~ 4,
        subquadrat_vftbl == 24 ~ 4,
        subquadrat_vftbl == 23 ~ 4,
      )
    )
}


#' Add variable sqds
#'
#' @param df A ViewFullTable dataframe with the variable subquadrat.
#'
#' @return Modified version of input.
#' @export
#' @keywords internal
#' @noRd
add_sqds <- function(df) {
  paginate(df) %>%
  dplyr::group_by(subquadrat_vftbl) %>%
  dplyr::mutate(sqds = paste0(unique(sort(subquadrat_vftbl)), collapse = "-")) %>%
  dplyr::ungroup() %>%
  dplyr::select(sqds, everything())
}


#' Prepare a list of dataframes to later plot repulsive tags.
#'
#' @param df_list A list of dataframes
#'
#' @return A modified version of the input.
#' @keywords internal
#' @export
#' @noRd
prep_repulsive_tags <- function(df_list) {
  x <- df_list %>%
    purrr::map(add_sqds)
  x %>%
    purrr::map(add_latest_tree_status) %>%  # fix this function
    purrr::map(dplyr::mutate, latest_tree_status = status_tree)  %>% # patch
    purrr::map(paginate) %>%
    purrr::map(dplyr::rename, quadrat = quadrat_vftbl) %>%
    purrr::map(add_subquad_limits) %>%
    purrr::map(dplyr::mutate,
      id = paste0("Q. ", quadrat, " SQ. ", sqds, " (p. ", subquadrat_vftbl, ")")
    ) %>%
    purrr::map(dplyr::select, id, subquadrat_vftbl, dplyr::everything()) %>%
    purrr::map(dplyr::select,
      id, tag, lx, ly, latest_tree_status, x1, x2, y1, y2, dplyr::everything()
    ) %>%
    purrr::reduce(dplyr::full_join) %>%
    # Add status to tag because some points dissapear from plot but tags
    # persist
    dplyr::mutate(
      tag = case_when(
        latest_tree_status == "alive" ~ paste0(tag, "_"),
        latest_tree_status == "dead" ~ paste0(tag, ".")
      )
    ) %>%
    split(., .$id)
}

# From plot.R -------------------------------------------------------------

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

  plot_list <- lapply(
    X = prep_df_list,
    FUN = plot_repulsive_tags,
    site_name = site_name,
    point_shape = point_shape,
    point_size = point_size,
    tag_size = tag_size,
    header = header,
    theme = theme
  )
  invisible(plot_list)
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
  ggplot2::ggplot(
    prep_df, ggplot2::aes(x = lx, y = ly, shape = latest_tree_status)
  ) +
    ggplot2::scale_shape_manual(values = point_shape) +
    ggplot2::geom_point(size = point_size) +
    ggrepel::geom_text_repel(ggplot2::aes(label = tag), size = tag_size) +
    ggplot2::scale_x_continuous(
      minor_breaks = seq(1, 20, 1), breaks = seq(0, 20, 5)
    ) +
    ggplot2::scale_y_continuous(
      minor_breaks = seq(1, 20, 1), breaks = seq(0, 20, 5)
    ) +
    ggplot2::coord_fixed(
      xlim = c(unique(prep_df$x1), unique(prep_df$x2)),
      ylim = c(unique(prep_df$y1), unique(prep_df$y2))
    ) +
    # coord_fixed() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::labs(
      title = paste0(site_name, ". ", id_quadrat_subquadrat),
      subtitle = header
    ) +
    theme
}
