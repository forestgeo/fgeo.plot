#' Title
#'
#' @param vft A ViewFullTable -- a subclass of a dataframe.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # Yosemite
#' yose <- as_tibble(yosemite::ViewFullTable_yosemite)
#' yose_quads <- unique(yose$QuadratName)[1:2]
#' yose_to_map <- yose %>% filter(QuadratName %in% yose_quads)
#'
#' p <- map_tag(yose_to_map, site_name = "Yosemite 2017")
#' pdf("Yose.pdf", paper = "letter", width = 8, height = 11)
#' p
#' dev.off()
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
  crucial_vars_only <- c("tag", "qx", "qy", "status", "dbhid", "quadratname")
  vft2 <- lower_names_then_check(vft, nms = crucial_vars_only)
  # Save memmory by removing irrelevant variables
  vft2 <- vft2[crucial_vars_only]

  with_subquadrat <- add_subquadrat(
    df = vft2, dim_x = dim_x, dim_y = dim_y, div_x = div_x, div_y = div_y
  )
  with_status_tree <- add_status_tree(with_subquadrat)
  with_symbol <- add_symbol(with_status_tree)
  # Prepare.
  # maybe I can remove duplicated tags, considering the next step
  prep <- prep_repulsive_tags(with_symbol)
  unique_tags <- discard_duplicated_tags(prep)
  unique_tags_list <- split(unique_tags, unique_tags$split)
  plot_list <- lapply_plot_repulsive_tags(
    unique_tags_list,
    site_name = site_name, point_shape = point_shape, point_size = point_size,
    tag_size = tag_size, header = header, theme = theme
  )
  plot_list
}

lower_names_then_check <- function(x, nms) {
  # check names
  x <- setNames(x, tolower(names(x)))
  check_crucial_names(x, nms)
  x
}

#' helps lower_names_then_check
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

#' Help map_tag()
#' @noRd
add_status_tree <- function(renamed) {
  renamed %>%
    dplyr::group_by(tag) %>%
    dplyr::arrange(.by_group = TRUE) %>%
    # ensure that the status refers to the tree, not to the stem
    dplyr::mutate(status_tree = ifelse(any(status == "alive"), "alive", "dead")) %>%
    dplyr::select(dbhid, tag, status, status_tree, everything())
}

#' Help map_tag()
#' @noRd
add_symbol <- function(with_status_tree){
  with_status_tree %>%
  mutate(symbol = ifelse(status_tree == "alive", 19, 4)) %>%
  select(status_tree, symbol, everything())
}

#' Help map_tag(); Keep minimum data and remove duplicates.
#' @noRd
discard_duplicated_tags <- function(prep_df) {
  unique(
    dplyr::select(
      prep_df,
      split, tag, status, id, status_tree, page, qx, qy, x1, x2, y1, y2
    )
  )
}



# From add_subquadrat.R ---------------------------------------------------



#' Paginate a ViewFullTable. Add a variable indicating page to map on.
#'
#' @param x A ViewFullTable dataframe.
#'
#' @return A modified ViewFullTable dataframe..
#' @export
#' @keywords internal
#' @noRd
paginate <- function(x) {
  dplyr::mutate(x, page =
      case_when(
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
  dplyr::group_by(subquadrat) %>%
  dplyr::mutate(sqds = paste0(unique(sort(subquadrat)), collapse = "-")) %>%
  dplyr::ungroup() %>%
  dplyr::select(sqds, everything())
}


#' Prepare a list of dataframes to later plot repulsive tags.
#'
#' @param df A dataframes
#'
#' @return A modified version of the input.
#' @keywords internal
#' @export
#' @noRd
prep_repulsive_tags <- function(df) {
  df %>%
    dplyr::group_by(quadratname) %>%
    add_sqds() %>%
    paginate() %>%
    add_subquad_limits() %>%
    dplyr::mutate(
      split = paste(quadratname, page, sep = "_"),
      id = paste0("Q. ", quadratname)
    )
}

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

#' Help map_tag() by iterating over the list of data and passing each split
#' to plot_repulsive_tags()
#' @noRd
lapply_plot_repulsive_tags <- function(prep_df_list,
                                       site_name = site_name,
                                       point_shape = c(19, 4),
                                       point_size = 1.5,
                                       tag_size = 3,
                                       header = get_header(),
                                       theme = get_theme()) {
  check_lapply_plot_repulsive_tags(
    prep_df_list = prep_df_list, site_name = site_name,
    point_shape = point_shape, point_size = point_size, tag_size = tag_size,
    header = header, theme = theme
  )

  plot_list <- lapply(
    X = prep_df_list,
    FUN = plot_repulsive_tags,
    site_name = site_name, point_shape = point_shape, point_size = point_size,
    tag_size = tag_size, header = header, theme = theme
  )
  invisible(plot_list)
}

#' Help lapply_plot_repulsive_tags() by checking inputs.
#' @noRd

check_lapply_plot_repulsive_tags <- function(prep_df_list,
                                             site_name,
                                             point_shape,
                                             point_size,
                                             tag_size,
                                             header,
                                             theme) {
  assertive::assert_is_data.frame(prep_df_list[[1]])
  assertive::assert_is_character(site_name)
  assertive::assert_is_numeric(point_shape)
  assertive::assert_is_of_length(point_shape, 2)
  assertive::assert_is_numeric(point_size)
  assertive::assert_is_of_length(point_size, 1)
  assertive::assert_is_numeric(tag_size)
  assertive::assert_is_of_length(tag_size, 1)
  assertive::assert_is_character(header)
}

#' Help map_tag() by doing the actual mapping.
#'
#' Hide because the data to be passed must be first prepared. That preparation
#' is made by other functions, also warpped in map_tag(). This means that users
#' won't be able to plot with a data set that has not first been prepared by
#' the functions composed inside map_tag().
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
  lab_df <- df_labels(dim_x = dim_x, dim_y = dim_y, div_x = div_x, div_y= div_y)
  # Allow plotting labels on a ggplot mapping to shape = status_tree
  lab_df$status_tree <- NA

  id_quadrat_subquadrat <- unique(prep_df$id)
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
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::labs(
      # Title
      title = paste0(
        site_name, ". ",
        id_quadrat_subquadrat,
        " (page ", as.character(prep_df$page), ")"
      ),
      # Subtitle
      subtitle = header
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
  xpushed <- seq(0, dim_x, div_x) + xoffset
  xcentered <- xpushed[xpushed < dim_x]  # remove tags beyond the range
  # y
  yoffset <- div_y / 2
  ypushed <- seq(0, dim_y, div_y) + yoffset
  ycentered <- ypushed[ypushed < dim_y]  # remove tags beyond the range

  df <- data.frame(qx = xcentered, qy = ycentered, stringsAsFactors = FALSE)
  tidyr::expand(df, qx, qy)
}




























#' To a dataframe with `subquadrat` variable, add plot limits.
#'
#' This function helps fine tune the limits of your plots. You can live withouth
#' it, but this function gives you some extra control. The output is a modified
#' dataframe that includes the limits that, via [plot_repulsive_tags()], will be
#' passed to the arguments `xlim` and `ylim` of [ggplot2::coord_fixed()].
#'
#' Plots produced with __ggplot__ by default print with a margin around the
#' limits set by the user. To remove that extra margin and maximize space, this
#' function shrinks the limits a little. Be sure not to shrink so much that
#' you loose data; The numbers on the x and y axes should include the limits
#' that you expect. For example, if your plot is 20x20 meters, each subplot
#' will be 10x10 meters, so your plot axes should show either the 0 to 10, or
#' 10 to 20. If you do not read 0 and/or 10, or 10 and/or 20, your plot may not
#' show the data you expect.
#'
#' @family functions to prepare data to plot repulsive tags.
#'
#' @param df_with_subquad A dataframe with the variable `subquadrat` that
#'   defines the 1-4 subquadrats within each quadrat.
#' @param quad_size Size of each quadrat.
#' @param shrink A number, generally smaller than one, giving how much to
#'   shrink the plot.
#'
#' @return A modified data frame.
#' @export
#' @examples
#' \dontrun{
#' # not running because this example may be obsolete
#' library(dplyr)
#'
#' # Showing only 1 quadrat to save space
#' with_subquad_list <- toy_list[1] %>%
#'   add_quadrat_and_subquadrat_from_list()
#' str(with_subquad_list)
#'
#' # Pulling only one dataframe
#' with_subquad_df <- with_subquad_list[[1]][[1]]
#' head(with_subquad_df)
#'
#' with_subquad_df %>%
#'   # The only "must be" is the variable `subquadrat`; we could remove `quadratname`
#'   select(-quadratname) %>%
#'   add_subquad_limits(quad_size = 20) %>%
#'   head()
#' }
add_subquad_limits <- function(df_with_subquad, quad_size = 20, shrink = 0.45) {
  dplyr::mutate(df_with_subquad,
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
