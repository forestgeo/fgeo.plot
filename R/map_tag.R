#' Map tree tags by status, showing four subquadrats per plot-page.
#'
#' This function maps tree tags by status. Each map shows four subquadrats
#' within a quadrat.
#'
#' `map_tag()` plots a status that refers to the tree -- not to each individual
#' stem. For a tree to plot as "dead", all its stems must be dead (for the
#' selected census); else the tree will plot as "alive".
#'
#' From all censuses, this function will filter the one with greater numeric
#' value, and it will warn of such filtering. That is because most likely you
#' want information of the tree `Status` from the last census only. If this is
#' not what you want, here are some solutions:
#' * If you want to map a different census: filter the census you want and feed
#'  `map_tag()` with the filtered data set.
#' * If you want to lump trees accross multiple censuses, filter all the threes
#' that you want and change the value of `CensusID` so that all trees have the
#' same value of `CensusID`. Then feed `map_tag()` with the filtered data set.
#'
#' @template vft
#' @template title_quad
#' @param point_shape A vector of two numbers giving the shape of the points to
#'   plot (see possible shapes in the documentation of ?[graphics::points()],
#'   under the section entitled _'pch' values_).
#' @param point_size A number giving points size. Passed to
#'   [ggplot2::geom_point()].
#' @template tag_size
#' @template header
#' @template theme
#' @template extend_grid
#' @inheritParams add_subquadrat
#'
#' @seealso [graphics::points()], [ggplot2::geom_point()], [ggplot2::theme()]
#'   [map_tag_header()], [theme_map_tag()], [add_subquadrat()],
#'   [ggrepel::geom_text_repel].
#'
#' @section Acknowledgements:
#' Useful ideas and guidance came from Suzanne Lao, Stuart Davis, Shameema
#' Jafferjee Esufa, David Kenfack and Anudeep Singh. Andudeep Sinh also wrote
#' the algorithm to calculate subquadrats.
#'
#' @return A list of ggplots, where each element of the list is a map of tree
#'   tags by status, showing four subquadrats.
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(ggplot2)
#' library(map)
#' library(bciex)
#'
#' # Subset of a public ViewFullTable from BCI (source:
#' # https://repository.si.edu/handle/10088/20925).
#'
#' # Improve printing method
#' vft <- as_tibble(bci12vft_mini)
#' vft
#'
#' # Filter the plot you want to map
#' vft1 <- dplyr::filter(vft, PlotID == 1)
#'
#' # Another look
#' glimpse(vft1)
#'
#' # This data set has two wrong names that need to be fixed before using map_tag()
#' vft1_rnm <- dplyr::rename(vft1, qx = x, qy = y)
#' maps <- map_tag(vft1_rnm)
#'
#' # Plotting only one map to screen
#' maps[1]
#'
#' # Printing only maps 1-4 to a .pdf
#' pdf("default-map.pdf", paper = "letter", height = 10.5, width = 8)
#' maps[1:4]
#' dev.off()
#'
#' # Common customization (printing only 1 map to screen)
#' map_tag(vft1_rnm,
#'   title_quad = "BCI 2012", point_size = 3, point_shape = c(17, 6), tag_size = 5
#' )[1]
#'
#' # Custom header
#' map_tag(vft1_rnm, title_quad = "BCI 2012",
#'   header = "Line 1: _________\nLine 2:\nLine 3:....................."
#' )[1]
#'
#' # Or use a pre-made header
#' map_tag(vft1_rnm, title_quad = "BCI 2012", header = map_tag_header())[1]
#'
#' # Themes
#'
#' # Using a pre-made theme from ggplot2
#' library(ggplot2)
#' map_tag(vft1_rnm, title_quad = "BCI 2012", theme = theme_gray())[1]
#'
#' # Customizing the default theme of map_tag()
#' theme_small_change <- theme_map_tag(legend.position = "bottom")
#' map_tag(vft1_rnm, title_quad = "BCI 2012", theme = theme_small_change)[1]
#'
#' # Customizing the default theme extreemely, to show flexibility
#' theme_extreeme_change <- ggplot2::theme(
#'   legend.position = "bottom",
#'   legend.title = element_blank(),
#'   legend.text = element_text(size = 8, colour = "red"),
#'   text = element_text(size = 11, face = "bold.italic", colour = "white"),
#'   plot.background = element_rect(fill = "black"),
#'   plot.margin = margin(2, 2, 2, 2, "cm"),
#'   strip.background = element_rect(fill = "darkgreen"),
#'   strip.text = element_text(colour = "white"),
#'   # make grid to dissapear by matching background colour
#'   panel.background = element_rect(fill = "lightgreen"),
#'   panel.grid.minor = element_line(colour = "black", linetype = "dotted"),
#'   panel.grid.major = element_line(colour = "black")
#' )
#' map_tag(vft1_rnm, title_quad = "BCI 2012", theme = theme_extreeme_change)[1]
#'
#' # Adapting the dimensions of quadrat and subquadrat to the range of qx and qy
#' # Creating new data set with qx and qy ranging 0-100
#' smaller <- vft1_rnm
#' n <- nrow(smaller)
#' smaller$qx <- sample(0:10, n, replace = TRUE)
#' smaller$qy <- sample(0:10, n, replace = TRUE)
#'
#' map_tag(smaller, x_q = 10, x_sq = 2.5)[1]
#' # If limit-lines aren't visible, try extending the grid a little
#' map_tag(smaller, x_q = 10, x_sq = 2.5, extend_grid = 0.25)[1]
#' }
map_tag <- function(vft,
                    x_q = 20,
                    x_sq = 5,
                    y_q = x_q,
                    y_sq = x_sq,
                    title_quad = "Site Name, YYYY. Quadrat:",
                    point_shape = c(19, 4),
                    point_size = 1.5,
                    tag_size = 3,
                    header = map_tag_header(),
                    theme = theme_map_tag(),
                    extend_grid = 0) {
  # Lowercase names: avoid errors due to confusing upper- and lower-case
  vft_lower_nms <- stats::setNames(vft, tolower(names(vft)))
  crucial_vars <- c(
    "tag", "qx", "qy", "status", "quadratname", "censusid", "plotid"
  )
  check_crucial_names(vft_lower_nms, crucial_vars)
  check_unique_plotid(vft_lower_nms)
  check_subquadrat_dimensions(
    df = vft_lower_nms, x_q = x_q, y_q = y_q, x_sq = x_sq, y_sq = y_sq
  )

  # Keep only rows of last census
  if (length(unique(vft_lower_nms$censusid)) > 1) {
    warning(
      "Multiple censuses were detected\n",
      "  * Filtering only the greatest `CensusID`"
    )
  }
  last_census <- max(unique(vft_lower_nms$censusid))
  is_last_census <- vft_lower_nms$censusid == last_census

  # Keep only variables that are important
  subset_with_lower_nms <- vft_lower_nms[is_last_census, crucial_vars]

  # Prepare data to plot: add important variables and remove duplicated tags
  with_subquadrat <- add_sbqd(
    df = subset_with_lower_nms,
    x_q = x_q, y_q = y_q, x_sq = x_sq, y_sq = y_sq
  )
  with_crucial_vars <- add_status_tree_page_x1_x2_y1_y2_split_quad_id(
    with_subquadrat, quad_size = x_q, extend_grid = extend_grid
  )
  data_to_plot <- unique(with_crucial_vars)

  # Iterate over each quadrat to produce one map per page (4 subquadrats/page)
  list_of_data_to_plot <- split(data_to_plot, data_to_plot$split)
  list_of_plots <- lapply_plot_repulsive_tags(
    list_of_data_to_plot,
    x_q = x_q, x_sq = x_sq, y_q = y_q, y_sq = y_sq,
    title_quad = title_quad, point_shape = point_shape, point_size = point_size,
    tag_size = tag_size, header = header, theme = theme
  )
  list_of_plots
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
# Fixing wrong names
#' vft <- dplyr::rename(bciex::bci12vft_mini, QX = x, QY = y)
#' with_subquadrat <- add_subquadrat(vft, 20, 20, 5, 5)
#' with_subquadrat[c("subquadrat", "qx", "qy")]
add_subquadrat <- function(df, x_q, y_q = x_q, x_sq, y_sq = x_sq) {
  message("Lowering names case")
  df <- stats::setNames(df, tolower(names(df)))
  check_crucial_names(df, c("qx", "qy"))
  check_subquadrat_dimensions(
    df = df, x_q = x_q, y_q = y_q, x_sq = x_sq, y_sq = y_sq
  )
  add_sbqd(df = df, x_q = x_q, y_q = y_q, x_sq = x_sq, y_sq = y_sq)
}

#' Help map_tag()
#'
#' This function is to be called by map_tag(); it has no checks, which saves
#' checking again and again the data of each quadrat that was checked once at
#' the start of map_tag(). For using directly, use add_subquadrat()
#' @noRd
add_sbqd <- function(df, x_q, y_q, x_sq, y_sq) {
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
check_subquadrat_dimensions <- function(df,
                                        x_q,
                                        y_q,
                                        x_sq,
                                        y_sq) {
  stopifnot(is.data.frame(df))
  remaining_args <- list(x_q, y_q, x_sq, y_sq)

  lapply(remaining_args, function(x) stopifnot(is.numeric(x)))
  lapply(remaining_args, function(x) stopifnot(length(x) == 1))
  lapply(remaining_args, function(x) stopifnot(all(x >= 0)))
  lapply(remaining_args, function(x) stopifnot(all(abs(x) != Inf)))
}

#' Help map_tag()
#' Prepare dataset for plotting, by adding a number of useful variables
#' @noRd
add_status_tree_page_x1_x2_y1_y2_split_quad_id <- function(with_subquadrat,
                                                           quad_size,
                                                           extend_grid) {
    with_status_tree <- add_status_tree(df = with_subquadrat)
    paginated <- dplyr::ungroup(  # restore flat data
      paginate(dplyr::group_by(with_status_tree, .data$quadratname))
    )
    with_limits <- add_subquad_limits(
      df_with_page = paginated,
      quad_size = quad_size, extend_grid = extend_grid
    )
    with_split_and_quad_id <- dplyr::mutate(
      with_limits,
      split = paste(.data$quadratname, .data$page, sep = "_"),
      quad_id = .data$quadratname
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
                                       x_q = x_q,
                                       x_sq = x_sq,
                                       y_q = x_q,  # same as x
                                       y_sq = x_sq,  # same as x
                                       title_quad = title_quad,
                                       point_shape = c(19, 4),
                                       point_size = 1.5,
                                       tag_size = 3,
                                       header = map_tag_header(),
                                       theme = theme_map_tag()) {
  check_lapply_plot_repulsive_tags(
    list_of_data_to_plot = list_of_data_to_plot, title_quad = title_quad,
    point_shape = point_shape, point_size = point_size, tag_size = tag_size,
    header = header, theme = theme
  )

  plot_list <- lapply(
    X = list_of_data_to_plot,
    FUN = plot_repulsive_tags,
    x_q = x_q, x_sq = x_sq, y_q = y_q, y_sq = y_sq,
    title_quad = title_quad, point_shape = point_shape, point_size = point_size,
    tag_size = tag_size, header = header, theme = theme
  )
  invisible(plot_list)
}

#' Help lapply_plot_repulsive_tags() by checking inputs.
#' @noRd
check_lapply_plot_repulsive_tags <- function(list_of_data_to_plot,
                                             title_quad,
                                             point_shape,
                                             point_size,
                                             tag_size,
                                             header,
                                             theme) {
  stopifnot(is.data.frame(list_of_data_to_plot[[1]]))

  stopifnot(is.character(header))
  stopifnot(is.character(title_quad))

  stopifnot(is.numeric(point_shape))
  stopifnot(is.numeric(point_size))
  stopifnot(is.numeric(tag_size))

  stopifnot(length(point_shape) == 2)
  stopifnot(length(point_size) == 1)
  stopifnot(length(tag_size) == 1)
}

#' Help map_tag()
#' This funciton does the actual mapping.
#'
#' @param prep_df A data frame specifically prepared for this function.
#' @inheritParams map_tag
#' @noRd
plot_repulsive_tags <- function(prep_df,
                                title_quad,
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

  ggplot(
    data = prep_df,
    # /* ********************************************************************
    # If in this chunk I refer to `var` as `df$var` I get this error:
    #   Error: Aesthetics must be either length 1 or the same as the data (16):
    #   x, y, label, shape
    aes(x = qx, y = qy, shape = status_tree)
  ) +
    scale_shape_manual(values = point_shape) +
    geom_label(
      data = lab_df,
      aes(qx, qy, label = subquadrat),
      colour = "white", fill = "#f4f2f2", fontface = "bold", size = 12
    ) +
    # */ ********************************************************************
    geom_point(size = point_size) +
    ggrepel::geom_text_repel(aes(label = prep_df$tag), size = tag_size) +
    scale_x_continuous(
      minor_breaks = seq(1, x_q, 1), breaks = seq(0, x_q, x_sq)
    ) +
    scale_y_continuous(
      minor_breaks = seq(1, y_q, 1), breaks = seq(0, y_q, y_sq)
    ) +
    coord_fixed(
      xlim = c(unique(prep_df$x1), unique(prep_df$x2)),
      ylim = c(unique(prep_df$y1), unique(prep_df$y2))
    ) +
    labs(
      # title = paste0(title_quad, quad_id_label),
      title =  paste(title_quad, unique(prep_df$quad_id), sep = " "),
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
  add_sbqd(df = pos, ...)
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
#' Add plot limits to a dataframe with `subquadrat` variable.

#' @param extend_grid A number to extend the grid beyond the plot limits.
#' @noRd
add_subquad_limits <- function(df_with_page, quad_size = 20, extend_grid = 0) {
  # ggplots come with a default extention
  default_extention <- 0.45
  grid_adjust <- default_extention - extend_grid

  dplyr::mutate(df_with_page,
    x1 = dplyr::case_when(
      page == 1 ~ 0 + grid_adjust,
      page == 2 ~ (quad_size / 2) + grid_adjust,
      page == 3 ~ (quad_size / 2) + grid_adjust,
      page == 4 ~ 0 + grid_adjust
    ),
    x2 = dplyr::case_when(
      page == 1 ~ (quad_size / 2) - grid_adjust,
      page == 2 ~ quad_size - grid_adjust,
      page == 3 ~ quad_size - grid_adjust,
      page == 4 ~ (quad_size / 2) - grid_adjust
    ),
    y1 = dplyr::case_when(
      page == 1 ~ 0 + grid_adjust,
      page == 2 ~ 0 + grid_adjust,
      page == 3 ~ (quad_size / 2) + grid_adjust,
      page == 4 ~ (quad_size / 2) + grid_adjust
    ),
    y2 = dplyr::case_when(
      page == 1 ~ (quad_size / 2) - grid_adjust,
      page == 2 ~ (quad_size / 2) - grid_adjust,
      page == 3 ~ quad_size  - grid_adjust,
      page == 4 ~ quad_size - grid_adjust
    )
  )
}
