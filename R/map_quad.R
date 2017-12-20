#' Map trees within a quadrat.
#'
#' @param vft A dataframe -- specifically, a ForestGEO ViewFullTable.
#' @param lim_min
#' @param lim_max
#' @param subquadrat_side
#' @param size_label
#' @param extend_grid
#' @param title_quad
#' @param header
#' @param theme
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
                     extend_grid = 0,
                     title_quad = "Site Name, YYYY, Quadrat:",
                     header = header_map_quad(),
                     theme = theme_map_quad()) {
  vft_lower_nms <- stats::setNames(vft, tolower(names(vft)))
  crucial_vars <- c("quadratname", "qx", "qy", "tagged_tag", "dbh_standarized")
  check_map_quad(
    crucial_vars = crucial_vars, vft = vft_lower_nms, lim_min = lim_min,
    lim_max = lim_max, subquadrat_side = subquadrat_side, size_label =
    size_label, extend_grid = extend_grid, title_quad = title_quad, header = header,
    theme = theme
  )
  # Remove useless vars
  vft_checked <- vft_lower_nms[crucial_vars]

  df_list <- split(vft_checked, vft_checked$quadratname)
  lapply(
    df_list, map_quad_each,
    lim_min = lim_min,
    lim_max = lim_max,
    subquadrat_side = subquadrat_side,
    size_label = size_label,
    extend_grid = extend_grid,
    title_quad = title_quad,
    header = header,
    theme = theme
  )
}

check_map_quad <- function(crucial_vars,
                           vft_lower_nms,
                           lim_min,
                           lim_max,
                           subquadrat_side,
                           size_label,
                           extend_grid,
                           title_quad,
                           header,
                           theme) {
  if (missing(vft_lower_nms)) stop("`vft` can't be missing")
  if (!is.data.frame(vft_lower_nms)) stop("`vft` should be a dataframe")
  stopifnot(
    is.numeric(lim_min), is.numeric(lim_max), is.numeric(subquadrat_side),
    is.numeric(size_label), is.numeric(extend_grid)
  )
  arg_theme_has_class_theme <- any(grepl("theme", class(theme)))
  stopifnot(arg_theme_has_class_theme)
  stopifnot(is.character(title_quad), is.character(header))
  check_crucial_names(vft_lower_nms, crucial_vars)
  check_single_plotid(vft_lower_nms)
  check_single_censusid(vft_lower_nms)
}

map_quad_each <- function(df,
                          lim_min,
                          lim_max,
                          subquadrat_side,
                          size_label,
                          extend_grid,
                          title_quad,
                          header,
                          theme) {
  # ggplots come with a default extention
  default_extention <- 1
  grid_adjust <- default_extention - extend_grid

  title_quad <- paste(title_quad, unique(df$quadratname), sep = " ")
  ggplot(df, aes(x = qx, y = qy)) +
    geom_text_repel(aes(label = tagged_tag), size = size_label) +
    geom_point(aes(size = dbh_standarized), shape = 1) +
    labs(title = title_quad, subtitle = header, x = NULL, y = NULL) +
    geom_vline(
      xintercept = seq(lim_min, lim_max, by = subquadrat_side),
      linetype = "dashed"
    ) +
    geom_hline(
      yintercept = seq(lim_min, lim_max, by = subquadrat_side),
      linetype = "dashed"
    ) +
    coord_fixed(
      xlim = c(lim_min + grid_adjust, lim_max - grid_adjust),
      ylim = c(lim_min + grid_adjust, lim_max - grid_adjust)
    ) +
    scale_x_continuous(breaks = lim_min:lim_max, sec.axis = dup_axis()) +
    scale_y_continuous(breaks = lim_min:lim_max, sec.axis = dup_axis()) +
    theme
}

# Helpers -----------------------------------------------------------------

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

