#' Add a quadrat variable to a dataframe based based on QX and QY coordinates.
#'
#' @param df A dataframe.
#' @param dim_x,dim_y Quadrat dimension for the plot. For example, in Sinharaja
#'   both are 20.
#' @param divide_x,divide_y Total number of divisions of each quadrat side. For
#'   example, For Sinharaja and most other plots, the value of these arguments
#'   is 5, which results in a 4x4 grid of subquadrats within each quadrat.
#' @return A dataframe with the additional variable `subquadrat`.
#' @export
#'
#' @examples
#' df <- sinharaja::sinh_vftbl_selected
#' with_subquadrat <- add_subquadrat(df, 20, 20, 5, 5)
#' head(with_subquadrat)
add_subquadrat <- function(df, dim_x, dim_y, divide_x, divide_y) {
  # Define conditions and actions for each case
  # (Each chunk answers: what coordinate is at or over the edge of the quadrat?)

  # Both x and y.
  are_both_xy_at_or_over_quadrat_edge <- df$QX >= dim_x & df$QY >= dim_y
  col_both_xy_at_or_over_quadrat_edge <- floor(
    (dim_x - 0.1 - dim_x * floor(dim_x - 0.1 / dim_x)) / divide_x
  )
  row_both_xy_at_or_over_quadrat_edge <- floor(
    (dim_y - 0.1 - dim_y * floor(dim_y - 0.1 / dim_y)) / divide_y
  )

  # Only x.
  is_only_x_at_or_over_quadrat_edge <- df$QX >= dim_x
  col_only_x_at_or_over_quadrat_edge <- floor(
    (  (dim_x - 0.1)  -  (dim_x * floor( (dim_x - 0.1) / dim_x ) ) ) / divide_x
  )
  row_only_x_at_or_over_quadrat_edge <- floor(
    (df$QY - dim_y * floor(df$QY / dim_y)) / divide_y
  )

  # Only y
  is_only_y_at_or_over_quadrat_edge <- df$QY >= dim_y
  col_only_y_are_or_over_quadrat_edge <- floor(
    (df$QX - dim_x * floor(df$QX / dim_x)) / divide_x
  )
  row_only_y_are_or_over_quadrat_edge <- floor(
    (dim_y - 0.1 - dim_y * floor(dim_y - 0.1 / dim_y)) / divide_y
  )

  # Neither x nor y
  is_within_quadrat_edges <- TRUE
  col_within_quadrat_edges <- floor(
    (df$QX - dim_x * floor(df$QX / dim_x)) / divide_x
  )
  row_within_quadrat_edges <- floor(
    (df$QY - dim_y * floor(df$QY / dim_y)) / divide_y
  )



  # Implement cases

  dplyr::mutate(df,
    subquadrat = dplyr::case_when(
      are_both_xy_at_or_over_quadrat_edge ~ paste0(
        1 + col_both_xy_at_or_over_quadrat_edge,
        1 + row_both_xy_at_or_over_quadrat_edge
        ),
      is_only_x_at_or_over_quadrat_edge ~ paste0(
        1 + col_only_x_at_or_over_quadrat_edge,
        1 + row_only_x_at_or_over_quadrat_edge
      ),
      is_only_y_at_or_over_quadrat_edge ~ paste0(
        1 + col_only_y_are_or_over_quadrat_edge,
        1 + row_only_y_are_or_over_quadrat_edge
      ),
      is_within_quadrat_edges ~ paste0(
        1 + col_within_quadrat_edges, 1 + row_within_quadrat_edges
      )
    )
  )
}
