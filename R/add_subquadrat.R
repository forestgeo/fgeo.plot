#' Add a quadrat variable to a dataframe based based on QX and QY coordinates.
#'
#' @param df A dataframe.
#' @param dimx,dimy Quadrat dimension for the plot. For example, in Sinharaja
#'   both are 20.
#' @param subqx_n,subqy_n Total number of divisions of each quadrat side. For
#'   example, For Sinharaja and most other plots, the value of these arguments
#'   is 5, which results in a 4x4 grid of subquadrats within each quadrat.
#' @return A dataframe with the additional variable `subquadrat`.
#' @export
#'
#' @examples
#' df <- sinharaja::sinh_vftbl_selected
#' with_subquadrat <- add_subquadrat(df, 20, 20, 5, 5)
#' head(with_subquadrat)
add_subquadrat <- function(df, dimx, dimy, subqx_n, subqy_n) {
  # Define conditions and actions for each case
  # (Each chunk answers: what coordinate is at or over the edge of the quadrat?)

  # Both x and y.
  are_both_xy_at_or_over_quadrat_edge <- df$QX >= dimx & df$QY >= dimy
  col_both_xy_at_or_over_quadrat_edge <- floor(
    (dimx - 0.1 - dimx * floor(dimx - 0.1 / dimx)) / subquadx_n
  )
  row_both_xy_at_or_over_quadrat_edge <- floor(
    (dimy - 0.1 - dimy * floor(dimy - 0.1 / dimy)) / subquady_n
  )

  # Only x.
  is_only_x_at_or_over_quadrat_edge <- df$QX >= dimx
  col_only_x_at_or_over_quadrat_edge <- floor(
    (dimx - 0.1 - dimx * floor(dimx - 0.1 / dimx)) / subquadx_n
  )
  row_only_x_at_or_over_quadrat_edge <- floor(
    (df$QY - dimy * floor(df$QY / dimy)) / subquady_n
  )

  # Only y
  is_only_y_at_or_over_quadrat_edge <- df$QY >= dimy
  col_only_y_are_or_over_quadrat_edge <- floor(
    (df$QX - dimx * floor(df$QX / dimx)) / subquadx_n
  )
  row_only_y_are_or_over_quadrat_edge <- floor(
    (dimy - 0.1 - dimy * floor(dimy - 0.1 / dimy)) / subquady_n
  )

  # Neither x nor y
  is_within_quadrat_edges <- TRUE
  col_within_quadrat_edges <- floor(
    (df$QX - dimx * floor(df$QX / dimx)) / subquadx_n
  )
  row_within_quadrat_edges <- floor(
    (df$QY - dimy * floor(df$QY / dimy)) / subquady_n
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
