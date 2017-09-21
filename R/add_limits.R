#' Add plot limits for quadrats 1-4.
#'
#' @param df A data frame.
#'
#' @return A modified data frame.
#' @export
#'
#' @examples
add_limits <- function(df) {
  mutate(df,
    x1 = case_when(
      subquadrat == 1 ~ 0,
      subquadrat == 2 ~ 10,
      subquadrat == 3 ~ 10,
      subquadrat == 4 ~ 0
    ),
    x2 = case_when(
      subquadrat == 1 ~ 10,
      subquadrat == 2 ~ 20,
      subquadrat == 3 ~ 20,
      subquadrat == 4 ~ 10
    ),
    y1 = case_when(
      subquadrat == 1 ~ 0,
      subquadrat == 2 ~ 0,
      subquadrat == 3 ~ 10,
      subquadrat == 4 ~ 10
    ),
    y2 = case_when(
      subquadrat == 1 ~ 10,
      subquadrat == 2 ~ 10,
      subquadrat == 3 ~ 20,
      subquadrat == 4 ~ 20
    )
  )
}






#' Add plot limits for quadrats 1-4.
#'
#' @param df A data frame.
#'
#' @return A modified data frame.
#' @export
#'
#' @examples
add_limits_shrinked <- function(df) {
  mutate(df,
    x1 = case_when(
      subquadrat == 1 ~ 0.4,
      subquadrat == 2 ~ 10.4,
      subquadrat == 3 ~ 10.4,
      subquadrat == 4 ~ 0.4
    ),
    x2 = case_when(
      subquadrat == 1 ~ 9.6,
      subquadrat == 2 ~ 19.6,
      subquadrat == 3 ~ 19.6,
      subquadrat == 4 ~ 9.6
    ),
    y1 = case_when(
      subquadrat == 1 ~ 0.4,
      subquadrat == 2 ~ 0.4,
      subquadrat == 3 ~ 10.4,
      subquadrat == 4 ~ 10.4
    ),
    y2 = case_when(
      subquadrat == 1 ~ 9.6,
      subquadrat == 2 ~ 9.6,
      subquadrat == 3 ~ 19.6,
      subquadrat == 4 ~ 19.6
    )
  )
}
