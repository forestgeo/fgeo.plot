#' Help `identify_subquadrat()`.
#'
#' @param df_list A list of dataframes
#' @param ... Arguments passed to [add_subquadrat()]. Importantly, this allows
#'   changing the size of the subquadrats.
#'
#' @family functions to prepare data to plot repulsive tags.
#' @export
#' @examples
#' df_list <- toy_list[1:2]
#' # Extracting first quadrat and showing the head of each subquadrat
#' added <- add_quadrat_and_subquadrat_from_list(df_list)[[1]]
#' lapply(added, head)
add_quadrat_and_subquadrat_from_list <- function(df_list, ...) {
  with_quadrat <- add_quadrat_to_df_list(df_list)
  lapply(with_quadrat, add_subquadrat, ...)
}

# test ----

context("add_quadrat_and_subquadrat_from_list")

df_list <- toy_list[1:2]

test_that("outputs a list of dataframs", {
  result <- add_quadrat_and_subquadrat_from_list(df_list)
  expect_type(result, "list")
  expect_type(result[[1]], "list")
  expect_true(assertive.types::is_data.frame(result[[1]][[1]]))
  # Each quadrat has 4 subquadrats
  expect_length(result[[1]], 4)

})







