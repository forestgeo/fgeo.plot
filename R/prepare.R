# #' Prepare a list of data frames to plot repulsive tags.
# #'
# #' This function may be rarely necessary. It wraps other functions that you will
# #' likely want to use directly, so you have more flexibility. The input of this
# #' function may also be a rare data structure. It is a list of dataframes, each
# #' corresponding to a 20x20 quadrat and with the variables: "tag", "lx", "ly",
# #' "subtag", "status", "symbol".
# #'
# #' @family functions to prepare data to plot repulsive tags.
# #' @param df_list A list of data frames.
# #' @return A list of dataframes, prepared for [plot_repulsive_tags()].
# #'
# #' @export
# #' @examples
# #' list_of_dataframes <- toy_list
# #' str(list_of_dataframes)
# #'
# #' prepared <- prep_repulsive_tags(list_of_dataframes)
# #' str(prepared[1:2])
# prep_repulsive_tags <- function(df_list) {
#  # xxx add function and amend documentation
# }






# Towards creating an id variable with info from quad and subquad ---------









#' From a list of dataframes output a dataframe with a useful id variable.

#' This function with quadrat and subquadrat variables add id.
#'
#' The name of each element of the input list becomes a value of a new variable
#' `quadrat`; and the variable `subquadrat` is also added, based on the values
#' of the variables `lx` and `ly` that each dataframe should have.
#'
#' @family functions to prepare data to plot repulsive tags.
#'
#' @param df_list A list of data frames.
#' @param ... Arguments passed to [add_quadrat_and_subquadrat_from_list()].
#'   This is lets users change the size of subquadrat via [add_subquadrat()].
#'
#' @return A data frames with new variables quadrat, subquadrat and id, which
#'   combines the other two.
#' @export
#'
#' @examples
#' #' library(dplyr)
#' #' library(ggplot2)
#' #'
#' #' df_list <- toy_list[1:2]
#' #' head(identify_subquadrat(df_list))
#' #'
#' #' df_list <- toy_list[1:2]
#' #'
#' #' # Passing arguments to add_subquadrat(), to make subquadrats half the size
#' #' id <- identify_subquadrat(
#' #'  df_list,
#' #'  x1 = c(0, 10, 10, 0) / 2,
#' #'  x2 = c(10, 20, 20, 10) / 2,
#' #'  y1 = c(0, 0, 10, 10) / 2,
#' #'  y2 = c(10, 10, 20, 20) / 2
#' #' )
#' #' \dontrun{
#' #' one_subquad <- filter(id, id == "16-3")
#' #' ggplot(one_subquad, aes(lx, ly)) + geom_point()
#' #' }
#' identify_subquadrat <- function(df_list, ...) {
#'   with_quad_subquad_list <- add_quadrat_and_subquadrat_from_list(df_list, ...)
#'   reduced_deep <- suppressMessages(
#'     purrr::map(with_quad_subquad_list, purrr::reduce, dplyr::full_join)
#'   )
#'   reduced_shallow <- suppressMessages(
#'     purrr::reduce(reduced_deep, dplyr::full_join)
#'   )
#'   ided <- dplyr::mutate(
#'     reduced_shallow, id = paste(.data$quadrat, .data$subquadrat, sep = "-")
#'   )
#'   ordered <- dplyr::select(
#'     ided, .data$id, .data$quadrat, .data$subquadrat, dplyr::everything()
#'   )
#'   ordered
#' }



















#' Add the name of each element of a list as a value in the variable quadrat
#'
#' @family functions to prepare data to plot repulsive tags.
#' @param df_list A list of data frames.
#'
#' @return A list of dataframes with the variable quadrat.
#' @export
#'
#' @examples
#' \dontrun{
#' df_list <- toy_list[1:2]
#' with_quadrat <- add_quadrat_to_df_list(df_list)
#' str(with_quadrat)
#' }
add_quadrat_to_df_list <- function(df_list) {
  enframed_df <- tibble::enframe(df_list)
  purrr::map2(enframed_df$name, enframed_df$value, add_quadrat_to_one_df)
}
# Helper of add_quadrat_to_df_list()
add_quadrat_to_one_df <- function(x, y) {
  # Use mutate because it works with list columns
  dplyr::mutate(y, quadrat = x)
}
