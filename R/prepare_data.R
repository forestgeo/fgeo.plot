# Prepare data that will be passed to plot_repel and friends.



#' Wrap a number of functions that prepare a list of data frames for plotting.
#'
#' @export
#' @keywords internal
#'
#' @examples
prepare_for_plot_repel <- function(df_list) {
  explicit <- purrr::map(df_list, add_latest_tree_status)
  identified <- identify_subquadrat(explicit)
  add_limits_shrinked(identified)
}

#' Add alternatives to symbol, that are easier to understand.
#'
#' @param x A data frame.
#'
#' @return The modified data frame.
#' @export
#'
#' @examples
#' sin_q20 <- map(sin_q20, add_latest_tree_status)
add_latest_tree_status <- function(x) {
  mutate(x,
    sym1 = case_when(
      symbol == 16 ~ "Alive in 4",
      symbol ==  1 ~ "Alive in 3, Dead in 4",
      symbol == 15 ~ "Alive in 2, Dead in 3",
      symbol ==  0 ~ "Alive in 1, Dead in 2"
    ),
    latest_tree_status = case_when(
      symbol == 16 ~ "Alive",
      symbol ==  1 ~ "Dead",
      symbol == 15 ~ "Dead",
      symbol ==  0 ~ "Dead"
    )
  )
}

#' Adds subquadrat variable to a data frame, using Shameema's code.
#'
#' @param df A data frame.
#'
#' @author Shameema Jafferjee Esufali <shameemaesufali@gmail.com>.
#'
#' @return A list of 4 data frames.
#' @export
#'
#' @examples
#' df <- sin_q20[[15]]
#' lapply(add_subquadrat(df), head)
add_subquadrat <- function(df) {
  x1 <- c(0, 10, 10, 0)
  x2 <- c(10, 20, 20, 10)
  y1 <- c(0, 0, 10, 10)
  y2 <- c(10, 10, 20, 20)

  df_list <- replicate(4, data.frame(NULL, stringsAsFactors = FALSE))

  for (n in 1:4) {
    condition <- df$lx >= x1[n] & df$lx < x2[n] & df$ly >= y1[n] & df$ly < y2[n]
    df_list[[n]] <- df[condition, ]
    df_list[[n]]$subquadrat <- n
  }
  df_list
}

# help identify_subquadrat()
add_quadrat_and_subquadrat_from_list <- function(df_list) {
  with_quadrat <- add_quadrat_to_df_list(df_list)
  lapply(with_quadrat, add_subquadrat)
}

#' Identify quadrat and subquadrat, from list to data frame.
#'
#' @param df_list A list of data frames.
#'
#' @return A data frames with new variables identifying quadrat, and subquadrat.
#' @export
#'
#' @examples
#' df_list1 <- sin_q20[1:2]
#' names(df_list1)
#' identify_subquadrat(df_list1)
identify_subquadrat <- function(df_list) {
  with_quad_subquad_list <- add_quadrat_and_subquadrat_from_list(df_list)
  reduced_deep <- suppressMessages(
    purrr::map(with_quad_subquad_list, purrr::reduce, dplyr::full_join)
  )
  reduced_shallow <- suppressMessages(
    purrr::reduce(reduced_deep, dplyr::full_join)
  )
  ided <- mutate(reduced_shallow, id = paste(quadrat, subquadrat, sep = "-"))
  ordered <- dplyr::select(ided, id, quadrat, subquadrat, dplyr::everything())
  ordered
}

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

# help add_quadrat_to_df_list()
add_quadrat_to_one_df <- function(x, y) {
  with_quadrat <- dplyr::mutate(y, quadrat = x)
  quadrat_first <- dplyr::select(with_quadrat, quadrat, dplyr::everything())
  quadrat_first
}

#' Add the name of each element of a list as a value in the variable quadrat
#'
#' @param df A list of data frames
#'
#' @return A list
#' @export
#'
#' @examples
#' # xxx add example
# @examples
# df <- sin_q20[7:8]
# df_list <- add_quadrat_to_df_list(df)
# str(df_list)
# # Demonstrate on only two quadrats
# list_of_dfs <- sin_q20[c("109", "15")]
# str(list_of_dfs)
# add_quadrat_to_df_list(list_of_dfs)
add_quadrat_to_df_list <- function(df) {
  enframed_df <- tibble::enframe(df)
  purrr::map2(enframed_df$name, enframed_df$value, add_quadrat_to_one_df)
}
