# Prepare data that will be passed to plot_repel and friends.



#' Prepare a list of data frames to plot repulsive tags.
#'
#' This function may be rarely necessary. Its input is a rare data structure. It
#' is a list of dataframes, each corresponding to a 20x20 quadrat and with the
#' variables: "tag", "lx", "ly", "subtag", "status", "symbol".
#'
#' @family functions to prepare data to plot repulsive tags.
#' @param df_list A list of data frames.
#' @return A list of dataframes, prepared for [plot_repulsive_tags()].
#'
#' @export
#' @examples
#' # The sinharaja package is local and the data set sinh_q20 is private.
#' # Choosing only two 20x20 quadrats
#' list_of_dataframes <- sinharaja::sinh_q20[1:2]
#' str(list_of_dataframes)
#'
#' prepared <- prep_repulsive_tags(list_of_dataframes)
#' str(prepared)
prep_repulsive_tags <- function(df_list) {
  explicit_status <- purrr::map(df_list, add_latest_tree_status)
  identified <- identify_subquadrat(explicit_status)
  with_limits <- add_limits_shrinked(identified)
  split(with_limits, with_limits$id)
}

#' Add alternatives to symbol, that are easier to understand.
#'
#' @family functions to prepare data to plot repulsive tags.
#' @param x A data frame.
#'
#' @return The modified data frame.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' library(purrr)
#' library(try)
#' sin_q20 <- purrr::map(sin_q20, add_latest_tree_status)
#' }
add_latest_tree_status <- function(x) {
  dplyr::mutate(x,
    sym1 = dplyr::case_when(
      symbol == 16 ~ "Alive in 4",
      symbol ==  1 ~ "Alive in 3, Dead in 4",
      symbol == 15 ~ "Alive in 2, Dead in 3",
      symbol ==  0 ~ "Alive in 1, Dead in 2"
    ),
    latest_tree_status = dplyr::case_when(
      symbol == 16 ~ "Alive",
      symbol ==  1 ~ "Dead",
      symbol == 15 ~ "Dead",
      symbol ==  0 ~ "Dead"
    )
  )
}








#' Add plot limits for quadrats 1-4.
#'
#' @family functions to prepare data to plot repulsive tags.
#' @param df A data frame.
#'
#' @return A modified data frame.
#' @export
#' @keywords internal
add_limits <- function(df) {
  dplyr::mutate(df,
    x1 = dplyr::case_when(
      subquadrat == 1 ~ 0,
      subquadrat == 2 ~ 10,
      subquadrat == 3 ~ 10,
      subquadrat == 4 ~ 0
    ),
    x2 = dplyr::case_when(
      subquadrat == 1 ~ 10,
      subquadrat == 2 ~ 20,
      subquadrat == 3 ~ 20,
      subquadrat == 4 ~ 10
    ),
    y1 = dplyr::case_when(
      subquadrat == 1 ~ 0,
      subquadrat == 2 ~ 0,
      subquadrat == 3 ~ 10,
      subquadrat == 4 ~ 10
    ),
    y2 = dplyr::case_when(
      subquadrat == 1 ~ 10,
      subquadrat == 2 ~ 10,
      subquadrat == 3 ~ 20,
      subquadrat == 4 ~ 20
    )
  )
}

#' Add plot limits for quadrats 1-4.
#'
#' @family functions to prepare data to plot repulsive tags.
#' @param df A data frame.
#'
#' @return A modified data frame.
#' @export
#' @keywords internal
add_limits_shrinked <- function(df) {
  dplyr::mutate(df,
    x1 = dplyr::case_when(
      subquadrat == 1 ~ 0.4,
      subquadrat == 2 ~ 10.4,
      subquadrat == 3 ~ 10.4,
      subquadrat == 4 ~ 0.4
    ),
    x2 = dplyr::case_when(
      subquadrat == 1 ~ 9.6,
      subquadrat == 2 ~ 19.6,
      subquadrat == 3 ~ 19.6,
      subquadrat == 4 ~ 9.6
    ),
    y1 = dplyr::case_when(
      subquadrat == 1 ~ 0.4,
      subquadrat == 2 ~ 0.4,
      subquadrat == 3 ~ 10.4,
      subquadrat == 4 ~ 10.4
    ),
    y2 = dplyr::case_when(
      subquadrat == 1 ~ 9.6,
      subquadrat == 2 ~ 9.6,
      subquadrat == 3 ~ 19.6,
      subquadrat == 4 ~ 19.6
    )
  )
}
















# Functions ordered by increasing cummulative change. ---------------------

#' Help identify_subquadrat().
#'
#' @param df_list A list of dataframes
#' @param ... Arguments passed to [add_subquadrat()]. Importantly, this allows
#'   changing the size of the subquadrats.
#'
#' @family functions to prepare data to plot repulsive tags.
#' @export
#' @keywords internal
#' @examples
#' df_list <- sinharaja::sinh_q20[1:2]
#' # Extracting first quadrat and showing the head of each subquadrat
#' add_quadrat_and_subquadrat_from_list(df_list)[[1]] %>% lapply(head)
add_quadrat_and_subquadrat_from_list <- function(df_list, ...) {
  with_quadrat <- add_quadrat_to_df_list(df_list)
  lapply(with_quadrat, add_subquadrat, ...)
}

#' Add the name of each element of a list as a value in the variable quadrat
#'
#' @family functions to prepare data to plot repulsive tags.
#' @param df_list A list of data frames.
#'
#' @return A list of dataframes with the variable quadrat.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' df_list <- sinharaja::sinh_q20[1:2]
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

#' Adds subquadrat variable to a data frame, using Shameema's code.
#'
#' @family functions to prepare data to plot repulsive tags.
#' @param df A data frame.
#' @author Shameema Jafferjee Esufali <shameemaesufali@gmail.com>.
#'
#' @return A list of 4 data frames.
#' @export
#'
#' @examples
#' # From a dataframe of one quadrat, get a list of 4 subquadrats.
#' one_quadrat <- sinharaja::sinh_q20[[15]]
#' head(one_quadrat)
#' lapply(add_subquadrat(one_quadrat), head)
#'
#' # Show one subquadrat of the default size
#' subquadrats_of_default_size <- add_subquadrat(one_quadrat)[[1]]
#' one_default_subquad <- filter(subquadrats_of_default_size, subquadrat == 1)
#' ggplot(one_default_subquad, aes(lx, ly)) + geom_point()
#'
#' # Show one subquadrat of half the default size
#' subquadrats_half_size <- add_subquadrat(
#'   one_quadrat,
#'   x1 = (c(0, 10, 10, 0) / 2),
#'   x2 = (c(10, 20, 20, 10) / 2),
#'   y1 = (c(0, 0, 10, 10) / 2),
#'   y2 = (c(10, 10, 20, 20) / 2)
#' )[[1]]
#' one_half_sized_subquad <- filter(subquadrats_half_size, subquadrat == 1)
#' ggplot(one_half_sized_subquad, aes(lx, ly)) + geom_point()
add_subquadrat <- function(df,
                           x1 = c(0, 10, 10, 0),
                           x2 = c(10, 20, 20, 10),
                           y1 = c(0, 0, 10, 10),
                           y2 = c(10, 10, 20, 20)) {
  assertive.types::assert_is_data.frame(df)

  df_list <- replicate(4, data.frame(NULL, stringsAsFactors = FALSE))

  for (n in 1:4) {
    condition <- df$lx >= x1[n] & df$lx < x2[n] & df$ly >= y1[n] & df$ly < y2[n]
    df_list[[n]] <- df[condition, ]
    df_list[[n]]$subquadrat <- n
  }
  df_list
}

#' From a list of dataframes output a dataframe with a useful id variable.
#'
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
#' @keywords internal
#'
#' @examples
#' df_list <- sinharaja::sinh_q20[1:2]
#' identify_subquadrat(df_list)
#'
#' df_list <- sinharaja::sinh_q20[1:2]
#'
#' # Passing arguments to add_subquadrat(), to make subquadrats half the size
#' id <- identify_subquadrat(
#'  df_list,
#'  x1 = c(0, 10, 10, 0) / 2,
#'  x2 = c(10, 20, 20, 10) / 2,
#'  y1 = c(0, 0, 10, 10) / 2,
#'  y2 = c(10, 10, 20, 20) / 2
#' )
#' id %>% filter(id == "16-3") %>%
#'   ggplot(aes(lx, ly)) + geom_point()
identify_subquadrat <- function(df_list, ...) {
  with_quad_subquad_list <- add_quadrat_and_subquadrat_from_list(df_list, ...)
  reduced_deep <- suppressMessages(
    purrr::map(with_quad_subquad_list, purrr::reduce, dplyr::full_join)
  )
  reduced_shallow <- suppressMessages(
    purrr::reduce(reduced_deep, dplyr::full_join)
  )
  ided <- dplyr::mutate(
    reduced_shallow, id = paste(.data$quadrat, .data$subquadrat, sep = "-")
  )
  ordered <- dplyr::select(
    ided, .data$id, .data$quadrat, .data$subquadrat, dplyr::everything()
  )
  ordered
}





