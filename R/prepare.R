# Prepare data that will be passed to plot_repel and friends.

#' Prepare a list of data frames to plot repulsive tags.
#'
#' This function may be rarely necessary. It wraps other functions that you will
#' likely want to use directly, so you have more flexibility. The input of this
#' function may also be a rare data structure. It is a list of dataframes, each
#' corresponding to a 20x20 quadrat and with the variables: "tag", "lx", "ly",
#' "subtag", "status", "symbol".
#'
#' @family functions to prepare data to plot repulsive tags.
#' @param df_list A list of data frames.
#' @return A list of dataframes, prepared for [plot_repulsive_tags()].
#'
#' @export
#' @examples
#' list_of_dataframes <- toy_list
#' str(list_of_dataframes)
#'
#' prepared <- prep_repulsive_tags(list_of_dataframes)
#' str(prepared[1:2])
prep_repulsive_tags <- function(df_list) {
  # Pad quadrat names with 0 to the left.
  names(df_list) <- stringr::str_pad(
    names(df_list), width = 4, pad = "0", side = "left"
  )
  explicit_status <- purrr::map(df_list, add_latest_tree_status)
  identified <- identify_subquadrat(explicit_status)
  with_limits <- add_subquad_limits(identified)
  useful_vars <- dplyr::select(
    with_limits,
    id, tag, lx, ly, latest_tree_status, x1, x2, y1, y2
  )
  split(useful_vars, with_limits$id)
}

#' Add alternatives to the variable `symbol` that are easier to understand.
#'
#' The variable `symbol` codes the status of a tree in a very succint way. This
#' function unpacks the meaning of `symbol`. Be careful. This function is
#' specifically designed for one particular dataset and should not be used
#' in other data sets.
#'
#' @family functions to prepare data to plot repulsive tags.
#' @param df A data frame with a variable named `symbol`.
#'
#' @return The modified data frame.
#' @export
#'
#' @examples
#'  # Add to a single dataframe
#'  df <- toy_list[[1]]
#'  head(add_latest_tree_status(df))
#'
#'  # Add to each dataframe in a list
#'  df_list <- toy_list
#'  result <- lapply(df_list, add_latest_tree_status)
#'  # Just show a few rows of each dataframe
#'  lapply(result, head)
add_latest_tree_status <- function(df) {
  dplyr::mutate(df,
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
#'   # The only "must be" is the variable `subquadrat`; we could remove `quadrat`
#'   select(-quadrat) %>%
#'   add_subquad_limits(quad_size = 20) %>%
#'   head()
add_subquad_limits <- function(df_with_subquad, quad_size = 20, shrink = 0.4) {
  dplyr::mutate(df_with_subquad,
    x1 = dplyr::case_when(
      subquadrat == 1 ~ 0 + shrink,
      subquadrat == 2 ~ (quad_size / 2) + shrink,
      subquadrat == 3 ~ (quad_size / 2) + shrink,
      subquadrat == 4 ~ 0 + shrink
    ),
    x2 = dplyr::case_when(
      subquadrat == 1 ~ (quad_size / 2) - shrink,
      subquadrat == 2 ~ quad_size - shrink,
      subquadrat == 3 ~ quad_size - shrink,
      subquadrat == 4 ~ (quad_size / 2) - shrink
    ),
    y1 = dplyr::case_when(
      subquadrat == 1 ~ 0 + shrink,
      subquadrat == 2 ~ 0 + shrink,
      subquadrat == 3 ~ (quad_size / 2) + shrink,
      subquadrat == 4 ~ (quad_size / 2) + shrink
    ),
    y2 = dplyr::case_when(
      subquadrat == 1 ~ (quad_size / 2) - shrink,
      subquadrat == 2 ~ (quad_size / 2) - shrink,
      subquadrat == 3 ~ quad_size  - shrink,
      subquadrat == 4 ~ quad_size - shrink
    )
  )
}



# Towards creating an id variable with info from quad and subquad ---------

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
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' df_list <- toy_list[1:2]
#' head(identify_subquadrat(df_list))
#'
#' df_list <- toy_list[1:2]
#'
#' # Passing arguments to add_subquadrat(), to make subquadrats half the size
#' id <- identify_subquadrat(
#'  df_list,
#'  x1 = c(0, 10, 10, 0) / 2,
#'  x2 = c(10, 20, 20, 10) / 2,
#'  y1 = c(0, 0, 10, 10) / 2,
#'  y2 = c(10, 10, 20, 20) / 2
#' )
#' \dontrun{
#' one_subquad <- filter(id, id == "16-3")
#' ggplot(one_subquad, aes(lx, ly)) + geom_point()
#' }
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

#' Help identify_subquadrat().
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

#' Adds subquadrat variable to a data frame, using Shameema's code.
#'
#' @family functions to prepare data to plot repulsive tags.
#'
#' @param x1,x2,y1,y2 Parameters to set x and y limits of each subquadrat.
#' @param df A data frame.
#'
#' @author Shameema Jafferjee Esufali <shameemaesufali@gmail.com>.
#'
#' @return A list of 4 data frames.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' # From a dataframe of one quadrat, get a list of 4 subquadrats.
#' one_quadrat <- toy_list[[1]]
#' head(one_quadrat)
#' lapply(add_subquadrat(one_quadrat), head)
#'
#' # Show one subquadrat of the default size
#' subquadrats_of_default_size <- add_subquadrat(one_quadrat)[[1]]
#' one_default_subquad <- filter(subquadrats_of_default_size, subquadrat == 1)
#' \dontrun{
#' ggplot(one_default_subquad, aes(lx, ly)) + geom_point()
#' }
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
#' \dontrun{
#' ggplot(one_half_sized_subquad, aes(lx, ly)) + geom_point()
#' }
add_subquadrat <- function(df,
                           x1 = c(0, 10, 10, 0),
                           x2 = c(10, 20, 20, 10),
                           y1 = c(0, 0, 10, 10),
                           y2 = c(10, 10, 20, 20)) {
  assertive::assert_is_data.frame(df)

  df_list <- replicate(4, data.frame(NULL, stringsAsFactors = FALSE))

  for (n in 1:4) {
    condition <- df$lx >= x1[n] & df$lx < x2[n] & df$ly >= y1[n] & df$ly < y2[n]
    df_list[[n]] <- df[condition, ]
    df_list[[n]]$subquadrat <- n
  }
  df_list
}

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
