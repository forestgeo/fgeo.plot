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


