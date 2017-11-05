#' Add a quadrat variable to a dataframe based based on qx and qy coordinates.
#'
#' @param df A dataframe.
#' @param dim_x,dim_y Quadrat dimension for the plot. For example, in Sinharaja
#'   both are 20.
#' @param div_x,div_y Total number of divisions of each quadrat side. For
#'   example, For Sinharaja and most other plots, the value of these arguments
#'   is 5, which results in a 4x4 grid of subquadrats within each quadrat.
#' @return A dataframe with the additional variable `subquadrat`.
#' @author Anudeep Singh.
#' @export
#'
#' @examples
#' \dontrun{
#' # sinharaja is a private package
#' df <- sinharaja::sinh_vftbl_selected
#' with_subquadrat <- add_subquadrat(df, 20, 20, 5, 5)
#' head(with_subquadrat)
#' }
add_subquadrat <- function(df, dim_x, dim_y, div_x, div_y) {
  # Simplify nested parentheses
  dim_x_mns.1 <- dim_x - 0.1
  dim_y_mns.1 <- dim_y - 0.1

  # Conditions
  is_odd_both <- df$qx >=  dim_x & df$qy >=  dim_y
  is_odd_x <- df$qx >=  dim_x
  is_odd_y <- df$qy >=  dim_y
  is_not_odd <- TRUE

  # Cases
  with_subquadrat <- dplyr::mutate(df,
    subquadrat = dplyr::case_when(
      is_odd_both ~ paste0(
        (1 + floor((dim_x_mns.1 - dim_x * floor(dim_x_mns.1 / dim_x)) / div_x)),
        (1 + floor((dim_y_mns.1- dim_y * floor(dim_y_mns.1/ dim_y)) / div_y))
      ),
      is_odd_x ~ paste0(
        (1 + floor((dim_x_mns.1 - dim_x * floor(dim_x_mns.1 / dim_x)) / div_x)),
        (1 + floor((df$qy - dim_y * floor(df$qy/ dim_y)) / div_y))
      ),
      is_odd_y ~ paste0(
        (1 + floor((df$qx - dim_x * floor(df$qx/ dim_x)) / div_x)),
        (1 + floor((dim_y_mns.1- dim_y * floor(dim_y_mns.1 / dim_y)) / div_y))
      ),
      is_not_odd ~ paste0(
        (1 + floor((df$qx - dim_x * floor(df$qx/ dim_x)) / div_x)),
        (1 + floor((df$qy - dim_y * floor(df$qy/ dim_y)) / div_y))
      )
    )
  )
  with_subquadrat
}


#' Paginate a ViewFullTable. Add a variable indicating page to map on.
#'
#' @param x A ViewFullTable dataframe.
#'
#' @return A modified ViewFullTable dataframe..
#' @export
#' @keywords internal
#' @noRd
paginate <- function(x) {
  dplyr::mutate(x, subquadrat =
      case_when(
        subquadrat_vftbl == 11 ~ 1,
        subquadrat_vftbl == 12 ~ 1,
        subquadrat_vftbl == 21 ~ 1,
        subquadrat_vftbl == 22 ~ 1,

        subquadrat_vftbl == 31 ~ 2,
        subquadrat_vftbl == 32 ~ 2,
        subquadrat_vftbl == 41 ~ 2,
        subquadrat_vftbl == 42 ~ 2,

        subquadrat_vftbl == 34 ~ 3,
        subquadrat_vftbl == 33 ~ 3,
        subquadrat_vftbl == 44 ~ 3,
        subquadrat_vftbl == 43 ~ 3,

        subquadrat_vftbl == 14 ~ 4,
        subquadrat_vftbl == 13 ~ 4,
        subquadrat_vftbl == 24 ~ 4,
        subquadrat_vftbl == 23 ~ 4,
      )
    )
}


#' Add variable sqds
#'
#' @param df A ViewFullTable dataframe with the variable subquadrat.
#'
#' @return Modified version of input.
#' @export
#' @keywords internal
#' @noRd
add_sqds <- function(df) {
  paginate(df) %>%
  dplyr::group_by(subquadrat_vftbl) %>%
  dplyr::mutate(sqds = paste0(unique(sort(subquadrat_vftbl)), collapse = "-")) %>%
  dplyr::ungroup() %>%
  dplyr::select(sqds, everything())
}


#' Prepare a list of dataframes to later plot repulsive tags.
#'
#' @param df_list A list of dataframes
#'
#' @return A modified version of the input.
#' @keywords internal
#' @export
#' @noRd
prep_repulsive_tags <- function(df_list) {
  x <- df_list %>%
    purrr::map(add_sqds)
  x %>%
    purrr::map(add_latest_tree_status) %>%  # fix this function
    purrr::map(dplyr::mutate, latest_tree_status = status_tree)  %>% # patch
    purrr::map(paginate) %>%
    purrr::map(dplyr::rename, quadrat = quadrat_vftbl) %>%
    purrr::map(add_subquad_limits) %>%
    purrr::map(dplyr::mutate,
      id = paste0("Q. ", quadrat, " SQ. ", sqds, " (p. ", subquadrat_vftbl, ")")
    ) %>%
    purrr::map(dplyr::select, id, subquadrat_vftbl, dplyr::everything()) %>%
    purrr::map(dplyr::select,
      id, tag, lx, ly, latest_tree_status, x1, x2, y1, y2, dplyr::everything()
    ) %>%
    purrr::reduce(dplyr::full_join) %>%
    # Add status to tag because some points dissapear from plot but tags
    # persist
    dplyr::mutate(
      tag = case_when(
        latest_tree_status == "alive" ~ paste0(tag, "_"),
        latest_tree_status == "dead" ~ paste0(tag, ".")
      )
    ) %>%
    split(., .$id)
}
