#' Title
#'
#' @param vft A ViewFullTable -- a subclass of a dataframe.
#'
#' @return
#' @export
#'
#' @examples
map_tag <- function(vft,
                    dim_x = 20,
                    div_x = 5,
                    dim_y = dim_x,
                    div_y = div_x,
                    site_name = "Site Name, YYYY") {
  # Lowercase names
  vft2 <- setNames(vft, tolower(names(vft)))
  # Check names
  nms <- c("tag", "qx", "qy", "status")
  check_crucial_names(vft2, nms)

  with_subquadrat <- add_subquadrat(df = vft2,
    dim_x = dim_x, dim_y = dim_y, div_x = div_x, div_y = div_y
  )


  # xxx continue refactoring here

  renamed <- dplyr::rename(
    with_subquadrat,
    subquadrat_vftbl = subquadrat,
    quadrat_vftbl = quadratname,
    lx = qx,
    ly = qy,
  )

  with_status <- renamed %>%
    dplyr::group_by(tag) %>%
    dplyr::arrange(.by_group = TRUE) %>%
    # ensure that the status refers to the tree, not to the stem
    dplyr::mutate(status_tree = ifelse(any(status == "alive"), "alive", "dead")) %>%
    dplyr::select(dbhid, tag, status, status_tree, everything())

  with_symbol <- with_status %>%
    mutate(symbol = ifelse(status_tree == "alive", 19, 4)) %>%
    select(status_tree, symbol, everything())

  # xxx maybe I can avoid splitting and work with df?
  splitted <- with_symbol %>% split(.$quadrat_vftbl)

  # Prepare.
  # xxx maybe I can remove useless variables considering the next step
  # maybe I can remove duplicated tabs, considering the next step
  prep_list <- prep_repulsive_tags(splitted)

  # Keep minimum data and remove duplicates
  unique_tags <- prep_list %>%
    purrr::map(select, tag, lx, ly, status, id, latest_tree_status, x1, x2, y1, y2) %>%
    purrr::map(unique)

  plot_list <- unique_tags %>%
  lapply_plot_repulsive_tags(site_name = site_name)
}






check_crucial_names <- function(x, nms) {
  are_names_expected <- all(nms %in% names(x))
  if (are_names_expected) {
    return(invisible())
  } else {
    stop("Ensure your data has names ",
      glue::collapse(nms, ", ", last = ", and "), call. = FALSE)
  }
}

