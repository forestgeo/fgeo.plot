# help add_quadrat_to_df_list()
add_quadrat_to_one_df <- function(x, y) {
  with_quadrat <- dplyr::mutate(y, quadrat = x)
  quadrat_first <- dplyr::select(with_quadrat, quadrat, dplyr::everything())
  quadrat_first
}
# Add the name of each element of a list as a value in the variable quadrat
#
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
