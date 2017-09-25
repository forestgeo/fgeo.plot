context("add_quadrat_to_df_list")

test_that("adds quadrat names that match list names", {
  df_list <- sinharaja::sinh_q20[1:2]
  actual <- add_quadrat_to_df_list(df_list) %>%
    purrr::map(dplyr::pull, quadrat) %>%
    purrr::map(unique) %>%
    unlist()
  expected <- names(df_list)
  expect_equal(actual, expected)
})
