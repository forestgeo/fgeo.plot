context("add_quadrat_to_df_list")

test_that("adds quadrat names that match list names", {
  df <- sinharaja::sinh_q20[7:8]
  actual <- add_quadrat_to_df_list(df) %>%
    purrr::map(dplyr::pull, quadrat) %>%
    purrr::map(unique) %>%
    unlist()
  expected <- names(df)
  expect_equal(actual, expected)
})
