context("add_quadrat_to_df_list")

library(tidyverse)
test_that("adds quadrat names that match list names", {
  df <- sin_q20[7:8]
  actual <- add_quadrat_to_df_list(df) %>%
    purrr::map(pull, quadrat) %>%
    purrr::map(unique) %>%
    unlist()
  expected <- names(df)
  expect_equal(actual, expected)
})
