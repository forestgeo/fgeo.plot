context("identify_subquadrat")


df_list <- toy_list[1:2]

test_that("outputs the expected dataframe", {
  result <- identify_subquadrat(df_list)
  expect_type(result, "list")
  expect_true(assertive.types::is_data.frame(result))
  expect_true(any(grepl("id", names(result))))
})
