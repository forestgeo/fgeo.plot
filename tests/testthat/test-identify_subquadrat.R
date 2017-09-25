context("identify_subquadrat")


df_list <- sinharaja::sinh_q20[1:2]

test_that("outputs the expected dataframe", {
  result <- identify_subquadrat(df_list)
  expect_type(result, "list")
  expect_true(assertive.types::is_data.frame(result))
  expect_true(any(grepl("id", names(result))))
})
