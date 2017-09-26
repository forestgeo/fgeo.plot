context("add_quadrat_and_subquadrat_from_list")

df_list <- toy_list[1:2]

test_that("outputs a list of dataframs", {
  result <- add_quadrat_and_subquadrat_from_list(df_list)
  expect_type(result, "list")
  expect_type(result[[1]], "list")
  expect_true(assertive.types::is_data.frame(result[[1]][[1]]))
  # Each quadrat has 4 subquadrats
  expect_length(result[[1]], 4)

})
