context("print_first")

test_that("multiplication works", {
  expect_message(x <- print_first(list(1, 2)))
  expect_type(x, "list")
})




