context("get_header")

test_that("outputs the expected string", {
  x <- get_header()
  expect_type(x, "character")
  x <- NULL

  x <- get_header("test", NULL, NULL)
  expect_true(grepl("test", x))
})
