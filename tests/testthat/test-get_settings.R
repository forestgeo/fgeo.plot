context("test-get_header")

test_that("outputs the expected string", {
  x <- get_header()
  expect_type(x, "character")
  x <- NULL

  x <- get_header("test", NULL, NULL)
  expect_true(grepl("test", x))
})



context("test-get_theme")

test_that("outputs an object of class equal to that of a ggplot2 theme", {
  x <- get_theme()
  expect_equal(class(x), class(ggplot2::theme()))
})
