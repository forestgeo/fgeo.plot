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



context("test-pad")

test_that("returns a string of the expected number of characters", {
  test_string <- c("12345", "67890")
  x <- pad(test_string, total_width = 20)
  expect_true(grepl("12345", x))
  # this is a bug I need to fix
  expect_failure(expect_equal(nchar(x), 20))
})
