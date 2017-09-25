context("get_theme")

test_that("outputs an object of class equal to that of a ggplot2 theme", {
  x <- get_theme()
  expect_equal(class(x), class(ggplot2::theme()))
})
