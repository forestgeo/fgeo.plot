context("themes")

test_that("outputs an object of class equal to that of a ggplot2 theme", {
  tag <- theme_tag_status()
  expect_equal(class(tag), class(ggplot2::theme()))

  quad <- theme_dbh_bubbles()
  expect_equal(class(quad), class(ggplot2::theme()))
})
