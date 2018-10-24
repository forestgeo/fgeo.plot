context("headers")

test_that("header_tag_status outputs the expected string", {
  x <- header_tag_status()
  expect_type(x, "character")
  expect_true(grepl("Checking", x))
  x <- NULL

})

test_that("header_dbh_bubles outputs the expected string", {
  x <- header_dbh_bubles(lang = "spanish")
  expect_true(grepl("Nombres y Fecha", x))
  expect_type(x, "character")
  x <- NULL
})



context("themes")

test_that("outputs an object of class equal to that of a ggplot2 theme", {
  tag <- theme_tag_status()
  expect_equal(class(tag), class(ggplot2::theme()))

  quad <- theme_dbh_bubles()
  expect_equal(class(quad), class(ggplot2::theme()))
})

