context("test-header")

test_that("header_map_tag outputs the expected string", {
  x <- header_map_tag()
  expect_type(x, "character")
  expect_true(grepl("Checking", x))
  x <- NULL

})

test_that("header_map_quad outputs the expected string", {
  x <- header_map_quad()
  expect_true(grepl("Nombres y fecha", x))
  expect_type(x, "character")
  x <- NULL
})



context("test-theme_map_tag")

test_that("outputs an object of class equal to that of a ggplot2 theme", {
  x <- theme_map_tag()
  expect_equal(class(x), class(ggplot2::theme()))
})
