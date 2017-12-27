context("test-header")

test_that("map_tag_header outputs the expected string", {
  x <- map_tag_header()
  expect_type(x, "character")
  expect_true(grepl("Checking", x))
  x <- NULL

})

test_that("map_quad_header outputs the expected string", {
  x <- map_quad_header(lang = "spanish")
  expect_true(grepl("Nombres y Fecha", x))
  expect_type(x, "character")
  x <- NULL
})



context("test-theme_map_tag")

test_that("outputs an object of class equal to that of a ggplot2 theme", {
  x <- theme_map_tag()
  expect_equal(class(x), class(ggplot2::theme()))
})
