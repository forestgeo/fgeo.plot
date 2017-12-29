context("headers")

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



context("themes")

test_that("outputs an object of class equal to that of a ggplot2 theme", {
  tag <- theme_map_tag()
  expect_equal(class(tag), class(ggplot2::theme()))

  quad <- theme_map_quad()
  expect_equal(class(quad), class(ggplot2::theme()))

  sp <- theme_map_sp()
  expect_equal(class(sp), class(ggplot2::theme()))
})

