context("test-map_quad.R")

test_that("checks of map_quad() do their job", {
  # missing vft
  expect_error(
    map_quad()
  )
  # not dataframe
  expect_error(
    map_quad(1)
  )
  # not numeric
  expect_error(map_quad(map::one_quadrat, lim_min = "a"))
  expect_error(map_quad(map::one_quadrat, lim_max = "a"))
  expect_error(map_quad(map::one_quadrat, subquadrat_side = "a"))
  expect_error(map_quad(map::one_quadrat, size_label = "a"))
  expect_error(map_quad(map::one_quadrat, offset = "a"))
  # not character
  expect_error(map_quad(map::one_quadrat, header = 1))
  expect_error(map_quad(map::one_quadrat, title = 1))
})














test_that("theme_map_quad() returns a valid ggplot2 theme", {
  expect_s3_class(theme_map_quad(), c("theme", "gg"))
})

test_that("tag_dead() labels a vector", {
  actual <- tag_dead(
    x = c("tag1", "tag2"),
    y = c("dead", "whatever")
  )
  expected <- c("tag1.d", "tag2")
  expect_equal(actual, expected)
})

test_that("tag_dead() warns if no stem is dead", {
  expect_warning(
    tag_dead(
      x = c("tag1", "tag2"),
      y = c("not-dead", "not-dead")
    )
  )
})

test_that("tag_dead() fails if x, y are not character vectors", {
  expect_error(tag_dead(1, "dead"))
})





# Utils -------------------------------------------------------------------

