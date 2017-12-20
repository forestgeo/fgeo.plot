one_quadrat <- map_quad(dplyr::filter(four_quadrats, QuadratName == 1))



context("map_quad")

test_that("wrong inputs to are rejected", {
  expect_error(map_quad())
  expect_error(map_quad(1))
  expect_error(map_quad(one_quadrat, lim_min = "a"))
  expect_error(map_quad(one_quadrat, lim_max = "a"))
  expect_error(map_quad(one_quadrat, subquadrat_side = "a"))
  expect_error(map_quad(one_quadrat, size_label = "a"))
  expect_error(map_quad(one_quadrat, offset = "a"))
  # not character
  expect_error(map_quad(one_quadrat, header = 1))
  expect_error(map_quad(one_quadrat, .title = 1))
})

test_that("returns a list of ggplots", {
  expect_type(one_quadrat, "list")
  expect_s3_class(one_quadrat[[1]], "ggplot")
})



context("tag_dead")

test_that("tags a vector", {
  actual <- tag_dead(
    x = c("tag1", "tag2"),
    y = c("dead", "whatever")
  )
  expected <- c("tag1.d", "tag2")
  expect_equal(actual, expected)
})

test_that("warns if no stem is dead", {
  expect_warning(
    tag_dead(
      x = c("tag1", "tag2"),
      y = c("not-dead", "not-dead")
    )
  )
})

test_that("fails if x, y are not character vectors", {
  expect_error(tag_dead(1, "dead"))
})



context("theme_map_quad")

test_that("returns a valid ggplot2 theme", {
  expect_s3_class(theme_map_quad(), c("theme"))
  expect_s3_class(theme_map_quad(), "gg")
})
