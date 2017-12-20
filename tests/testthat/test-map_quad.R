one_quadrat <- dplyr::filter(four_quadrats, QuadratName == 1)
one_map <- map_quad(one_quadrat)



context("map_quad")

test_that("passes with default arguments", {
  expect_silent(
    map_quad(
      one_quadrat,
      title_quad = "Site Name, YYYY, Quadrat:",
      header = header_map_quad(),
      theme = theme_map_quad(),
      lim_min = 0,
      lim_max = 20,
      subquadrat_side = 5,
      tag_size = 2,
      extend_grid = 0
    )[[1]]
  )
})

test_that("wrong inputs to are rejected", {
  expect_error(map_quad())
  expect_error(map_quad(1))
  expect_error(map_quad(one_quadrat, lim_min = "a"))
  expect_error(map_quad(one_quadrat, lim_max = "a"))
  expect_error(map_quad(one_quadrat, subquadrat_side = "a"))
  expect_error(map_quad(one_quadrat, tag_size = "a"))
  expect_error(map_quad(one_quadrat, extend_grid = "a"))
  expect_error(map_quad(one_quadrat, header = 1))
  expect_error(map_quad(one_quadrat, title_quad = 1))
  expect_error(map_quad(one_quadrat, theme = 1))
})

test_that("returns a list of ggplots", {
  expect_type(one_map, "list")
  expect_s3_class(one_map[[1]], "ggplot")
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
