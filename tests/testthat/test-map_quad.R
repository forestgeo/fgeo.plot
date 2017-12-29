one_quadrat_sub <- dplyr::sample_n(one_quadrat, 5)


context("map_quad")

test_that("passes with default arguments", {
  # Warns because dataset has no dead trees
  expect_warning(
    expect_message(
      map_quad(
        one_quadrat_sub,
        title_quad = "Site Name, YYYY, Quadrat:",
        header = map_quad_header(),
        theme = theme_map_quad(),
        lim_min = 0,
        lim_max = 20,
        subquadrat_side = 5,
        tag_size = 2,
        extend_grid = 0
      )
    )
  )
})

test_that("wrong inputs to are rejected", {
  expect_error(map_quad())
  expect_error(map_quad(1))
  expect_error(map_quad(one_quadrat_sub, lim_min = "a"))
  expect_error(map_quad(one_quadrat_sub, lim_max = "a"))
  expect_error(map_quad(one_quadrat_sub, subquadrat_side = "a"))
  expect_error(map_quad(one_quadrat_sub, tag_size = "a"))
  expect_error(map_quad(one_quadrat_sub, extend_grid = "a"))
  expect_error(map_quad(one_quadrat_sub, header = 1))
  expect_error(map_quad(one_quadrat_sub, title_quad = 1))
  expect_error(map_quad(one_quadrat_sub, theme = 1))
})

test_that("returns a list of ggplots", {
  # Warns because dataset has no dead trees
  expect_warning(one_map <- map_quad(one_quadrat_sub))
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



context("check_unique_plotid")

test_that("works as expected", {
  expect_silent(check_unique_plotid(data.frame(plotid = c(1, 1))))
  expect_error(check_unique_plotid(data.frame(plotid = c(1, 2))))
})



context("check_unique_censusid")

test_that("works as expected", {
  expect_silent(
    check_unique_censusid(data.frame(censusid = c(1, 1)))
  )
  expect_warning(
    check_unique_censusid(
      data.frame(censusid = c(1, 2))
    )
  )
})

