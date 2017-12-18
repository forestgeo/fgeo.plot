context("test-map_one_quad.R")

test_that("theme_map_quad returns a valid ggplot2 theme", {
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

