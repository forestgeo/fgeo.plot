context("headers")

test_that("header_tag_status outputs the expected string", {
  x <- header_tag_status()
  expect_type(x, "character")
  expect_true(grepl("Checking", x))
  x <- NULL
})

test_that("header_dbh_bubbles outputs the expected string", {
  x <- header_dbh_bubbles(lang = "spanish")
  expect_true(grepl("Nombres y Fecha", x))
  expect_type(x, "character")
  x <- NULL
})
