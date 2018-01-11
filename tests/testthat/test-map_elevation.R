library(ggplot2)

context("test-map_elevation.R")

bad_nms_elev <- bciex::bci_elevation
bci_elev <- dplyr::rename(bad_nms_elev, gx = x, gy = y)

test_that("outputs a ggplot", {
  
  x <- map_elevation(bci_elev)
  expect_true(
    any(grepl("ggplot", class(x)))
  )
})

test_that("errs with wrong input", {
  expect_error(
    map_elevation(bci_elev, line_size = "a")
  )
  expect_error(
    map_elevation(bci_elev, low = 1)
  )
  expect_error(
    map_elevation(bci_elev, high = 1)
  )
  expect_error(
    map_elevation(bci_elev, bins = "a")
  )
})
