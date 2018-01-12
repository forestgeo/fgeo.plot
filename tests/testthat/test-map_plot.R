library(ggplot2)

bad_nms_elev <- bciex::bci_elevation
bci_elev <- dplyr::rename(bad_nms_elev, gx = x, gy = y)
context("map_plot")

# Tests are in map_base

context("map_base.R")

test_that("outputs as a ggplot", {
  # Just in case, calling map_plot()
  expect_silent(p <- map_plot(bci_elev))
  
  expect_silent(p <- map_base(bci_elev))
  expect_true(
    any(grepl("ggplot", class(p)))
  )
})

test_that("errs with wrong input", {
  expect_error(
    map_base(1)
  )
  expect_error(
    map_base(bad_nms_elev)
  )
  expect_error(
    map_base(bci_elev, xlim = -1)
  )
  expect_error(
    map_base(bci_elev, theme = "wrong")
  )
})
