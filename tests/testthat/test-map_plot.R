library(ggplot2)

bad_nms_elev <- bciex::bci_elevation
bci_elev <- dplyr::rename(bad_nms_elev, gx = x, gy = y)

context("map_plot.R")

test_that("outputs as a ggplot", {
  # Just in case, calling map_plot()
  expect_silent(
    p <- map_plot(bci_elev, NULL)
  )
  expect_true(
    any(grepl("ggplot", class(p)))
  )

  expect_equal(
    map_elevation(bci_elev, NULL),
    map_plot(bci_elev, NULL)
  )

  expect_silent(
    p <- map_plot(NULL, bci_elev)
  )
  expect_silent(
    p <- map_plot(NULL, bci_elev)
  )
  expect_equal(
    map_elevation(NULL, bci_elev),
    map_plot(NULL, bci_elev)
  )
})

test_that("errs with wrong input", {
  expect_error(
    map_plot(NULL, NULL)
  )
  expect_error(
    map_plot(1)
  )
  expect_error(
    map_plot(bad_nms_elev)
  )
  expect_error(
    map_plot(bci_elev, xlim = -1)
  )
  expect_error(
    map_plot(bci_elev, theme = "wrong")
  )
})
