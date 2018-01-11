library(ggplot2)

context("map_base.R")

bad_nms_elev <- bciex::bci_elevation
bci_elev <- dplyr::rename(bad_nms_elev, gx = x, gy = y)

test_that("outputs as a ggplot", {
  expect_silent(x <- map_base(bci_elev))
  expect_true(
    any(grepl("ggplot", class(x)))
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
