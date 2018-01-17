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




context("map_plot.R")

sp4 <- fgeo.tool::top(bciex::bci12s7mini, sp, 4)
bad_nms_elev <- bciex::bci_elevation
bci_elev <- dplyr::rename(bad_nms_elev, gx = x, gy = y)

test_that("informs missing elevation data", {
  expect_silent(
    map_elevation(bci_elev)
  )
  expect_message(
    map_elevation(sp4)
  )
  expect_error(
    map_elevation(sp4, sp4)
  )
})



context("map_species")

test_that("outputs as expected", {
  p <- map_species(sp4)
  expect_true(
    any(grepl("ggplot", class(p)))
  )
  expect_silent(map_species(sp4, bci_elev))
  expect_silent(
    map_species(sp4, bci_elev, drop_fill = TRUE, label_elev = FALSE, size = 4) +
      facet_wrap_sp() +
      guides(color = "none")
  )
})
