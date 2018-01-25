census <- fgeo.tool::top(bciex::bci12s7mini, sp, 2)
elevation <- bciex::bci_elevation



context("maply_sp_elev")


test_that("works with species parameters", {
  spp <- unique(census$sp)
  elev <- elevation
  cns <- census

  expect_silent(
    maply_sp_elev(
      cns, elev, species = spp, fill = "white", shape = 21, point_size = 5
    )[[1]]
  )
  
})

test_that("works with elevation parameters", {
  spp <- unique(census$sp)
  elev <- elevation
  cns <- census

  expect_silent(
    maply_sp_elev(
      cns, elev, species = spp, fill = "white", shape = 21, point_size = 5,
      contour_size = 1, low = "grey", high = "black", hide_legend_color = TRUE,
      bins = 7, label_elev = FALSE
    )[[1]]
  )
  
  expect_silent(
    maply_sp_elev(
      cns, elev, species = spp, fill = "white", shape = 21, point_size = 5,
      contour_size = 1, low = "grey", high = "black", hide_legend_color = TRUE,
      bins = NULL, label_elev = TRUE, label_color = "black", xyjust = 1,
      fontface = "bold", xlim = c(0, 500), ylim = c(0, 400), 
      custom_theme = ggplot2::theme_bw()
    )[[1]]
  )
})

test_that("outputs a list of ggplots", {
  p <- maply_sp_elev(census)
  expect_type(p, "list")
  expect_true(has_class(p[[1]], "gg"))
})

test_that("errs with wrong inputs", {
  expect_error(
    maply_sp_elev(1)
  )
  expect_error(
    maply_sp_elev(census, 1)
  )
  expect_error(
    maply_sp_elev(census, NULL, 1)
  )
  expect_error(
    maply_sp_elev(census, xlim = 0)
  )
})



context("map_sp_elev")

test_that("outputs a ggplot", {
  p <- map_sp_elev(census)
  expect_true(has_class(p, "gg"))
})

test_that("errs with wrong inputs", {
  expect_error(
    map_sp_elev(1)
  )
  expect_error(
    map_sp_elev(census, 1)
  )
  expect_error(
    map_sp_elev(census, xlim = 0)
  )
})



context("map_elev")

test_that("outputs a ggplot", {
  p <- map_elev(elevation)
  expect_true(has_class(p, "gg"))
})

test_that("errs with wrong inputs", {
  expect_error(
    map_elev(1)
  )
  expect_error(
    map_elev(census)
  )
  expect_error(
    map_elev(list(not_col = census))
  )
  expect_error(
    map_elev(elevation, xlim = 0)
  )
})
