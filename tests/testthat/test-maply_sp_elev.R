census <- fgeo.tool::pick_top(fgeo.data::luquillo_stem5_random, sp, 2)
elevation <- fgeo.data::luquillo_elevation$col

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
  expect_error(maply_sp_elev(1), "is not TRUE")
  expect_error(maply_sp_elev(census, 1), "Can't deal with data of class")
  expect_error(maply_sp_elev(census, NULL, 1), "is not TRUE")
  expect_error(maply_sp_elev(census, xlim = 0), "Limits must be in a")
})



context("map_sp_elev")

test_that("outputs a ggplot", {
  expect_is(map_sp_elev(census), "gg")
})

test_that("errs with wrong inputs", {
  expect_error(map_sp_elev(1), "is not TRUE")
  expect_error(map_sp_elev(census, 1), "Can't deal with data of class")
  expect_error(map_sp_elev(census, xlim = 0), "Limits must be in a")
})



context("map_elev")

test_that("outputs a ggplot", {
  p <- map_elev(elevation)
  expect_true(has_class(p, "gg"))
})

test_that("errs with wrong inputs", {
  expect_error(map_elev(1), "Can't deal with data of class")
  expect_error(map_elev(census), "Ensure your data set has these variables")
  expect_error(map_elev(list(not_col = census)), "Your list must contain")
  expect_error(map_elev(elevation, xlim = 0), "Limits must be in a")
})



context("map_gx_gy_elev")

test_that("works with raw elevation data", {
  elevation_ls <- fgeo.data::luquillo_elevation
  expect_silent(map_gx_gy_elev(elevation_ls))
  expect_silent(map_gx_gy_elev(elevation_ls))
})

test_that("errs if elevation data is confused with census data", {
  expect_error(map_gx_gy_elev(census), "Ensure your data set has")
})
