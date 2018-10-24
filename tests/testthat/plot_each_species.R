census <- fgeo.tool::pick_top(fgeo.data::luquillo_stem5_random, sp, 2)
elevation <- fgeo.data::luquillo_elevation$col

context("plot_each_species")

test_that("works with species parameters", {
  spp <- unique(census$sp)
  elev <- elevation
  cns <- census

  expect_silent(
    plot_each_species(
      cns, elev, species = spp, fill = "white", shape = 21, point_size = 5
    )[[1]]
  )
  
})

test_that("works with elevation parameters", {
  spp <- unique(census$sp)
  elev <- elevation
  cns <- census

  expect_silent(
    plot_each_species(
      cns, elev, species = spp, fill = "white", shape = 21, point_size = 5,
      contour_size = 1, low = "grey", high = "black", hide_legend_color = TRUE,
      bins = 7, label_elev = FALSE
    )[[1]]
  )
  
  expect_silent(
    plot_each_species(
      cns, elev, species = spp, fill = "white", shape = 21, point_size = 5,
      contour_size = 1, low = "grey", high = "black", hide_legend_color = TRUE,
      bins = NULL, label_elev = TRUE, label_color = "black", xyjust = 1,
      fontface = "bold", xlim = c(0, 500), ylim = c(0, 400), 
      custom_theme = ggplot2::theme_bw()
    )[[1]]
  )
})

test_that("outputs a list of ggplots", {
  p <- plot_each_species(census)
  expect_type(p, "list")
  expect_true(has_class(p[[1]], "gg"))
})

test_that("errs with wrong inputs", {
  expect_error(plot_each_species(1), "is not TRUE")
  expect_error(plot_each_species(census, 1), "Can't deal with data of class")
  expect_error(plot_each_species(census, NULL, 1), "is not TRUE")
  expect_error(plot_each_species(census, xlim = 0), "Limits must be in a")
})



context("plot_species_or_elevation")

test_that("outputs a ggplot", {
  expect_is(plot_species_or_elevation(census), "gg")
})

test_that("errs with wrong inputs", {
  expect_error(plot_species_or_elevation(1), "is not TRUE")
  expect_error(plot_species_or_elevation(census, 1), "Can't deal with data of class")
  expect_error(plot_species_or_elevation(census, xlim = 0), "Limits must be in a")
})



context("plot_elevation")

test_that("outputs a ggplot", {
  p <- plot_elevation(elevation)
  expect_true(has_class(p, "gg"))
})

test_that("errs with wrong inputs", {
  expect_error(plot_elevation(1), "Can't deal with data of class")
  expect_error(plot_elevation(census), "Ensure your data set has these variables")
  expect_error(plot_elevation(list(not_col = census)), "Your list must contain")
  expect_error(plot_elevation(elevation, xlim = 0), "Limits must be in a")
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
