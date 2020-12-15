census <- pick_top(fgeo.x::stem5, sp, 2)
elevation <- fgeo.x::elevation$col

context("plot_each_species")

test_that("works with species parameters", {
  spp <- unique(census$sp)
  elev <- elevation
  cns <- census

  expect_silent(
    plot_each_species(
      cns, elev,
      species = spp, fill = "white", shape = 21, point_size = 5
    )[[1]]
  )
})

test_that("works with elevation parameters", {
  spp <- unique(census$sp)
  elev <- elevation
  cns <- census

  expect_silent(
    plot_each_species(
      cns, elev,
      species = spp, fill = "white", shape = 21, point_size = 5,
      contour_size = 1, low = "grey", high = "black", hide_color_legend =
        TRUE, bins = 7, add_elevation_labels = FALSE
    )[[1]]
  )

  expect_silent(
    plot_each_species(
      cns, elev,
      species = spp, fill = "white", shape = 21, point_size = 5,
      contour_size = 1, low = "grey", high = "black", hide_color_legend =
        TRUE, bins = NULL, add_elevation_labels = TRUE, label_color = "black",
      xyjust = 1, fontface = "bold", xlim = c(0, 500), ylim = c(0, 400),
      custom_theme = ggplot2::theme_bw()
    )[[1]]
  )
})

test_that("outputs a list of ggplots", {
  p <- plot_each_species(census)
  expect_type(p, "list")
  expect_is(p[[1]], "gg")
})

test_that("errs with wrong inputs", {
  expect_error(plot_each_species(1), "is not TRUE")
  expect_error(plot_each_species(census, 1), "Can't deal with data of class")
  expect_error(plot_each_species(census, NULL, 1), "is not TRUE")
  expect_error(plot_each_species(census, xlim = 0), "Limits must be in a")
})



context("plot_sp_elev")

test_that("outputs a ggplot", {
  expect_is(plot_sp_elev(census), "gg")
})

test_that("errs with wrong inputs", {
  expect_error(plot_sp_elev(1), "is not TRUE")
  expect_error(plot_sp_elev(census, 1), "Can't deal with data of class")
  expect_error(plot_sp_elev(census, xlim = 0), "Limits must be in a")
})



context("plot_elev")

test_that("outputs a ggplot", {
  p <- plot_elev(elevation)
  expect_is(p, "gg")
})

test_that("errs with wrong inputs", {
  expect_error(plot_elev(1), "Can't deal with data of class")
  expect_error(plot_elev(census), "Ensure your data set has these variables")
  expect_error(plot_elev(list(not_col = census)), "Your list must contain")
  expect_error(plot_elev(elevation, xlim = 0), "Limits must be in a")
})



context("plot_base_elevation")

test_that("works with raw elevation data", {
  elevation_ls <- fgeo.x::elevation
  expect_silent(plot_base_elevation(elevation_ls))
  expect_silent(plot_base_elevation(elevation_ls))
})

test_that("errs if elevation data is confused with census data", {
  expect_error(plot_base_elevation(census), "Ensure your data set has")
})
