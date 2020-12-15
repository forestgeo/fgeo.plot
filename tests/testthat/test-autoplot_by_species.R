context("autoplot_by_species")

# Small dataset with a few species for quick examples
some_sp <- c("PREMON", "CASARB")
census <- subset(fgeo.x::tree5, sp %in% some_sp)
elevation_lst <- fgeo.x::elevation
elevation_df <- elevation_lst$col

test_that("fails with informative message", {
  expect_error(autoplot_by_species(elevation_lst), "Can't deal with data")
  expect_error(autoplot_by_species(elev(elevation_lst)), "Can't deal with data")
})

test_that("output expected ggplot", {
  expect_is(autoplot_by_species(sp(census)), "list")
  expect_is(autoplot_by_species(sp_elev(census, elevation_lst)), "list")
  expect_is(autoplot_by_species(sp_elev(census, elevation_df)), "list")

  expect_is(autoplot_by_species(sp(census))[[1]], "ggplot")
  expect_is(autoplot_by_species(sp_elev(census, elevation_lst))[[1]], "ggplot")
  expect_is(autoplot_by_species(sp_elev(census, elevation_df))[[1]], "ggplot")
  expect_is(autoplot_by_species(sp_elev(census))[[1]], "ggplot")
})

test_that("isn't sensitive to argument `hide_fill_legend`", {
  p <- autoplot_by_species(sp(census), hide_fill_legend = TRUE)
  q <- autoplot_by_species(sp(census), hide_fill_legend = FALSE)
  expect_equal(p, q)
})
