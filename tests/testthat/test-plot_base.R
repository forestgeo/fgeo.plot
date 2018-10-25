context("plot_base")

# Small dataset with a few species for quick examples
some_sp <- c("PREMON", "CASARB")
census <- subset(fgeo.data::luquillo_tree5_random, sp %in% some_sp)
elevation_lst <- fgeo.data::luquillo_elevation
elevation_df <- elevation_lst$col

test_that("errs with informative message", {
  expect_error(plot_base(1), "Can't deal with data")
})

test_that("returns a gg object with census and elevation data", {
  expect_is(plot_base(sp(census)), "gg")
  expect_is(plot_base(elev(elevation_lst)), "gg")
  expect_is(plot_base(elev(elevation_df)), "gg")
})
