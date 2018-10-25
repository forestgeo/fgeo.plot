context("autoplot-sp_elev")

# Small dataset with a few species for quick examples
some_sp <- c("PREMON", "CASARB")
census <- subset(fgeo.data::luquillo_tree5_random, sp %in% some_sp)
elevation_lst <- fgeo.data::luquillo_elevation
elevation_df <- elevation_lst$col

test_that("returns a ggplot", {
  expect_is(autoplot(sp_elev(census, elevation_lst)), "gg")
  expect_is(autoplot(sp_elev(census, elevation_df)), "gg")
  expect_is(autoplot(sp(census)), "gg")
  expect_is(autoplot(elev(elevation_lst)), "gg")
  expect_is(autoplot(elev(elevation_df)), "gg")
})
