context("autoplot-sp_elev")

# Small dataset with a few species for quick examples
some_sp <- c("PREMON", "CASARB")
census <- subset(fgeo.data::luquillo_tree5_random, sp %in% some_sp)
elevation_lst <- fgeo.data::luquillo_elevation
elevation_df <- elevation_lst$col

test_that("fails with informative message", {
  expect_error(autoplot(census), "Can't deal with data")
  expect_error(autoplot(elevation_lst), "Can't deal with data")
  expect_error(autoplot_by_species(elevation_lst), "Can't deal with data")
})

test_that("returns a ggplot", {
  expect_is(autoplot(sp_elev(census, elevation_lst)), "gg")
  expect_is(autoplot(sp_elev(census, elevation_df)), "gg")
  expect_is(autoplot(sp(census)), "gg")
  expect_is(autoplot(elev(elevation_lst)), "gg")
  expect_is(autoplot(elev(elevation_df)), "gg")
})



context("sp_elev")

test_that("sp_elev()) and friends return expected class", {
  expect_is(sp(census), "data.frame")
  expect_is(sp(census), "sp")
  
  expect_is(elev(elevation_lst), "list")
  expect_is(elev(elevation_df), "data.frame")
  expect_is(elev(elevation_df), "elev")
  expect_is(elev(elevation_lst), "elev")
})

