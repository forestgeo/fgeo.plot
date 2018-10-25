context("sp_elev")

# Small dataset with a few species for quick examples
some_sp <- c("PREMON", "CASARB")
census <- subset(fgeo.data::luquillo_tree5_random, sp %in% some_sp)
elevation_lst <- fgeo.data::luquillo_elevation
elevation_df <- elevation_lst$col

test_that("sp_elev()) and friends return expected class", {
  expect_is(sp(census), "data.frame")
  expect_is(sp(census), "sp")
  
  expect_is(elev(elevation_lst), "list")
  expect_is(elev(elevation_df), "data.frame")
  expect_is(elev(elevation_df), "elev")
  expect_is(elev(elevation_lst), "elev")
})

