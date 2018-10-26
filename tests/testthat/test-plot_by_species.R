context("plot_by_species")

# Small dataset with a few species for quick examples
some_sp <- c("PREMON", "CASARB")
census <- subset(fgeo.data::luquillo_tree5_random, sp %in% some_sp)
elevation_lst <- fgeo.data::luquillo_elevation
elevation_df <- elevation_lst$col

test_that("fails with informative message", {
  expect_error(plot_by_species(elevation_lst), "Can't deal with data")
  expect_error(plot_by_species(elev(elevation_lst)), "Can't deal with data")
})

test_that("output expected ggplot", {
  plot_by_species(sp(census))
  plot_by_species(sp_elev(census, elevation_lst))
  plot_by_species(sp_elev(census, elevation_df))
})

