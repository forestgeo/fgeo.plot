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
  expect_type(autoplot_by_species(sp(census)), "list")
  expect_type(autoplot_by_species(sp_elev(census, elevation_lst)), "list")
  expect_type(autoplot_by_species(sp_elev(census, elevation_df)), "list")

  expect_s3_class(autoplot_by_species(sp(census))[[1]], "ggplot")
  expect_s3_class(autoplot_by_species(sp_elev(census, elevation_lst))[[1]], "ggplot")
  expect_s3_class(autoplot_by_species(sp_elev(census, elevation_df))[[1]], "ggplot")
  expect_s3_class(autoplot_by_species(sp_elev(census))[[1]], "ggplot")
})

test_that("isn't sensitive to argument `hide_fill_legend`", {
  # testthat v3 does not pass check.environment = FALSE to all.equal
  testthat::local_edition(2)

  p <- autoplot_by_species(sp(census), hide_fill_legend = TRUE)
  q <- autoplot_by_species(sp(census), hide_fill_legend = FALSE)
  expect_equal(p, q, check.environment = FALSE)
})
