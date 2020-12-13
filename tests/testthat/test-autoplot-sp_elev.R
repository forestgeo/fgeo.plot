# autoplot-sp_elev

# Small dataset with a few species for quick examples
some_sp <- c("PREMON", "CASARB")
census <- subset(fgeo.x::tree5, sp %in% some_sp)
elevation_lst <- fgeo.x::elevation
elevation_df <- elevation_lst$col

test_that("fails with informative message", {
  expect_error(autoplot(census), "Can't deal with data")
  expect_error(autoplot(elevation_lst), "Can't deal with data")
  expect_error(autoplot_by_species(elevation_lst), "Can't deal with data")
})

test_that("returns a ggplot", {
  expect_s3_class(autoplot(sp_elev(census, elevation_lst)), "gg")
  expect_s3_class(autoplot(sp_elev(census, elevation_df)), "gg")

  expect_s3_class(autoplot(sp_elev(census, NULL)), "gg")
  expect_s3_class(autoplot(sp_elev(census)), "gg")

  expect_s3_class(autoplot(sp(census)), "gg")
  expect_s3_class(autoplot(elev(elevation_lst)), "gg")
  expect_s3_class(autoplot(elev(elevation_df)), "gg")
})

test_that("is sensitive to argument `hide_fill_legend`", {
  p <- autoplot(sp(census), hide_fill_legend = TRUE)
  q <- autoplot(sp(census), hide_fill_legend = FALSE)
  expect_false(identical(p, q))

  r <- autoplot(sp_elev(census), hide_fill_legend = TRUE)
  s <- autoplot(sp_elev(census), hide_fill_legend = FALSE)
  expect_false(identical(r, s))
})



# sp_elev

test_that("sp_elev()) and friends return expected class", {
  expect_s3_class(sp(census), "data.frame")
  expect_s3_class(sp(census), "sp")

  expect_s3_class(elev(elevation_lst), "list")
  expect_s3_class(elev(elevation_df), "data.frame")
  expect_s3_class(elev(elevation_df), "elev")
  expect_s3_class(elev(elevation_lst), "elev")
})

test_that("with elev = NULL doesn't throw error", {
  expect_error(sp_elev(census, NULL), NA)
})
