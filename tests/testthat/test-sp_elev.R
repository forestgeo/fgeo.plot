context("sp_elev")

library(dplyr)

test_that("sp_elev() is silent if `sp` and `elev` share plot dimensions", {
  species_from_luquillo <- fgeo.x::stem5
  elevation_from_luquillo <- fgeo.x::elevation
  expect_silent(
    sp_elev(species_from_luquillo, elevation_from_luquillo)
  )
})

test_that("sp_elev() warns if `sp` and `elev` have different plot dimensions", {
  species_from_luquillo <- fgeo.x::stem5
  elevation_from_luquillo <- fgeo.tool::fgeo_elevation(fgeo.x::elevation)
  
  shrink_gx <- round(max(elevation_from_luquillo$gx, na.rm = TRUE) / 2)
  smaller_elev <- elevation_from_luquillo %>% 
    filter(gx < shrink_gx)

  expect_warning(
    sp_elev(species_from_luquillo, smaller_elev),
    "`sp` and `elev`.*different dimensions"
  )
})
