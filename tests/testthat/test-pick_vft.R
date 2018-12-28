context("pick_vft")

test_that("is sensitive to `n_rows`", {
  expect_length(
    pick_vft(fgeo.x::vft_4quad, n_rows = 5)[[1]], 
    5
  )
})

test_that("defaults to picking all censuses", {
  result <- pick_vft(fgeo.x::vft_4quad)
  expect_equal(
    unique(fgeo.x::vft_4quad$CensusID),
    unique(result$CensusID)
  )
})

test_that("defaults to picking all quadrats", {
  result <- pick_vft(fgeo.x::vft_4quad)
  expect_equal(
    unique(fgeo.x::vft_4quad$QuadratID),
    unique(result$QuadratID)
  )
})
