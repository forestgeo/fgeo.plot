context("datasets")

test_that("exists", {
  expect_true(fgeo.base::exists_in_pkg("vft_4quad", "fgeo.map"))
  expect_true(fgeo.base::exists_in_pkg("vft_1quad", "fgeo.map"))
})

test_that("remains unchanged", {
  expect_known_output(
    head(as.data.frame(fgeo.map::vft_4quad)), 
    "ref-vft_4quad.csv", 
    print = TRUE
  )
  expect_known_output(
    head(as.data.frame(fgeo.map::vft_1quad)), 
    "ref-vft_1quad.csv", 
    print = TRUE
  )
})
