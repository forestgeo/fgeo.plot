context("datasets")

test_that("exists", {
  expect_true(fgeo.base::exists_in_pkg("vft_4quad", "fgeo.map"))
  expect_true(fgeo.base::exists_in_pkg("vft_1quad", "fgeo.map"))
})

test_that("remains unchanged", {
  expect_known_output(vft_4quad, "vft_4quad.csv")
  expect_known_output(vft_1quad, "vft_1quad.csv")
})
