context("datasets")

test_that("exists", {
  expect_true(fgeo.base::exists_in_pkg("top4quad", "fgeo.map"))
  expect_true(fgeo.base::exists_in_pkg("top1quad", "fgeo.map"))
})

test_that("remains unchanged", {
  expect_known_output(top4quad, "top4quad.csv")
  expect_known_output(top1quad, "top1quad.csv")
})
