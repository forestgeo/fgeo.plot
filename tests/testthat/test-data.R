context("datasets")

test_that("exists", {
  expect_true(fgeo.utils::exists_in_pkg("four_quadrats", "map"))
  expect_true(fgeo.utils::exists_in_pkg("one_quadrat", "map"))
  expect_true(fgeo.utils::exists_in_pkg("top4quad", "map"))
  expect_true(fgeo.utils::exists_in_pkg("top1quad", "map"))
})

test_that("remains unchanged", {
  expect_known_output(four_quadrats, "ref_four_quadrats.csv")
  expect_known_output(one_quadrat, "ref_one_quadrat.csv")
  expect_known_output(top4quad, "top4quad.csv")
  expect_known_output(top1quad, "top1quad.csv")
})
