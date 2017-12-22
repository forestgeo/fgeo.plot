context("four_quadrat")

test_that("exists", {
  expect_true(exists_in_pkg("four_quadrats", "map"))
})

test_that("remains unchanged", {
  expect_known_output(four_quadrats, "ref_four_quadrats.csv")
})

test_that("exists", {
  expect_true(exists_in_pkg("one_quadrat", "map"))
})

test_that("remains unchanged", {
  expect_known_output(four_quadrats, "ref_one_quadrat.csv")
})
