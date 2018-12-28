context("vft_1quad")

test_that("outputs equal", {
  expect_equal(vft_1quad(), pick_vft())
})

test_that("is sensitive to n_rows", {
  expect_length(pick_vft(n_rows = 5)[[1]], 5)
})
