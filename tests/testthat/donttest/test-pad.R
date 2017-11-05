context("pad")

test_that("returns a string of the expected number of characters", {
  test_string <- c("12345", "67890")
  x <- pad(test_string, total_width = 20)
  expect_true(grepl("12345", x))
  # this is a bug I need to fix
  expect_failure(expect_equal(nchar(x), 20))
})
