context("caption_tag_xy.R")

test_that("returns expected string", {
  expected <- "1:(1, 1); 2:(2, 2); 3:(3, 3)"
  actual <- caption_tag_xy(1:3, 1:3, 1:3, NULL)
  expect_equal(actual, expected)

  expected <- "1:(1, 1); 2:(2, 2) ... and 1 more."
  actual <- caption_tag_xy(1:3, 1:3, 1:3, 2)
  expect_equal(actual, expected)

  expected <- "1:(1, 1) ... and 2 more."
  actual <- caption_tag_xy(1:3, 1:3, 1:3, 1)
  expect_equal(actual, expected)

  expected <- "1:(1, 1); 2:(2, 2); 3:(3, 3)"
  actual <- caption_tag_xy(1:3, 1:3, 1:3, 4)
  expect_equal(actual, expected)

  expected <- " ... and 3 more."
  actual <- caption_tag_xy(1:3, 1:3, 1:3, 0)
  expect_equal(actual, expected)

  expect_error(
    caption_tag_xy(1:3, 1:3, 1:3, NA)
  )
})
