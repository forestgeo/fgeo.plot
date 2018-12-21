context("suffix_match")

test_that("tags a vector", {
  actual <- suffix_match(
    c("tag1", "tag2"),
    c("dead", "whatever"),
    "dead",
    ".d"
  )
  expected <- c("tag1.d", "tag2")
  expect_equal(actual, expected)
})

test_that("with numeric `tag` does not abort -- only warns (#13)", {
  expect_warning(
    suffix_match(
      as.numeric(c("000", "001")),
      c("dead", "whatever"),
      "dead",
      ".d"
    )
  )
})


test_that("warns if no stem is dead", {
  expect_warning(
    suffix_match(
      c("tag1", "tag2"),
      c("not-dead", "not-dead"),
      "dead",
      ".d"
    )
  )
})

test_that("fails if x, status, and suffix are not character vectors", {
  # Passes
  expect_equal(
    suffix_match(
      c("tag1", "tag2"),
      c("dead", "whatever"),
      "dead",
      "_suffix"
    ),
    c("tag1_suffix", "tag2")
  )

  # Warn
  expect_warning(
    suffix_match(
      1,
      c("dead", "whatever"),
      "dead",
      "_suffix"
    )
  )
  expect_error(
    suffix_match(
      c("tag1", "tag2"),
      1,
      "dead",
      "_suffix"
    )
  )
  expect_error(
    suffix_match(
      c("tag1", "tag2"),
      c("dead", "whatever"),
      "dead",
      suffix = 1
    )
  )

  expect_warning(
    suffix_match(
      c("tag1", "tag2"),
      c("dead", "whatever"),
      "D",
      "_suffix"
    )
  )
})
