library(tibble)

context("suffix_edgy_dead")

test_that("errs with wrong input", {
  expect_message(
    suffix_edgy_dead(
      x = tibble(x = 21, y = 21, tag = "01", status = "dead"), 
      status_d = "dead",
      suffix = "suffix", 
      x_q = 20
    ) 
  )
  expect_message(
    suffix_edgy_dead(
      x = tibble(x = 20, y = 20, tag = "01", status = "dead"), 
      status_d = "dead",
      suffix = "suffix", 
      x_q = 20
    ) 
  )
  
  expect_error(
    suffix_edgy_dead(x = "not dfm", 
      status_d = "dead",
      suffix = "suffix", 
      x_q = 20
    )  
  )
  expect_error(
    suffix_edgy_dead(
      x = tibble(x = 21), 
      status_d = "dead",
      suffix = "suffix", 
      x_q = 20
    )  
  )
  expect_error(
    suffix_edgy_dead(
      x = tibble(y = 21), 
      status_d = "dead",
      suffix = "suffix", 
      x_q = 20
    )  
  )
  expect_error(
    suffix_edgy_dead(
      x = tibble(a = 21), 
      status_d = "dead",
      suffix = "suffix", 
      x_q = 20
    )  
  )
  
})



context("assert_spillover")

test_that("asserts correctly", {
  expect_false(
    expect_message(
      assert_spillover(x = tibble(x = 20, y = 20), x_q = 20, y_q = 20)
    )
  )
  expect_true(
    expect_message(
      assert_spillover(x = tibble(x = 21, y = 20), x_q = 20, y_q = 20)
    )
  )
  expect_true(
    expect_message(
      assert_spillover(x = tibble(x = 20, y = 21), x_q = 20, y_q = 20)
    )
  )
})



context("str_suffix_match")

test_that("tags a vector", {
  actual <- str_suffix_match(
    c("tag1", "tag2"),
    c("dead", "whatever"),
    "dead",
    ".d"
  )
  expected <- c("tag1.d", "tag2")
  expect_equal(actual, expected)
})

test_that("warns if no stem is dead", {
  expect_warning(
    str_suffix_match(
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
    str_suffix_match(
      c("tag1", "tag2"),
      c("dead", "whatever"),
      "dead",
      "_suffix"
    ), 
    c("tag1_suffix", "tag2")
  )
  
  # Fails
  expect_error(
    str_suffix_match(
      1,
      c("dead", "whatever"),
      "dead",
      "_suffix"
    )
  )
  expect_error(
    str_suffix_match(
      c("tag1", "tag2"),
      1,
      "dead",
      "_suffix"
    )
  )
  expect_error(
    str_suffix_match(
      c("tag1", "tag2"),
      c("dead", "whatever"),
      "dead",
      suffix = 1
    )
  )
  
  expect_warning(
    str_suffix_match(
      c("tag1", "tag2"),
      c("dead", "whatever"),
      "D",
      "_suffix"
    )
  )
})
