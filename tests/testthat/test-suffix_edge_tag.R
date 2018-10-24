library(tibble)

context("suffix_tags_beyond_edge")

test_that("errs with wrong input", {
  expect_message(
    suffix_tags_beyond_edge(
      x = tibble(QX = 21, QY = 21, tag = "01", status = "dead"), 
      .match = "dead",
      suffix = "suffix", 
      x_q = 20
    ) 
  )
  expect_message(
    suffix_tags_beyond_edge(
      x = tibble(QX = 20, QY = 20, tag = "01", status = "dead"), 
      .match = "dead",
      suffix = "suffix", 
      x_q = 20
    ) 
  )
  
  expect_error(
    suffix_tags_beyond_edge(x = "not dfm", 
      .match = "dead",
      suffix = "suffix", 
      x_q = 20
    )  
  )
  expect_error(
    suffix_tags_beyond_edge(
      x = tibble(x = 21), 
      .match = "dead",
      suffix = "suffix", 
      x_q = 20
    )  
  )
  expect_error(
    suffix_tags_beyond_edge(
      x = tibble(y = 21), 
      .match = "dead",
      suffix = "suffix", 
      x_q = 20
    )  
  )
  expect_error(
    suffix_tags_beyond_edge(
      x = tibble(a = 21), 
      .match = "dead",
      suffix = "suffix", 
      x_q = 20
    )  
  )
  
})



context("detect_spillover")

test_that("asserts correctly", {
  expect_false(
    expect_message(
      detect_spillover(x = tibble(qx = 20, qy = 20), x_q = 20, y_q = 20)
    )
  )
  expect_true(
    expect_message(
      detect_spillover(x = tibble(qx = 21, qy = 20), x_q = 20, y_q = 20)
    )
  )
  expect_true(
    expect_message(
      detect_spillover(x = tibble(qx = 20, qy = 21), x_q = 20, y_q = 20)
    )
  )
})

