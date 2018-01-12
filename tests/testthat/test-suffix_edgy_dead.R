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

