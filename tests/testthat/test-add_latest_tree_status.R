context("add_latest_tree_status")

test_that("output is as expected", {
  df <- sinharaja::sinh_q20[[1]]
  expect_type(add_latest_tree_status(df), "list")
  expect_true(
    any(
      grepl("latest_tree_status", names(add_latest_tree_status(df)))
    )
  )
})
