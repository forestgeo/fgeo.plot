context("prep_repulsive_tags")

list_of_dataframes <- sinharaja::sinh_q20["16"]
prepared <- prep_repulsive_tags(list_of_dataframes)[1:2]

test_that("output is as expected", {

  nms <- names(prepared)
  is_padded <- all(c("0016-1", "0016-2") %in% nms)
  expect_true(is_padded)
  nms <- NULL



  nms <- names(prepared[[1]])

  has_latest_tree_status <- any(grepl("latest_tree_status", nms))
  expect_true(has_latest_tree_status)

  has_id <- any(grepl("id", nms))
  expect_true(has_id)

  has_limits <- any(nms %in% c("x1", "x2", "y1", "y2"))
  expect_true(has_limits)

  expect_type(prepared, "list")
  expect_true(
    assertive::is_data.frame(prepared[[1]])
  )
})

