context("collapse_censusid")

test_that("outputs object of expected status and status_tree", {
  skip_if_not_installed("fgeo.tool")

  x <- data.frame(
    CensusID = c(1, 1, 1, 1, 2, 2, 2, 2),
    TreeID = c(1, 1, 2, 2, 1, 1, 2, 2),
    Tag = c(1, 1, 2, 2, 1, 1, 2, 2),
    Status = c("alive", "dead", "dead", "dead", "alive", "alive", "alive", "dead"),
    stringsAsFactors = FALSE
  )

  expect_equal(collapse_censusid(x)$Status, c("alive", rep("dead", 2), "alive"))
  x <- fgeo.tool::add_status_tree(x, "alive", "dead")
  x$Status <- NULL
  collapsed <- collapse_censusid(unique(x))
  expected_status_tree <- c("alive", "dead", "alive")
  expect_equal(collapsed$status_tree, expected_status_tree)
})
