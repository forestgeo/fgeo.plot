x <- tibble::tribble(
  ~CensusID, ~Tag, ~Status,
  1,    1, "alive",
  1,    1,  "dead",
  1,    2,  "dead",
  1,    2,  "dead",
  2,    1, "alive",
  2,    1, "alive",
  2,    2, "alive",
  2,    2,  "dead"
)

context("collapse_censusid")

test_that("outputs object of expected status and status_tree", {
  expect_equal(collapse_censusid(x)$Status, c("alive", rep("dead", 2), "alive"))
  collapsed <- fgeo.tool::add_status_tree(x, "alive", "dead") %>% 
    select(-Status) %>% 
    unique() %>% 
    collapse_censusid()
  expected_status_tree <- c("alive", "dead", "alive")
  expect_equal(collapsed$status_tree, expected_status_tree)
})

