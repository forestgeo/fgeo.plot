context("check_unique_vector")

single <- rep(1, 3)
num <- c(1:3)
chr <- c(letters[1:3])

test_that("behaves as expected", {
  expect_silent(check_unique_vector(single, "warning"))
  expect_warning(check_unique_vector(num, "warning"))
  expect_error(check_unique_vector(chr, "stop", "Do something"), "Do something")
})

.df <- data.frame(a = 1:3, b = 1, stringsAsFactors = FALSE)

test_that("returns the expected messages and output x", {
  expect_warning(check_unique(.df, "a"))
  expect_error(check_unique(.df, "a", "stop", "do this"), "do this")
  expect_silent(out <- check_unique(.df, "b", "stop", "do this"))
  expect_identical(.df, out)
})

context("check_unique_plotid")

test_that("works as expected", {
  expect_silent(check_unique_plotid(data.frame(plotid = c(1, 1))))
  expect_error(check_unique_plotid(data.frame(plotid = c(1, 2))))
})



context("check_unique_censusid")

test_that("works as expected", {
  expect_silent(check_unique_censusid(data.frame(censusid = c(1, 1))))
  expect_warning(
    check_unique_censusid(
      data.frame(censusid = c(1, 2))
    )
  )
})



context("add_status_tree")

test_that("the tree status is dead only if one stem is dead", {
  one_dead <- tibble::tibble(
    tag = c(
      1, 1,
      2, 2,
      3, 3
    ),
    status = c(
      "alive", "dead",
      "dead", "dead",
      "broken below", "missing"
    ),
    censusid = 1,
    plotid = 1
  )
  expected <- c(rep("alive", 2), rep("dead", 2), rep("alive", 2))
  out <- add_status_tree(one_dead)
  expect_equal(out$status_tree, expected)
  expect_is(out, "data.frame")
})

.df <- tibble::tribble(
  ~CensusID, ~Tag,  ~Status,
          1,    1,   "alive",
          1,    1,    "dead",
          1,    2,    "dead",
          1,    2,    "dead",

          2,    1,   "alive",
          2,    1,   "alive",
          2,    2,   "alive",
          2,    2,    "dead"
)

test_that("works even if data already contains the variable `status_tree`", {
  expect_silent(add_status_tree(add_status_tree(.df)))
})

test_that("outputs the correct variable status_tree", {
  exp <- c("alive", "alive", "dead", "dead", "alive", "alive", "alive", "alive")
  expect_identical(add_status_tree(.df)$status_tree, exp)
})



context("rm_dead_twice")

cns3 <- tibble::tribble(
  ~CensusID, ~Tag,  ~Status,
          3,    1,   "alive",
          3,    1,   "alive",
          3,    2,    "dead",
          3,    2,    "dead"
)

test_that("returns equal  to a known object", {
  expect_known_output(rm_dead_twice(.df), "ref_rm_dead_twice.csv")
})

test_that("adding a third census removes a first census", {
  with_cns3 <- dplyr::bind_rows(.df, cns3)
  out <- rm_dead_twice(with_cns3)
  expect_false(any(grepl(1, out$CensusID)))
})

