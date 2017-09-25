context("add_subquadrat")

# From a dataframe of one quadrat, get a list of 4 subquadrats.
one_quadrat <- sinharaja::sinh_q20[[15]]
head(one_quadrat)
lapply(add_subquadrat(one_quadrat), head)

test_that("first argument is a data.frame or fails", {
  # Force class data.frame. Here trying list to test that it fails.
  expect_error(add_subquadrat(list(one_quadrat)))
})

test_that("output is a list of four dataframes", {
  expect_type(add_subquadrat(one_quadrat), "list")
  expect_type(add_subquadrat(one_quadrat)[[1]], "list")
  expect_true(
    assertive.types::is_data.frame(add_subquadrat(one_quadrat)[[1]])
  )
  expect_length(add_subquadrat(one_quadrat), 4)
})

