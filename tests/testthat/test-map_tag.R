library(dplyr)

# Minimal data
few_quads <- unique(ngelnyaki::ngelnyaki_vft_unid$QuadratName)[1]
vft <- ngelnyaki::ngelnyaki_vft_unid %>% filter(QuadratName %in% few_quads)
result <- map_tag(vft)

context("test-map_tag.R")

test_that("errs with uppercase names", {
  vft_renamed <- vft
  vft_renamed$QX <- NULL
  expect_error(
    map_tag(vft_renamed),
    "Ensure your data has names"
  )
})

test_that("outputs a ggplot", {
  expect_true(
    any(grepl("ggplot", class(result[[1]])))
  )
})

context("test-add_subquadrat")

# From a dataframe of one quadrat, get a list of 4 subquadrats.
one_quadrat <- toy_list[[1]]
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

