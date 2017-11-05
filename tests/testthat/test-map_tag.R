library(dplyr)

# Minimal data
few_quads <- unique(ngelnyaki::ngelnyaki_vft_unid$QuadratName)[1:2]
vft <- ngelnyaki::ngelnyaki_vft_unid %>% filter(QuadratName %in% few_quads)

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
    any(grepl("ggplot", class(map_tag(vft))))
  )
})

# test_that("warns if vft is not of class 'vft'", {
#   expect_true(
#     any(
#       grepl("vft", class(vft))
#     )
#   )
# })
