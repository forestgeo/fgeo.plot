library(vetr)
library(dplyr)

# Minimal data
few_quads <- unique(ngelnyaki::ngelnyaki_vft_unid$QuadratName)[1]
vft <- ngelnyaki::ngelnyaki_vft_unid %>% filter(QuadratName %in% few_quads)



context("test-map_tag.R")

test_that("errs with uppercase names", {
  vft_no_qx <- vft
  vft_no_qx$QX <- NULL
  expect_error(
    map_tag(vft_no_qx),
    "Ensure your data has names"
  )
})

test_that("outputs a ggplot", {
  result <- map_tag(vft)
  expect_true(
    any(grepl("ggplot", class(result[[1]])))
  )
})



context("test-add_subquadrat")

lower_names_then_check <- function(x, nms) {
  # check names
  x <- setNames(vft, tolower(names(vft)))
  check_crucial_names(x, nms)
  x
}

test_that("outputs a dataframe with new expected variable", {
  vft2 <- lower_names_then_check(vft, nms = c("tag", "qx", "qy", "status"))
  result <- add_subquadrat(df = vft2, 20, 20, 5, 5)
  expect_named(result, c(names(vft2), "subquadrat"))
  expect_is(result, "data.frame")
})
