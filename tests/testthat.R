library(testthat)
library(fgeo.plot)

expect_equal <- function(...) {
  testthat::expect_equal(..., check.environment = FALSE)
}

test_check("fgeo.plot")
