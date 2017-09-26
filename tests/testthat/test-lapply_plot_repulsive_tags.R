context("lapply_plot_repulsive_tags")

library(dplyr)
library(ggplot2)

list_of_one_quadrat <- sinharaja::sinh_q20[1]

ond_dataframe <- list_of_one_quadrat %>%
  prep_repulsive_tags() %>%
  .[[1]]

test_that("errs with wrong input passed to the first few arguments", {
  p <- list_of_one_quadrat %>%
    prep_repulsive_tags() %>%
    lapply_plot_repulsive_tags("site")
  expect_type(p, "list")
  expect_equal(class(p[[1]]), c("gg", "ggplot"))

  # Expect failure because this needs a list of dataframes
  expect_error({
    one_dataframe %>%
      prep_repulsive_tags() %>%
    lapply_plot_repulsive_tags("site")
  })

  # should fail if site_name is not a character.
  expect_error({
    list_of_one_quadrat %>%
      prep_repulsive_tags() %>%
      lapply_plot_repulsive_tags(site_name = as.factor("site"))
  })
})

