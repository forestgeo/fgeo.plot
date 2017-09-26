context("plot_repulsive_tags")

library(dplyr)
library(ggplot2)

list_of_one_quadrat <- toy_list[1]

one_dataframe <- list_of_one_quadrat %>%
  prep_repulsive_tags() %>%
  .[[1]]


test_that("errs with wrong input passed to the first few arguments", {
    # should pass
    p <- one_dataframe %>%
      plot_repulsive_tags(
        site_name = "Toy",
        point_shape = c(19, 4),
        point_size = 1.5,
        tag_size = 3,
        header = get_header(),
        theme = get_theme()
      )
    expect_equal(class(p), c("gg", "ggplot"))

    # should fail becase a list is passed intead of a data frame.
    expect_error({
      list_of_one_quadrat %>%
        plot_repulsive_tags(
          site_name = "Toy",
          point_shape = c(19, 4),
          point_size = 1.5,
          tag_size = 3,
          header = get_header(),
          theme = get_theme()
        )
    })

    expect_error({
      p <- one_dataframe %>%
        plot_repulsive_tags(
          site_name = as.factor("Toy"),  # factors should fail
          point_shape = c(19, 4),
          point_size = 1.5,
          tag_size = 3,
          header = get_header(),
          theme = get_theme()
        )
    })
})
