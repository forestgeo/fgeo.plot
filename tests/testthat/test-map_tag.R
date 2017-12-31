library(dplyr)
library(purrr)

context("map_tag")

map_list <- map_tag(one_quadrat)

# Outputs -----------------------------------------------------------------

test_that("output is correct", {
  # Class is correct
  expect_true(any(grepl("ggplot", class(map_list[[1]]))))

  # Underlying output-data didn't change
  reference <- map_list[[1]][["data"]]
  expect_known_output(reference, "ref_map_tag.csv")

  # Plots all unique tags in data
  unique_tags_in_plot_n <- map_list %>%
    map_df("data") %>%
    select(tag) %>%
    unique() %>%
    pull() %>%
    length()
  unique_tags_in_plot_n
  unique_tags_in_data_n <- unique(one_quadrat$Tag) %>% length()
  expect_equal(unique_tags_in_plot_n, unique_tags_in_data_n)

  # plots the same with all or just the minimum needed vars in data
  all_vars <- map_list[[1]]$data
  min_vars <- one_quadrat %>%
    select(Tag, QX, QY, Status, QuadratName, CensusID, PlotID) %>%
    map_tag() %>%
    .[[1]] %>%
    .$data
  expect_equal(all_vars, min_vars)
})

# Inputs ------------------------------------------------------------------

test_that("wrong inputs get noticed", {

  expect_error(
   # data not a dataframe
    map_tag(1)
  )

  expect_error(
    map_tag(
     # data has cero rows
     filter(one_quadrat, CensusID == 999)
    )
  )

  # Missing name
  no_qx <- one_quadrat
  no_qx$QX <- NULL
  expect_error(map_tag(no_qx), "Ensure")

  # wrong type
  expect_error(map_tag(one_quadrat, x_q = "a"))
  expect_error(map_tag(one_quadrat, x_sq = "a"))
  expect_error(map_tag(one_quadrat, y_q = "a"))
  expect_error(map_tag(one_quadrat, y_sq = "a"))

  expect_error(
    map_tag(
     one_quadrat,
     # wrong type
     title_quad = 1
    )
  )

  expect_error(
    map_tag(
      one_quadrat,
      # wrong type
      point_shape = c("a", "b")
    )
  )

  expect_error(
    map_tag(
      one_quadrat,
      # wrong type
      point_size = "a"
    )
  )

  expect_error(
    map_tag(
      one_quadrat,
      # wrong type
      tag_size = "a"
    )
  )

  expect_error(
    map_tag(
      one_quadrat,
      # wrong type
      header = 1
    )
  )

  expect_error(
    map_tag(
      one_quadrat,
      # wrong type
      theme = 1
    )
  )

  expect_error(
    map_tag(
      one_quadrat,
      # wrong type
      move_edge = "a"
    )
  )

  # stops if data has more than one PlotID"
  dup_plotid <-   one_quadrat[1, ]
  dup_plotid$PlotID <- 999L
  w_dup_plotid <- dplyr::bind_rows(dup_plotid, one_quadrat)
  expect_error(x <- map_tag(w_dup_plotid), "Duplicated")

  # warns if data has more than one CensusID"
  dup_cnsid <-   one_quadrat[1, ]
  dup_cnsid$CensusID <- 999L
  w_dup_cnsid <- dplyr::bind_rows(dup_cnsid, one_quadrat)
  expect_warning(x <- map_tag(w_dup_cnsid), "Likely")
})

