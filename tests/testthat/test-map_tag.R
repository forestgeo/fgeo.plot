library(dplyr)
library(purrr)
library(gridExtra)
library(fgeo.utils)

# Minimal data

# Fix some odd variables of this particular data set
# See email by Suzanne Lao https://goo.gl/UwiRbj
minivft <- dplyr::rename(bciex::bci12vft_mini, QX = x, QY = y) %>%
  tibble::rowid_to_column("DBHID")

few_quads <- unique(minivft$QuadratName)[1]
vft <- minivft %>% filter(QuadratName %in% few_quads)

context("test-map_tag.R")

# Redo once the plot is stable
test_that("outputs same as satisfactory reference", {
  plots <- suppressWarnings(map_tag(vft)[1])
  reference <- plots[[1]][["data"]]
  expect_known_output(reference, "ref_map_tag.csv")
})


test_that("map_tag() stops if data has more than one PlotID", {
  new_row <-   vft[1, ]
  new_row$PlotID <- 2L
  vft_with_new_row <- bind_rows(new_row, vft)
  expect_error(map_tag(vft_with_new_row))
})

test_that("plots all unique tags in data", {
  plots <- suppressWarnings(map_tag(vft))
  unique_tags_in_plot_n <- plots %>%
    map_df("data") %>%
    select(tag) %>%
    unique() %>%
    pull() %>%
    length()
  unique_tags_in_plot_n
  unique_tags_in_data_n <- unique(vft$Tag) %>% length()

  expect_equal(unique_tags_in_plot_n, unique_tags_in_data_n)
})


test_that("plots the same with all or just the minimum needed vars in data", {
  all <- suppressWarnings(map_tag(vft)[1])
  vft_with_min_vars <- vft %>%
    select(Tag, Status, QX, QY, QuadratName, DBHID, CensusID, PlotID)
  min <- suppressWarnings(map_tag(vft_with_min_vars)[1])

  expect_equal(all, min)
})



test_that("errs with uppercase names", {
  vft_no_qx <- vft
  vft_no_qx$QX <- NULL
  expect_error(
    map_tag(vft_no_qx),
    "Ensure your data set has these variables"
  )
})

test_that("outputs a ggplot", {
  result <- suppressWarnings(map_tag(vft)[1])
  expect_true(
    any(grepl("ggplot", class(result[[1]])))
  )
})









# Updated tests -----------------------------------------------------------

context("map_tag")

vft <- bciex::bci12vft_mini %>%
  dplyr::rename(QX = x, QY = y) %>%
  fgeo.utils::top(QuadratID) %>%
  fgeo.utils::top(CensusID) %>%
  dplyr::sample_n(100)

test_that("outputs a ggplot", {
  first_map <- map_tag(vft)[[1]]
  expect_true(
    any(grepl("ggplot", class(first_map)))
  )
})

test_that("wrong inputs get noticed", {
  expect_error(
    map_tag(
     vft,
     # next line is wrong input
     title_quad = 1,
     point_shape = c(0, 2),
     point_size = 6,
     tag_size = 10
    )
  )
  expect_error(
    map_tag(
      vft,
      title_quad = "my site",
      # next line is wrong input
      point_shape = c("a", "b"),
      point_size = 6,
      tag_size = 10
    )
  )
  expect_error(
    map_tag(
      vft,
      title_quad = "my site",
      point_shape = c(1, 2),
      # next line is wrong input
      point_size = "a",
      tag_size = 10
    )
  )
  expect_error(
    map_tag(
      vft,
      title_quad = "my site",
      point_shape = c(1, 2),
      point_size = 1,
      # next line is wrong input
      tag_size = "a"
    )
  )
})
