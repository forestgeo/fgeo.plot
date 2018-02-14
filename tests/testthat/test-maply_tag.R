library(dplyr)
library(purrr)

context("maply_tag")

map_list <- maply_tag(top1quad)

# Outputs -----------------------------------------------------------------

test_that("output is correct", {
  # Class is correct
  expect_true(any(grepl("ggplot", class(map_list[[1]]))))

  # Underlying output-data didn't change
  reference <- map_list[[1]][["data"]]
  expect_known_output(reference, "ref_maply_tag.csv")

  # Plots all unique tags in data
  unique_tags_in_plot_n <- map_list %>%
    map_df("data") %>%
    select(tag) %>%
    unique() %>%
    pull() %>%
    length()
  unique_tags_in_plot_n
  unique_tags_in_data_n <- unique(top1quad$Tag) %>% length()
  expect_equal(unique_tags_in_plot_n, unique_tags_in_data_n)

  # plots the same with all or just the minimum needed vars in data
  all_vars <- map_list[[1]]$data
  min_vars <- top1quad %>%
    select(Tag, QX, QY, Status, QuadratName, CensusID, PlotID) %>%
    maply_tag() %>%
    .[[1]] %>%
    .$data
  expect_equal(all_vars, min_vars)
})

# Inputs ------------------------------------------------------------------

test_that("works with all defaults given in usage", {
  expect_silent(
    maply_tag(
      top1quad,
      x_q = 20,
      x_sq = 5,
      y_q = 20,
      y_sq = 5,
      subquad_offset = NULL,
      bl = 1,
      br = 2,
      tr = 3,
      tl = 4,
      title_quad = "Site Name, YYYY. Quadrat:",
      show_page = TRUE,
      show_subquad = TRUE,
      point_shape = c(19, 4),
      point_size = 1.5,
      tag_size = 3,
      header = map_tag_header(),
      theme = theme_map_tag(),
      move_edge = 0
    )
  )
})



test_that("handles wrong type", {
  crucial <- c("tag", "qx", "qy", "status", "quadratname", "censusid", "plotid")
  crucial_nms <- paste0(crucial, collapse = "|")
  x <- top1quad %>% 
    select(matches(crucial_nms)) 
  
  x <- map_df(x, as.character)
  expect_warning(maply_tag(x))
})

test_that("wrong inputs get noticed", {

  expect_error(
   # data not a dataframe
    maply_tag(1)
  )

  expect_error(
    maply_tag(
     # data has cero rows
     filter(top1quad, CensusID == 999)
    )
  )

  # Missing name
  no_qx <- top1quad
  no_qx$QX <- NULL
  expect_error(maply_tag(no_qx), "Ensure")

  # wrong type
  expect_error(maply_tag(top1quad, x_q = "a"))
  expect_error(maply_tag(top1quad, x_sq = "a"))
  expect_error(maply_tag(top1quad, y_q = "a"))
  expect_error(maply_tag(top1quad, y_sq = "a"))
  expect_error(maply_tag(top1quad, subquad_offset = "not -1"))

  expect_error(maply_tag(top1quad, subquad_offset = 0))

  # wrong length
  expect_error(maply_tag(top1quad, bl = 1:2))
  expect_error(maply_tag(top1quad, br = 1:2))
  expect_error(maply_tag(top1quad, tr = 1:2))
  expect_error(maply_tag(top1quad, tl = 1:2))

  expect_error(
    maply_tag(
     top1quad,
     # wrong type
     title_quad = 1
    )
  )

  expect_error(maply_tag(top1quad, show_page = "not logical"))

  expect_error(maply_tag(top1quad, show_subquad = "not logical"))

  expect_error(
    maply_tag(
      top1quad,
      # wrong type
      point_shape = c("a", "b")
    )
  )

  expect_error(
    maply_tag(
      top1quad,
      # wrong type
      point_size = "a"
    )
  )

  expect_error(
    maply_tag(
      top1quad,
      # wrong type
      tag_size = "a"
    )
  )

  expect_error(
    maply_tag(
      top1quad,
      # wrong type
      header = 1
    )
  )

  expect_error(
    maply_tag(
      top1quad,
      # wrong type
      theme = 1
    )
  )

  expect_error(
    maply_tag(
      top1quad,
      # wrong type
      move_edge = "a"
    )
  )

  # stops if data has more than one PlotID"
  dup_plotid <-   top1quad[1, ]
  dup_plotid$PlotID <- 999L
  w_dup_plotid <- dplyr::bind_rows(dup_plotid, top1quad)
  expect_error(x <- maply_tag(w_dup_plotid), "Duplicated")

  # warns if data has more than one CensusID"
  dup_cnsid <-   top1quad[1, ]
  dup_cnsid$CensusID <- 999L
  w_dup_cnsid <- dplyr::bind_rows(dup_cnsid, top1quad)
  expect_warning(x <- maply_tag(w_dup_cnsid), "Likely")
})

test_that("page labels can be changed", {
  plots <- 1:2
  maps <- maply_tag(top4quad,
    bl = "a", br = "b", tr = "c", tl = "d",
    show_page = FALSE
  )[plots]
  page <- unique(purrr::map_df(maps, "data")$page)
  expect_equal(page, letters[plots])
})

test_that("argument subquad_offset works as expected", {
  x <- maply_tag(top1quad, subquad_offset = -1)
  subquads <- unique(purrr::map_df(x, "data")$subquadrat)
  expect_true("01" %in% subquads)

  x <- maply_tag(top1quad, subquad_offset = -1)
  subquads <- unique(purrr::map_df(x, "data")$subquadrat)
  expect_true("01" %in% subquads)
})

test_that("outputs quadrats in order, even if QuadratName is numeric (#33)", {
  tricky_quad <- c("0100", "0101", "1000")
  expect_nms <- map(tricky_quad, paste0, "_", 1:4) %>% reduce(c)
  
  # Create some data
  vft_toy <- fgeo.tool::row_top(top4quad, QuadratName, 3)
  vft_toy <- mutate(
    vft_toy,
    QuadratName = recode(QuadratName,
      "4917" = "0100",
      "4916" = "0101",
      "4915" = "1000"
    )
  )
  
  good <- maply_tag(vft_toy)
  expect_equal(names(good), expect_nms)
  
  not_chr <- expect_warning(
    maply_tag(mutate(vft_toy, QuadratName = as.numeric(QuadratName)))
  )
  expect_equal(names(not_chr), expect_nms)
})

test_that("warns if option max.print is not high enough", {
  old_options <- options()
  options(max.print = 2)
  expect_warning(
    p <- maply_tag(top1quad)
  )
  options(old_options)
  
  old_options <- options()
  options(max.print = 4)
  expect_silent(
    p <- maply_tag(top1quad)
  )
  options(old_options)
})



context("curate_point_shape")

test_that("outputs the correct element(s) of point_shape", {
  ps <- c(1, 2)
  sa <- "alive"
  sd <- "dead"
  
  x <- tibble::tibble(status_tree = c("alive", "dead"))
  expect_equal(curate_point_shape(x, ps, sa, sd), c(1, 2))
  
  x <- tibble::tibble(status_tree = c("alive"))
  expect_equal(curate_point_shape(x, ps, sa, sd), 1)
  
  x <- tibble::tibble(status_tree = c("dead"))
  expect_equal(curate_point_shape(x, ps, sa, sd), 2)
})
