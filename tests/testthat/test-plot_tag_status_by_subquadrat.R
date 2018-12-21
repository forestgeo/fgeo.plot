library(dplyr)
library(purrr)

vft_1quad <- vft_1quad()
vft_4quad <- fgeo.x::vft_4quad

map_list <- plot_tag_status_by_subquadrat(vft_1quad)



context("plot_tag_status_by_subquadrat")

# Outputs -----------------------------------------------------------------

test_that("output is correct", {
  # Class is correct
  expect_true(any(grepl("ggplot", class(map_list[[1]]))))

  # Underlying output-data didn't change
  reference <- map_list[[1]][["data"]]
  expect_known_output(
    head(as.data.frame(reference)), 
    "ref-plot_tag_status_by_subquadrat.csv",
    print = TRUE,
    overwrite = FALSE
  )

  # Plots all unique tags in data
  unique_tags_in_plot_n <- map_list %>%
    map_df("data") %>%
    select(tag) %>%
    unique() %>%
    pull() %>%
    length()
  unique_tags_in_plot_n
  unique_tags_in_data_n <- unique(vft_1quad$Tag) %>% length()
  expect_equal(unique_tags_in_plot_n, unique_tags_in_data_n)

  # plots the same with all or just the minimum needed vars in data
  all_vars <- map_list[[1]]$data
  min_vars <- vft_1quad %>%
    select(Tag, TreeID, QX, QY, Status, QuadratName, CensusID, PlotID) %>%
    plot_tag_status_by_subquadrat() %>%
    .[[1]] %>%
    .$data
  expect_equal(all_vars, min_vars)
})

# Inputs ------------------------------------------------------------------

test_that("works with all defaults given in usage", {
  expect_silent(
    plot_tag_status_by_subquadrat(
      vft_1quad,
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
      header = header_tag_status(),
      theme = theme_tag_status(),
      move_edge = 0
    )
  )
})



test_that("handles wrong type", {
  crucial <- c(
    "tag", "treeid", "qx", "qy", "status", "quadratname", "censusid", "plotid"
  )
  crucial_nms <- paste0(crucial, collapse = "|")
  x <- vft_1quad %>% 
    select(matches(crucial_nms)) 
  
  x <- map_df(x, as.character)
  expect_warning(plot_tag_status_by_subquadrat(x))
})

test_that("wrong inputs get noticed", {

  expect_error(
   # data not a dataframe
    plot_tag_status_by_subquadrat(1)
  )

  expect_error(
    plot_tag_status_by_subquadrat(
     # data has cero rows
     filter(vft_1quad, CensusID == 999)
    )
  )

  # Missing name
  no_qx <- vft_1quad
  no_qx$QX <- NULL
  expect_error(plot_tag_status_by_subquadrat(no_qx), "Ensure")

  # wrong type
  expect_error(plot_tag_status_by_subquadrat(vft_1quad, x_q = "a"))
  expect_error(plot_tag_status_by_subquadrat(vft_1quad, x_sq = "a"))
  expect_error(plot_tag_status_by_subquadrat(vft_1quad, y_q = "a"))
  expect_error(plot_tag_status_by_subquadrat(vft_1quad, y_sq = "a"))
  expect_error(plot_tag_status_by_subquadrat(vft_1quad, subquad_offset = "not -1"))

  expect_error(plot_tag_status_by_subquadrat(vft_1quad, subquad_offset = 0))

  # wrong length
  expect_error(plot_tag_status_by_subquadrat(vft_1quad, bl = 1:2))
  expect_error(plot_tag_status_by_subquadrat(vft_1quad, br = 1:2))
  expect_error(plot_tag_status_by_subquadrat(vft_1quad, tr = 1:2))
  expect_error(plot_tag_status_by_subquadrat(vft_1quad, tl = 1:2))

  expect_error(
    plot_tag_status_by_subquadrat(
     vft_1quad,
     # wrong type
     title_quad = 1
    )
  )

  expect_error(plot_tag_status_by_subquadrat(vft_1quad, show_page = "not logical"))

  expect_error(plot_tag_status_by_subquadrat(vft_1quad, show_subquad = "not logical"))

  expect_error(
    plot_tag_status_by_subquadrat(
      vft_1quad,
      # wrong type
      point_shape = c("a", "b")
    )
  )

  expect_error(
    plot_tag_status_by_subquadrat(
      vft_1quad,
      # wrong type
      point_size = "a"
    )
  )

  expect_error(
    plot_tag_status_by_subquadrat(
      vft_1quad,
      # wrong type
      tag_size = "a"
    )
  )

  expect_error(
    plot_tag_status_by_subquadrat(
      vft_1quad,
      # wrong type
      header = 1
    )
  )

  expect_error(
    plot_tag_status_by_subquadrat(
      vft_1quad,
      # wrong type
      theme = 1
    )
  )

  expect_error(
    plot_tag_status_by_subquadrat(
      vft_1quad,
      # wrong type
      move_edge = "a"
    )
  )

  # stops if data has more than one PlotID"
  dup_plotid <-   vft_1quad[1, ]
  dup_plotid$PlotID <- 999L
  w_dup_plotid <- dplyr::bind_rows(dup_plotid, vft_1quad)
  expect_error(x <- plot_tag_status_by_subquadrat(w_dup_plotid), "Remove all but a single plot")

  # warns if data has more than one CensusID"
  dup_cnsid <-   vft_1quad[1, ]
  dup_cnsid$CensusID <- 999L
  w_dup_cnsid <- dplyr::bind_rows(dup_cnsid, vft_1quad)
  expect_warning(x <- plot_tag_status_by_subquadrat(w_dup_cnsid), "Likely")
})

test_that("page labels can be changed", {
  plots <- 1:2
  
  suppressWarnings({
    # Warns: "Likely you want only the last 2 censuses"
    maps <- plot_tag_status_by_subquadrat(vft_4quad,
      bl = "a", br = "b", tr = "c", tl = "d",
      show_page = FALSE
    )[plots]
  })
  
  
  page <- unique(purrr::map_df(maps, "data")$page)
  expect_equal(page, letters[plots])
})

test_that("argument subquad_offset works as expected", {
  x <- plot_tag_status_by_subquadrat(vft_1quad, subquad_offset = -1)
  subquads <- unique(purrr::map_df(x, "data")$subquadrat)
  expect_true("01" %in% subquads)

  x <- plot_tag_status_by_subquadrat(vft_1quad, subquad_offset = -1)
  subquads <- unique(purrr::map_df(x, "data")$subquadrat)
  expect_true("01" %in% subquads)
})

test_that("outputs quadrats in order, even if QuadratName is numeric (#33)", {
  tricky_quad <- c("0100", "0101", "1000")
  expect_nms <- flatten_chr(map(tricky_quad, paste0, "_", 1:4))
  
  # Create some data
  vft_toy <- pick_top(vft_4quad, QuadratName, 3)
  vft_toy <- vft_toy %>% 
    mutate(
      QuadratName = recode(QuadratName,
        "721" = "0100",
        "621" = "0101",
        "622" = "1000"
      )
    )
  
  suppressWarnings({
    # Warns: "Likely you want only the last 2 censuses"
    good <- plot_tag_status_by_subquadrat(vft_toy)
  })
  
  expect_equal(names(good), expect_nms)
  
  not_chr <- expect_warning(
    plot_tag_status_by_subquadrat(mutate(vft_toy, QuadratName = as.numeric(QuadratName)))
  )
  expect_equal(names(not_chr), expect_nms)
})

test_that("warns if option max.print is not high enough", {
  old_options <- options()
  options(max.print = 2)
  expect_warning(
    p <- plot_tag_status_by_subquadrat(vft_1quad)
  )
  options(old_options)
  
  old_options <- options()
  options(max.print = 4)
  expect_silent(
    p <- plot_tag_status_by_subquadrat(vft_1quad)
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
