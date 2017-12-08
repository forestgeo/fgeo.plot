library(vetr)
library(dplyr)
library(purrr)
library(gridExtra)

# Minimal data

# Fix some odd variables of this particular data set
# See email by Suzanne Lao https://goo.gl/UwiRbj
minivft <- dplyr::rename(map::bci12vft_mini, QX = x, QY = y) %>%
  tibble::rowid_to_column("DBHID")

few_quads <- unique(minivft$QuadratName)[1]
vft <- minivft %>% filter(QuadratName %in% few_quads)

context("test-map_tag.R")

# Redo once the plot is stable
test_that("outputs same as satisfactory reference", {
  plots <- suppressWarnings(map_tag(vft))
  expect_equal_to_reference(plots, "ref-map_tag.rds")
})


test_that("map_tag() stops if data has more than one PlotID", {
  new_row <-   vft[1, ]
  new_row$PlotID <- 2L
  vft_with_new_row <- bind_rows(new_row, vft)
  expect_error(map_tag(vft_with_new_row))
})




test_that("wrong inputs passed to lapply_plot_repulsive_tags() get noticed", {

  expect_error(
    suppressWarnings(
      map_tag(
       vft,
       # next line is wrong input
       site_name = 1,
       point_shape = c(0, 2),
       point_size = 6,
       tag_size = 10
      )
    )
  )
  expect_error(
    suppressWarnings(
      map_tag(
        vft,
        site_name = "my site",
        # next line is wrong input
        point_shape = c("a", "b"),
        point_size = 6,
        tag_size = 10
      )
    )
  )
  expect_error(
    suppressWarnings(
      map_tag(
        vft,
        site_name = "my site",
        point_shape = c(1, 2),
        # next line is wrong input
        point_size = "a",
        tag_size = 10
      )
    )
  )
  expect_error(
    suppressWarnings(
      map_tag(
        vft,
        site_name = "my site",
        point_shape = c(1, 2),
        point_size = 1,
        # next line is wrong input
        tag_size = "a"
      )
    )
  )
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
  all <- suppressWarnings(map_tag(vft))
  vft_with_min_vars <- vft %>%
    select(Tag, Status, QX, QY, QuadratName, DBHID, CensusID, PlotID)
  min <- suppressWarnings(map_tag(vft_with_min_vars))

  expect_equal(all, min)

  # # Visual confirmation
  all_multipaged <- marrangeGrob(all, nrow = 1, ncol = 1)
  ggplot2::ggsave("all_multipaged.pdf", all_multipaged,
    paper = "letter", width = 8, height = 10.5
  )
  min_multipaged <- marrangeGrob(min, nrow = 1, ncol = 1)
  ggplot2::ggsave(
    "min_multipaged.pdf", min_multipaged,
    paper = "letter", width = 8, height = 10.5
  )
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
  result <- suppressWarnings(map_tag(vft))
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

# reusing
vft2 <- lower_names_then_check(vft, nms = c("tag", "qx", "qy", "status"))
with_sq <- add_subquadrat(df = vft2, 20, 20, 5, 5)

test_that("outputs a dataframe with new expected variable", {
  expect_named(with_sq, c(names(vft2), "subquadrat"))
  expect_is(with_sq, "data.frame")
})



context("test-check_subquadrat_dimensions")





test_that("throws error with wrong inputs to add_subquadrat", {
  df <- minivft[1:5, c("QX", "QY")]
  # check that works
  expect_message(add_subquadrat(df, 20, 20, 5, 5))
  expect_message(add_subquadrat(df, 40, 50, 5, 5))

  # Fails
  expect_error(suppressMessages(add_subquadrat(1, 20, 20, 5, 5)))
  expect_error(suppressMessages(add_subquadrat(df, "a", 20, 5, 5)))
  expect_error(suppressMessages(add_subquadrat(df, 20, c(20, 20), 5, 5)))
  expect_error(suppressMessages(add_subquadrat(df, 20, 20, "a", 5)))
  expect_error(suppressMessages(add_subquadrat(df, 20, 20, 5, c(5, 5))))
  expect_error(suppressMessages(add_subquadrat(df, -1, 20, 5, 5)))
  expect_error(suppressMessages(add_subquadrat(df, 20, Inf, 5, 5)))
})



context("test-add_status_tree")

# reusing
with_status_tree <- add_status_tree(with_sq)

test_that("outputs a dataframe with new expected variable", {
  expect_true(vet("status_tree" %in% ., names(with_status_tree)))
  expect_is(with_status_tree, "data.frame")
})



context("test-lapply_plot_repulsive_tags")

prep_df <- unique(
  add_status_tree_page_x1_x2_y1_y2_split_quad_id(
    with_status_tree, quad_size = 20, extend_grid = 0.45
  )
)
prep_df_list <- split(prep_df, prep_df$split)

test_that("outputs a ggplot", {
  plot_list <- prep_df_list %>%
      lapply_plot_repulsive_tags(
        site_name = "my site",
        x_q = 20, x_sq = 5
      )
  expect_true(
    any(
      grepl("ggplot", class(plot_list[[1]]))
    )
  )
})



context("test-add_status_tree")

test_that("the tree status is dead only if one stem is dead", {
  one_dead <- tibble(
    tag = c(
      1, 1,
      2, 2,
      3, 3
    ),
    status = c(
      "alive", "dead",
      "dead", "dead",
      "broken below", "missing"
    )
  )
  expected <- c(rep("alive", 2), rep("dead", 2), rep("alive", 2))
  expect_equal(add_status_tree(one_dead)$status_tree, expected)
})


add_status_tree <- function(df) {
  grouped <- dplyr::group_by(df, .data$tag)
  mutated_grouped <- dplyr::mutate(
    grouped,
    status_tree = ifelse(all(.data$status == "dead"), "dead", "alive")
  )
  dplyr::ungroup(mutated_grouped)
}
