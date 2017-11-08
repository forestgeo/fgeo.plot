library(vetr)
library(dplyr)
library(purrr)
library(gridExtra)

# Minimal data
few_quads <- unique(ngelnyaki::ngelnyaki_vft_unid$QuadratName)[1]
vft <- ngelnyaki::ngelnyaki_vft_unid %>% filter(QuadratName %in% few_quads)



context("test-map_tag.R")

## Redo once the plot is stable
# test_that("outputs same as satisfactory reference", {
#   plots <- map_tag(vft)
#   expect_equal_to_reference(plots, "ref-map_tag.rds")
#
#   expect_equal_to_reference(plots, "ref-map_tag.rds")
#
# })



test_that("wrong inputs passed to lapply_plot_repulsive_tags() get noticed", {

  expect_error(
    map_tag(
      vft,
      # next line is wrong input
      site_name = 1,
      point_shape = c(0, 2),
      point_size = 6,
      tag_size = 10
    )
  )
  expect_error(
    map_tag(
      vft,
      site_name = "my site",
      # next line is wrong input
      point_shape = c("a", "b"),
      point_size = 6,
      tag_size = 10
    )
  )
  expect_error(
    map_tag(
      vft,
      site_name = "my site",
      point_shape = c(1, 2),
      # next line is wrong input
      point_size = "a",
      tag_size = 10
    )
  )
  expect_error(
    map_tag(
      vft,
      site_name = "my site",
      point_shape = c(1, 2),
      point_size = 1,
      # next line is wrong input
      tag_size = "a"
    )
  )
})



test_that("plots all unique tags in data", {
  plots <- map_tag(vft)
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
  all <- map_tag(vft)
  vft_with_min_vars <- vft %>% select(Tag, Status, QX, QY, QuadratName, DBHID)
  min <- map_tag(vft_with_min_vars)

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

# reusing
vft2 <- lower_names_then_check(vft, nms = c("tag", "qx", "qy", "status"))
with_sq <- add_subquadrat(df = vft2, 20, 20, 5, 5)

test_that("outputs a dataframe with new expected variable", {
  expect_named(with_sq, c(names(vft2), "subquadrat"))
  expect_is(with_sq, "data.frame")
})



context("test-rename_with_subquadrat")

# reusing
rnm <- rename_with_subquadrat(with_sq)

test_that("outputs a dataframe with new expected variables", {
  expect_true(
    vet(c("subquadrat_vftbl", "quadrat_vftbl", "qx", "qy") %in% ., names(rnm))
  )

  expect_is(rnm, "data.frame")
})


context("test-add_status_tree")

# reusing
with_status_tree <- add_status_tree(rnm)

test_that("outputs a dataframe with new expected variable", {
  expect_true(vet("status_tree" %in% ., names(with_status_tree)))
  expect_is(with_status_tree, "data.frame")
})



context("test-add_symbol")

# reusing
with_symbol <- add_symbol(with_status_tree)

test_that("outputs a dataframe with new expected variable", {
  expect_true(vet("symbol" %in% ., names(with_symbol)))
  expect_is(with_symbol, "data.frame")
})



context("test-prep_repulsive_tags")

# reusing
prep_df <- prep_repulsive_tags(with_symbol)

test_that("outputs a dataframe with new expected variable", {
  expect_true(
    vet(
      c("id", "status_tree", "x1", "x2", "y1", "y2") %in% .,
      names(prep_df)
    )
  )
  expect_is(prep_df, "data.frame")
})



context("test-discard_duplicated_tags_and_useless_vars")

# reusing
unique_tags <- discard_duplicated_tags_and_useless_vars(prep_df)
unique_tags_split <- split(unique_tags, unique_tags$split)

one_df_with_unique_tags <- unique_tags_split[[1]]
test_that("outputs a dataframe with only the expected vars and unique tags", {
  expect_length(names(one_df_with_unique_tags), 12)
  expect_is(one_df_with_unique_tags, "data.frame")
  expect_equal(
    length(one_df_with_unique_tags$tag),
    length(unique(one_df_with_unique_tags$tag))
  )
})



context("test-lapply_plot_repulsive_tags")

test_that("outputs a ggplot", {
  plot_list <- unique_tags_split %>%
      lapply_plot_repulsive_tags(site_name = "my site")
  expect_true(
    any(
      grepl("ggplot", class(plot_list[[1]]))
    )
  )
})




