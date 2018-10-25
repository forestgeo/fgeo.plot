vft_1quad_sub <- dplyr::sample_n(vft_1quad, 5)


context("plot_dbh_bubbles_by_quadrat")

test_that("passes with default arguments", {
  # Warns because dataset has no dead trees
  expect_message(
    plot_dbh_bubbles_by_quadrat(
      vft_1quad_sub,
      title_quad = "Site Name, YYYY, Quadrat:",
      header = header_dbh_bubbles(),
      theme = theme_dbh_bubbles(),
      lim_min = 0,
      lim_max = 20,
      subquadrat_side = 5,
      tag_size = 2,
      move_edge = 0
    )
  )
})

test_that("wrong inputs to are rejected", {
  expect_error(plot_dbh_bubbles_by_quadrat())
  expect_error(plot_dbh_bubbles_by_quadrat(1))
  expect_error(plot_dbh_bubbles_by_quadrat(vft_1quad_sub, lim_min = "a"))
  expect_error(plot_dbh_bubbles_by_quadrat(vft_1quad_sub, lim_max = "a"))
  expect_error(plot_dbh_bubbles_by_quadrat(vft_1quad_sub, subquadrat_side = "a"))
  expect_error(plot_dbh_bubbles_by_quadrat(vft_1quad_sub, tag_size = "a"))
  expect_error(plot_dbh_bubbles_by_quadrat(vft_1quad_sub, move_edge = "a"))
  expect_error(plot_dbh_bubbles_by_quadrat(vft_1quad_sub, header = 1))
  expect_error(plot_dbh_bubbles_by_quadrat(vft_1quad_sub, title_quad = 1))
  expect_error(plot_dbh_bubbles_by_quadrat(vft_1quad_sub, theme = 1))
})

test_that("returns a list of ggplots", {
  # Warns because dataset has no dead trees
  one_map <- plot_dbh_bubbles_by_quadrat(vft_1quad_sub)
  expect_type(one_map, "list")
  expect_s3_class(one_map[[1]], "ggplot")
})



context("theme_dbh_bubbles")

test_that("returns a valid ggplot2 theme", {
  expect_s3_class(theme_dbh_bubbles(), c("theme"))
  expect_s3_class(theme_dbh_bubbles(), "gg")
})

