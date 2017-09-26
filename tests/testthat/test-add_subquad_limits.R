context("add_subquad_limits")

# Just a shortcut to try different `qudrat_size`s (qs)
add_and_extract_lims <- function(quad_size, ...) {

with_subquad_df <- sinharaja::sinh_q20[1] %>%
 add_quadrat_and_subquadrat_from_list() %>%
  .[[1]] %>%
  .[[1]] %>%
  # The only "must be" is the variable `subquadrat`; we could remove `quadrat`
  select(-quadrat) %>%
  add_subquad_limits(quad_size = quad_size, ...)

with_subquad_df %>%
  filter(subquadrat == 1) %>%
  mutate(lims = paste(x1, x2, y1, y2)) %>%
  pull(lims) %>%
  unique()
}



test_that("multiplication works", {
  expect_equal(add_and_extract_lims(20, shrink = 0.4), "0.4 9.6 0.4 9.6")
  expect_equal(add_and_extract_lims(10, shrink = 0.5), "0.5 4.5 0.5 4.5")
})
