library(dplyr)

census <- bciex::bci12t7mini
few_sp <- dplyr::count(census, sp) %>%
  dplyr::arrange(n) %>%
  tail(3) %>%
  dplyr::pull(sp)
census <- census %>% dplyr::filter(sp  %in% few_sp)

# Selecting all species in the example dataset
all_species <- unique(census$sp)



context("map_sp")

test_that("errs with wrong inputs.", {
  # wrong name
  expect_error(
    map_sp(census = dplyr::rename(census, wrong_nm = sp),
    species = all_species)
  )
  # wrong species
  expect_error(map_sp(census = census, species = 1:3))
})

test_that("outputs a non empty list", {
  result <-  map_sp(census = census, species = all_species)
  expect_type(result, "list")
  expect_silent(
    lapply(result, function(x) stopifnot(length(x) != 0)) %>%
      unlist() %>%
      all()
  )
})

test_that("output is a named list with names equal to species codes in sp", {
  result <- map_sp(census = census, species = all_species)
  expect_named(result, sort(all_species))
})

