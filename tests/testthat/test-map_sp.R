# setup -------------------------------------------------------------------

library(dplyr)

census <- forestr::bci12t7mini
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
    map_sp(census = dplyr::rename(census, spp = sp), 
    species = all_species)
  )
  # wrong species
  expect_error(map_sp(census = census, species = 1:3))
})

test_that("outputs a non empty list", {
  result <-  map_sp(census = census, species = all_species)
  expect_type(result, "list")
  expect_silent(
    lapply(result, assertive::is_non_empty) %>%
      unlist() %>%
      all()
  )
})

test_that("output is a named list with names equal to species codes in sp", {
  result <- map_sp(census = census, species = all_species)
  expect_named(result, all_species)
})

test_that("output is an invisible and named list and prints pdf w/ message", {
  expect_message(
    result <- map_sp_pdf(census = census, species = all_species)
  )
  expect_type(result, "list")
  expect_length(result, length(all_species))
  expect_named(result, all_species)
  # Warns that given name is replaced by default
  expect_warning(
    # Message informs name used is default
    expect_message(
      map_sp_pdf(census = census, species = all_species, file = "bad"),
      "Saving as map.pdf"
    )
  )
})

test_that("output is invisible", {
  # There should be a message: "Saving as"
  expect_message(
    # This fails because there is no output -- output is invisible
    expect_error(
      expect_output(map_sp_pdf(census, "hybapr"), ""), "produced no output"
    ),
    "Saving as"
  )

  # invisible if assigned to an object
  expect_error(
    expect_output(x <- map_sp(census, "hybapr"), ""), "produced no output"
  )
})
