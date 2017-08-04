context("lapply_plot_sp")

# Minimal data to show
cns_data <- bci::bci12full7
# Filtering only 3 species for a minimal example.
(species_selected <- unique(cns_data$sp)[1:3])
cns_data_sub <- dplyr::filter(cns_data, sp %in% species_selected)
# Sampling only 1000 rows for a quick example
cns_data_sub <- dplyr::sample_n(cns_data_sub, 1000)

# Selecting all species in the example dataset
all_species <- unique(cns_data_sub$sp)



test_that("errs with wrong inputs.", {
  # wrong species
  expect_error(lapply_plot_sp(species = 1:3, cns_data = cns_data_sub))
  expect_error(lapply_plot_sp(
    species = all_species,
    # wrong name
    cns_data = dplyr::rename(cns_data_sub, spp = sp)
    )
  )
})

test_that("outputs a non empty list", {
  result <- lapply_plot_sp(species = all_species, cns_data = cns_data_sub)
  expect_type(result, "list")
  expect_silent(
    lapply(result, assertive::is_non_empty) %>%
      unlist() %>%
      all()
  )
})

