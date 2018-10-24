context("fgeo")

elev_ls <- fgeo.data::luquillo_elevation
elev_df <- elev_ls$col

stem <- fgeo.data::luquillo_stem5_random
tree <- fgeo.data::luquillo_tree5_random

test_that("with elevation list/dataframe returns 'elevation_lst/df'", {  
  expect_equal(classify_fgeo_tables(elev_ls), "elevation_lst")
  expect_equal(classify_fgeo_tables(elev_df), "elevation_df")
})

test_that("with a non-census dataframe returns 'unknown'", {  
  expect_equal(classify_fgeo_tables(data.frame(x = 1)), "unknown")
})

test_that("with stem/tree dataframe returns 'stem/tree'", {  
  expect_equal(classify_fgeo_tables(stem), "stem")
  expect_equal(classify_fgeo_tables(tree), "tree")
})

test_that("with a non-ForestGEO-like list returns 'unknown'", {  
  expect_equal(classify_fgeo_tables(list(x = 1)), "unknown")
})

test_that("accepts a list of lengh 1 and 2 but not 0 or 3", {
  expect_error(classify_fgeo_tables(list(1)), NA)
  expect_error(classify_fgeo_tables(list(1, 1)), NA)
  expect_error(classify_fgeo_tables(list(1, 1, 1)), "must be of length 1 or 2")
  expect_error(classify_fgeo_tables(list(0)), "must be of length 1 or 2")
})

