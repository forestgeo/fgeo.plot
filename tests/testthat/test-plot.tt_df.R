context("test-plot")

describe("outputs a ggplot", {
  skip_if_not_installed("fgeo.tool")
  skip_if_not_installed("fgeo.habitat")
  
  library(fgeo.tool)
  library(fgeo.habitat)
  
  census <- filter(luquillo_top3_sp, status == "A", dbh >= 10)
  habitat <- luquillo_habitat
  species <- c("CASARB", "PREMON", "SLOBER")
  
  tt_df <- to_df(tt_test(census, species, habitat))
  p <- plot(tt_df)

  it("outputs a ggplot", {
    expect_is(p, "ggplot")
  })
})

