context("test-plot-fgeo_habitat")

describe("outputs an object of class ggplot", {
  skip_if_not_installed("fgeo")
  library(fgeo)

  elev_list <- fgeo.x::elevation
  habitats <- fgeo_habitat(elev_list, gridsize = 20, n = 4)
  p <- autoplot(habitats)

  it("does something I want it to do", {
    expect_is(p, "ggplot")
  })
})
