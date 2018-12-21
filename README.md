
<!-- Don't edit README.md; instead, edit README.Rmd -->

# <img src="https://i.imgur.com/vTLlhbp.png" align="right" height=88 /> Plot ForestGEO data

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Build
Status](https://travis-ci.org/forestgeo/fgeo.plot.svg?branch=master)](https://travis-ci.org/forestgeo/fgeo.plot)
[![Coverage
status](https://codecov.io/gh/forestgeo/fgeo.plot/branch/master/graph/badge.svg)](https://codecov.io/github/forestgeo/fgeo.plot?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/fgeo.plot)](https://cran.r-project.org/package=fgeo.plot)

## Installation

Install the development version of **fgeo.plot**:

    # install.packages("devtools")
    devtools::install_github("forestgeo/fgeo.plot")

Or [install all **fgeo** packages in one
step](https://forestgeo.github.io/fgeo/index.html#installation).

For details on how to install packages from GitHub, see [this
article](https://goo.gl/dQKEeg).

## Example

``` r
library(fgeo.plot)

small_census <- fgeo.x::tree6_3species
autoplot(sp(small_census))
```

<img src="man/figures/README-fgeo.plot-sp-1.png" width="75%" style="display: block; margin: auto;" />

``` r
elevation <- fgeo.x::elevation
autoplot(
  sp_elev(small_census, elevation),
  fill = "red", 
  hide_color_legend = TRUE
)
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="75%" style="display: block; margin: auto;" />

[Get started with
**fgeo**](https://forestgeo.github.io/fgeo/articles/fgeo.html)

## Information

  - [Getting help](SUPPORT.md).
  - [Contributing](CONTRIBUTING.md).
  - [Contributor Code of Conduct](CODE_OF_CONDUCT.md).
