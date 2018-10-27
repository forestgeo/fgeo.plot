
<!-- Don't edit README.md; instead, edit README.Rmd -->

# <img src="https://i.imgur.com/vTLlhbp.png" align="right" height=88 /> Plot ForestGEO-like datasets

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Build
Status](https://travis-ci.org/forestgeo/fgeo.map.svg?branch=master)](https://travis-ci.org/forestgeo/fgeo.map)
[![Coverage
status](https://codecov.io/gh/forestgeo/fgeo.map/branch/master/graph/badge.svg)](https://codecov.io/github/forestgeo/fgeo.map?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/fgeo.map)](https://cran.r-project.org/package=fgeo.map)

## Installation

Install the pre-release version of **fgeo.map**:

    # install.packages("devtools")
    devtools::install_github("forestgeo/fgeo.map@pre-release")

Or install the development version of **fgeo.map**:

    # install.packages("devtools")
    devtools::install_github("forestgeo/fgeo.map")

Or [install all **fgeo** packages in one
step](https://forestgeo.github.io/fgeo/index.html#installation).

For details on how to install packages from GitHub, see [this
article](https://goo.gl/dQKEeg).

## Example

``` r
library(fgeo.map)

elevation <- fgeo.data::luquillo_elevation

# Small dataset with a few species for quick examples
selected_species <- c("PREMON", "CASARB")
census <- subset(fgeo.data::luquillo_tree5_random, sp %in% selected_species)
```

``` r
autoplot(sp(census))
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="100%" style="display: block; margin: auto;" />

``` r
autoplot(sp(census), point_size = 2, shape = 21, fill = "darkgreen")
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" style="display: block; margin: auto;" />

``` r
autoplot(elev(elevation))
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" style="display: block; margin: auto;" />

``` r
autoplot(sp_elev(census, elevation))
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" style="display: block; margin: auto;" />

``` r
# Customize
autoplot(sp_elev(census, elevation), fill = "red", hide_color_legend = TRUE)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" style="display: block; margin: auto;" />

[Get started with
**fgeo**](https://forestgeo.github.io/fgeo/articles/fgeo.html)

## Information

  - [Getting help](SUPPORT.md).
  - [Contributing](CONTRIBUTING.md).
  - [Contributor Code of Conduct](CODE_OF_CONDUCT.md).

## Acknowledgments

For ideas and guidance, thanks to Suzanne Lao, Stuart Davis, Shameema
Jafferjee Esufali, David Kenfack and Anudeep Singh.
