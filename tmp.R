# grep --------------------------------------------------------------------

grep -R "library(fgeo.map)" 
../fgeo
../fgeo.abundance
../fgeo.base
../fgeo.data
../fgeo.demography
../fgeo.habitat
../fgeo.map
../fgeo.tool

# /R
grep -R "maply_tag" ../fgeo/R ../fgeo.abundance/R ../fgeo.base/R ../fgeo.data/R ../fgeo.demography/R ../fgeo.habitat/R ../fgeo.tool/R 

# README
grep -R "maply_tag" ../fgeo/README.Rmd ../fgeo.abundance/README.Rmd ../fgeo.base/README.Rmd ../fgeo.data/README.Rmd ../fgeo.demography/README.Rmd ../fgeo.habitat/README.Rmd ../fgeo.tool/README.Rmd 

# /vignettes
grep -R "maply_tag" ../fgeo/vignettes ../fgeo.abundance/vignettes ../fgeo.base/vignettes ../fgeo.data/vignettes ../fgeo.demography/vignettes ../fgeo.habitat/vignettes ../fgeo.tool/vignettes

# new names ---------------------------------------------------------------

# Create a list of plots (optimized for printing)

* plot_tag_status_by_subquadrat() <- maply_tag
* plot_dbh_bubles_by_quadrat() <- maply_quad
* plot_each_species() <- maply_sp_elev

# Create a single plot (may have multiple panels)

# TODO: Use S3 to detect if data is census or elevation? Maybe first argument
# can be either census, or elevation, or a list of both.

# This may be simplified to a single function that automatically detects which
# of the two the user provided
* plot_species() <- maply_sp_elev  # Clarify that also plots elevation
* plot_elevation <- map_elev

# Create/modify plot layers

# This may be simplified to a single function that detects which of the two 
# the user provided.
plot_base_census <- map_gx_gy()
plot_base_elevation <- map_gx_gy_elev()

add_elevation_contrours <- contour_elev()
add_elevation_labels <- label_elev()
add_species <- add_sp()

set_facet <- wrap()
set_limits <- limit_gx_gy()

hide_axis_labels() <- hide_axis_labels()
hide_color_legend() <- hide_legend_color()

# Helpers

header_tag_status <- map_tag_header()
header_dbh_bubles <- map_quad_header()
suffix_edgy_tags <- suffix_edge_tag()
theme_dbh_bubles <- theme_map_tag()

# Appveyor badge ----------------------------------------------------------

[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/forestgeo/fgeo.map?branch=master&svg=true)](https://ci.appveyor.com/project/forestgeo/fgeo.map)

