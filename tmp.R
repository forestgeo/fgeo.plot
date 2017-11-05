load_all()
library("vetr")
library(dplyr)

vft <- ngelnyaki::ngelnyaki_vft_unid

first_two <- unique(vft$QuadratName)[15]
vftsub <- vft %>%
  filter(QuadratName %in% first_two)

x <- map_tag(vftsub, 20, 5, site_name = "my site 2017")
x




class(x)
class(x[[1]])

y <- suppressMessages(
  map_tag(vftsub, 20, 5, site_name = "my site 2017")
)
vet(x, y)


debugonce(add_subquad_limits)

map_tag(vftsub, 20, 5, site_name = "my site 2017")


# remove
# add_subquadrat.R

#
# pdf()
# x
# dev.off()
