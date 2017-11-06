load_all()
library(tidyverse)

# Minimal data
few_quads <- unique(ngelnyaki::ngelnyaki_vft_unid$QuadratName)[1:2]
vft <- ngelnyaki::ngelnyaki_vft_unid %>% filter(QuadratName %in% few_quads)



plots[["Q. 0101 SQ. 23 (p. 23)"]][["data"]][["tag"]]



library("vetr")

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





