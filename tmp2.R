load_all()
library(dplyr)

# Ngel Nyaki
ngel_quads <- unique(ngelnyaki::ngelnyaki_vft_unid$QuadratName)[2]
ngel <- ngelnyaki::ngelnyaki_vft_unid %>% filter(QuadratName %in% ngel_quads)

n <- map_tag(
  ngel,
  site_name = "Ngel Nyaki 2017"
)
n

# next xxxcont. here
# in map_tag() filter data by greatest censusid
# redefine possible states of a tag as alive or other.
# remove labels indicating subquadrat



b <- tibble::as.tibble(bci12vft7mini)
quads <- unique(b$QuadratName)[1]
bb <- b %>% filter(QuadratName  %in% quads)

# debugonce(plot_repulsive_tags)
map_tag(bb, site_name = "Sinharaja 2017")

bb %>% names()




s <- tibble::as.tibble(sinharaja::sinh_vft)
quads <- unique(s$QuadratName)[1]
ss <- s %>% filter(QuadratName  %in% quads)

# debugonce(plot_repulsive_tags)
map_tag(ss, site_name = "Sinharaja 2017")


y <- tibble::as.tibble(yosemite::ViewFullTable_yosemite)
quads <- unique(y$QuadratName)[1]
yy <- y %>% filter(QuadratName  %in% quads)

# debugonce(plot_repulsive_tags)
map_tag(yy, site_name = "Yosemite 2017")





# # Datasets
# yosemite::ViewFullTable_yosemite %>%
#   as_tibble() %>%
#   select(matches("census"), matches("date"))
#
# sinharaja::sinh_vft %>%
#   as_tibble() %>%
#   select(matches("census"), matches("date"))
#
# ngelnyaki::ngelnyaki_vft_unid %>%
#   as_tibble() %>%
#   select(matches("census"), matches("date"))
