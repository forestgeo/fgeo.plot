library(dplyr)

vft <- ngelnyaki::ngelnyaki_vft_unid

first_two <- unique(vft$QuadratName)[1:5]
vftsub <- vft %>%
  filter(QuadratName %in% first_two)

x <- suppressMessages(
  map_tag(vftsub, 20, 5, site_name = "my site 2017")
)

pdf()
x
dev.off()
