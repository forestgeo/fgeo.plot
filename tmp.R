load_all()
library(tidyverse)

# Ngel Nyaki
ngel_quads <- unique(ngelnyaki::ngelnyaki_vft_unid$QuadratName)[1:2]
ngel <- ngelnyaki::ngelnyaki_vft_unid %>% filter(QuadratName %in% ngel_quads)
n <- map_tag(ngel, site_name = "Ngel Nyaki 2017")
n[[1]]

pdf("Nge_Nyaki.pdf", paper = "letter", width = 8, height = 11)
n
dev.off()


# Yosemite
yose <- as_tibble(yosemite::ViewFullTable_yosemite)
yose_quads <- unique(yose$QuadratName)[1:2]
yose_to_map <- yose %>% filter(QuadratName %in% yose_quads)

p <- map_tag(yose_to_map, site_name = "Yosemite 2017")
pdf("Yose.pdf", paper = "letter", width = 8, height = 11)
p
dev.off()
