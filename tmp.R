# quadrat
#


library(try)
library(tidyverse)

list_of_one_quadrat <- sinharaja::sinh_q20[1]

prepdf <- list_of_one_quadrat %>%
  prep_repulsive_tags() %>%
  .[[1]] %>%
  as.tibble()

  plot_repulsive_tags(
    prep_df = prepdf,
    site_name = "Sinharaja 2017",
    point_shape = c(19, 4),
    point_size = 1.5,
    tag_size = 3,
    header = get_header(),
    theme = get_theme()
  )


# list --------------------------------------------------------------------

library(try)
library(tidyverse)

list_of_one_quadrat <- sinharaja::sinh_q20[1]

p <- list_of_one_quadrat %>%
  prep_repulsive_tags() %>%
  lapply_plot_repulsive_tags(site_name = "Sin")

pdf(onefile = TRUE)
p
dev.off()

