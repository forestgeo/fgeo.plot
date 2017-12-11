library(dplyr)
library(map)


vft <- bci12vft_mini
one_quadrat <-  unique(vft$QuadratName)[[1]]

# Filter rows
quad <- filter(vft, PlotID == 1, QuadratName == one_quadrat)
# Select columns
crucial_vars <- c(
  "tag", "x", "y", "status", "quadratname", "censusid", "plotid"
)
quad <- quad[, tolower(names(quad)) %in% crucial_vars]
# Remove duplicates
quad <- unique(quad)


map_one_quadrat <- function(vft,
                            x,
                            y,
                            shape,
                            label,
                            site_name = "My site, YYYY",
                            header = get_header(),
                            theme = get_theme()) {
  ggplot(data = vtf, aes_string(x = x, y = y, shape = shape)) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label = label)) +
    labs(
      title = paste0(site_name, ". ", one_quadrat),
      subtitle = header,
      x = NULL, y = NULL
    ) +
    coord_fixed() +
    theme
}
