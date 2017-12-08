## ----setup, echo = FALSE, message=FALSE, warning=FALSE-------------------
# hadley's settings
set.seed(1014)
options(digits = 3)

knitr::opts_chunk$set(
  echo = TRUE,
  comment = "#>",
  collapse = TRUE,
  cache = TRUE,
  out.width = "70%",
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618,  # 1 / phi
  fig.show = "hold"
)

library(ggplot2)
update_geom_defaults("text", list(size = 14.5))

## ------------------------------------------------------------------------
# To install from a private repo, generate a personal access token (PAT) in
# https://github.com/settings/tokens and supply to this argument.
GITHUB_PAT <- "your token"
# install_github("forestgeo/forestr", auth_token = GITHUB_PAT)
library(forestr)

# Also using the package dplyr for easier data manipulation
# install.packages(dplyr)
library(dplyr)
# Print only a few rows of tibbles (modern dataframes) to save space
options(dplyr.print_min = 6, dplyr.print_max = 6)

## ------------------------------------------------------------------------
# Converting dataframe to tibble for better printing
example_data <- as_tibble(forestr::bci12t7mini)

# Let's find the top-3 most abundant species
sp_n <- count(example_data, sp)
sp_arranged <- arrange(sp_n, desc(n))
sp_top3 <- sp_arranged$sp[1:3]
sp_top3

# For a small example, keeping data of only the 3 most abundant species
census <- filter(example_data, sp  %in% sp_top3)
census

## ------------------------------------------------------------------------
# Print to screen
map_sp(census, c("hybapr", "faraoc"))

## ------------------------------------------------------------------------
# Defaults to save in working directory. Returns invisibly.
map_sp_pdf(census, c("hybapr", "faraoc"))

## ----fig.align="default", out.width="45%", fig.widh=(6 * 0.45 / 0.7)-----
# Force printing the invisible output
print(map_sp_pdf(census, c("hybapr", "faraoc")))

# (Printing side by side to save space)

## ---- error=TRUE---------------------------------------------------------
# Show behaviour with wrong name
with_wrong_name <- rename(census, SP = sp)

# This will fail because the data set has name `SP`, not `sp`
map_sp(with_wrong_name, c("hybapr", "faraoc"))

## ------------------------------------------------------------------------
my_elev <- forestr::bci_elevation
my_elev

## ---- error=TRUE---------------------------------------------------------
# This will fail because the names of `my_elev` are not correct
map_sp(census, c("hybapr", "faraoc"), elevation = my_elev)

## ------------------------------------------------------------------------
# Renaming data and trying again
elev <- rename(my_elev, gx = x, gy = y)
map_sp(census, c("hybapr", "faraoc"), elevation = elev)

## ------------------------------------------------------------------------
# Fake over abundant species
crowded <- dplyr::tibble(
  sp = sample(c("species1"), 10000, replace = TRUE),
  gx = sample.int(1000, 10000, replace = TRUE),
  gy = sample.int(500, 10000, replace = TRUE)
)
map_sp(crowded, c("species1"))

## ----fig.align="default", out.width="45%", fig.widh=(6 * 0.45 / 0.7)-----
# smaller and semi transparent
map_sp(crowded, c("species1"), size = 1, alpha = 2/10)

# Smaller and open
map_sp(crowded, c("species1"), size = 1, shape = 21)

# (Printing side by side to save space)

## ------------------------------------------------------------------------
# A random sample of 2000 stems
smaller <- sample_n(crowded, 2000)
map_sp(smaller, c("species1"), shape = 21)

## ------------------------------------------------------------------------
map_sp(census, "hybapr",
  # Arguments passed to ggplot2::geom_point()
  size = 4, shape = 22, fill = "green", colour = "black", stroke = 2)

## ------------------------------------------------------------------------
map_sp(census, "hybapr", 
  elevation = elev, line_size = 1, low = "red", high = "blue", bins = 4)

## ---- fig.align="default", out.width="45%", fig.widh=(6 * 0.45 / 0.7)----
map_sp(census, "hybapr", theme = ggplot2::theme_classic())
map_sp(census, "hybapr", theme = ggplot2::theme_dark())

# (Printing side by side to save space)

## ---- error=TRUE---------------------------------------------------------
spp <- c("hybapr", "faraoc")

map_sp_pdf(census, spp, elevation = elev, file = "bad-name1")

map_sp_pdf(census, spp, elevation = elev, file = "bad-name2.png")


## ------------------------------------------------------------------------
map_sp_pdf(census, spp, elevation = elev, file = "good-name.pdf")

# Same without any message
suppressMessages(
  map_sp_pdf(census, spp, elevation = elev, file = "good-name.pdf")
)

## ---- fig.align="default", out.width="45%", fig.widh=(6 * 0.45 / 0.7)----
# Fake case: The maximum-limits of the site's plot are gx = 1000 and gy = 500;
# but all stems in the census occurr at lower gx and gy limits
odd_data <- filter(census, gx < 800, gy < 200)
two_species <- unique(odd_data$sp)[1:2]
map_sp(odd_data, two_species)

# (Printing side by side to save space)

## ---- fig.align="default", out.width="45%", fig.widh=(6 * 0.45 / 0.7)----
map_sp(odd_data, two_species, xlim = c(0, 1000), ylim = c(0, 500))

# (Printing side by side to save space)

## ------------------------------------------------------------------------
library(gridExtra)

all_species <- unique(census$sp)
maps <- map_sp(census, all_species)
multipaged <- marrangeGrob(maps, nrow = 1, ncol = 2)
multipaged

## ------------------------------------------------------------------------
# Saving to .pdf: Option 1
ggplot2::ggsave("multi-paged.pdf", multipaged)

# Saving to .pdf: Option 2
pdf()
multipaged
dev.off()

## ------------------------------------------------------------------------
# Saving to .png; this will create multiple files.
png()
multipaged
dev.off()

## ------------------------------------------------------------------------
# Showing only a few options; see all the options with ?ggplot2::theme()
my_theme <- ggplot2::theme(
  text = element_text(size = 25, face = "bold.italic", colour = "white"),
  plot.background = element_rect(fill = "black"),
  plot.margin = margin(2, 2, 2, 2, "cm"),
  strip.background = element_rect(fill = "darkgreen"),
  strip.text = element_text(colour = "white"),
  # make grid to dissapear by matching background colour
  panel.background = element_rect(fill = "lightgreen"),
  panel.grid.minor = element_line(colour = "lightgreen"),
  panel.grid.major = element_line(colour = "lightgreen")
)
map_sp(census, "hybapr", theme = my_theme)

## ------------------------------------------------------------------------
p0 <- map_sp(census, c("hybapr", "faraoc"))

# Adding a new layer to one element of the plots' list
p0[["hybapr"]] + geom_vline(aes(xintercept = 300), colour = "red")

## ----fig.align="default", out.width="45%", fig.widh=(6 * 0.45 / 0.7)-----
p1 <- lapply(p0, `+`, geom_vline(aes(xintercept = 300), colour = "red"))
p1

p2 <- lapply(p1, `+`, geom_hline(aes(yintercept = 400), colour = "blue"))
p2

# (Printing side by side to save space)

