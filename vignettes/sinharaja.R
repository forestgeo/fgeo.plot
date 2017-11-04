## ----setup, echo = FALSE, message=FALSE, warning=FALSE-------------------
# hadley's settings
set.seed(1014)
options(digits = 3)

knitr::opts_chunk$set(
  echo = TRUE,  # {mine}
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  out.width = "70%",
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618,  # 1 / phi
  fig.show = "hold",
  rows.print = 3  # {mine}
)

options(dplyr.print_min = 3, dplyr.print_max = 3)

## ------------------------------------------------------------------------
library(tidyverse)
library(try)

## ------------------------------------------------------------------------
vftbl <- sinharaja::sinh_vftbl_selected
vftbl

## ------------------------------------------------------------------------
nrow(vftbl)

## ------------------------------------------------------------------------
renamed <- vftbl %>% 
  select(-id, -StemTag, -Status) %>% 
  rename(
    tag = Tag,
    subquadrat_vftbl = Subquadrat,
    quadrat_vftbl = QuadratName,
    lx = QX,
    ly = QY
  ) %>% 
  unique()
renamed

## ---- message=FALSE------------------------------------------------------
sham_df <- sinharaja::sinh_q20 %>% 
  reduce(full_join) %>% 
  as_tibble()
# allow merging by tag
sham_df <- sham_df %>% 
  mutate(
    status = as.character(status),
    tag = str_pad(tag, width = 6, side = "left", pad = "0")
  ) %>% 
  select(-lx, -ly, -subtag) %>% 
  unique()
sham_df

## ------------------------------------------------------------------------
sinh_df <- suppressMessages(left_join(renamed, sham_df))
sinh_df

## ------------------------------------------------------------------------
splitted <- sinh_df %>% split(.$quadrat_vftbl)
# example
splitted[["0015"]]

## ------------------------------------------------------------------------
problematic <- splitted %>% 
  map(as.data.frame) %>% 
  map(filter, lx >= 20 | ly >= 20) %>% 
  map(arrange, tag) %>% 
  map(select, tag, subquadrat_vftbl)
# e.g.
problematic[["0015"]]

## ---- message=FALSE------------------------------------------------------
prep_list <- splitted %>% prep_repulsive_tags()
plot_list <- prep_list %>% 
  lapply_plot_repulsive_tags(site_name = "Sinharaja 2017")

# # Avoid running unintentionally; it takes a few minutes to run
# pdf(onefile = TRUE, paper = "a4", width = 11, height = 11)
# plot_list
# dev.off()

## ---- message=FALSE------------------------------------------------------
df <- splitted %>% reduce(full_join)
extreeme_quadrats <- df %>% 
  filter(lx == max(lx) | ly == max(ly)) %>%
  pull(quadrat_vftbl)
extreeme_quadrats

## ---- message=FALSE------------------------------------------------------
# dataframe and filter easyly by quadrat
prep_df <- prep_list %>% reduce(full_join)
prep_df_extreeme <- prep_df %>% 
  filter(quadrat %in% extreeme_quadrats)

# Split by id
prep_list_extreeme <- split(prep_df_extreeme, prep_df_extreeme$id)
plot_list_extreeme <- lapply_plot_repulsive_tags(
  prep_list_extreeme, site_name = "Sinharaja 2017"
)

pdf("extreeme.pdf", paper = "a4", width = 11, height = 11)
plot_list_extreeme
dev.off()

