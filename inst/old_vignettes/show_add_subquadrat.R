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
  fig.show = "hold"
)

options(dplyr.print_min = 6, dplyr.print_max = 6)

## ------------------------------------------------------------------------
add_subquadrat <- function(df, dim_x, dim_y, divide_x, divide_y) {
  # Simplify nested parentheses
  dim_x_mns.1 <- dim_x - 0.1
  dim_y_mns.1 <- dim_y - 0.1

  # Conditions
  is_odd_both <- df$QX >=  dim_x & df$QY >=  dim_y
  is_odd_x <- df$QX >=  dim_x
  is_odd_y <- df$QY >=  dim_y
  is_not_odd <- TRUE

  # Cases
  with_subquadrat <- dplyr::mutate(df,
    subquadrat = dplyr::case_when(
      is_odd_both ~ paste0(
        (1 + floor((dim_x_mns.1 - dim_x * floor(dim_x_mns.1 / dim_x)) / divide_x)),
        (1 + floor((dim_y_mns.1- dim_y * floor(dim_y_mns.1/ dim_y)) / divide_y))
      ),
      is_odd_x ~ paste0(
        (1 + floor((dim_x_mns.1 - dim_x * floor(dim_x_mns.1 / dim_x)) / divide_x)),
        (1 + floor((df$QY - dim_y * floor(df$QY/ dim_y)) / divide_y))
      ),
      is_odd_y ~ paste0(
        (1 + floor((df$QX - dim_x * floor(df$QX/ dim_x)) / divide_x)),
        (1 + floor((dim_y_mns.1- dim_y * floor(dim_y_mns.1 / dim_y)) / divide_y))
      ),
      is_not_odd ~ paste0(
        (1 + floor((df$QX - dim_x * floor(df$QX/ dim_x)) / divide_x)),
        (1 + floor((df$QY - dim_y * floor(df$QY/ dim_y)) / divide_y))
      )
    )
  )
  with_subquadrat
}

## ------------------------------------------------------------------------
library(tidyverse)

df <- sinharaja::sinh_vftbl_selected
with_subquadrat <- add_subquadrat(
  df, dim_x = 20, dim_y = 20, divide_x = 5, divide_y = 5
)

q15 <- with_subquadrat %>%
  filter(QuadratName == "0015")
odds <- q15 %>%
  filter(QX >= 20 | QY >= 20)

ggplot(data = q15, aes(QX, QY)) +
  geom_hline(yintercept = 20) +
  geom_vline(xintercept = 20) +
  geom_point(data = odds, colour = "red", size = 8) +
  geom_text(data = q15, aes(label = subquadrat))

