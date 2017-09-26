# Create a minimal list of dataframes to use as example data

sinharaja::sinh_q20[[1]] %>% summary()


n <- 300
symbols <- c(16, 15, 0, 1)
quadrats <- as.character(1:2)

df1 <- data.frame(
  tag = as.integer(sample.int(1000, n, replace = FALSE)),
  lx = round(runif(n, 0, 20), 3),
  ly = round(runif(n, 0, 20), 3),
  symbol = as.integer(base::sample(symbols, n, replace = TRUE))
)
df2 <- data.frame(
  tag = as.integer(sample.int(1000, n, replace = FALSE)),
  lx = round(runif(n, 0, 20), 3),
  ly = round(runif(n, 0, 20), 3),
  symbol = as.integer(base::sample(symbols, n, replace = TRUE))
)

toy_list <- setNames(list(df1, df2), quadrats)

use_data(toy_list, overwrite = TRUE)
