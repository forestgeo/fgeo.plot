# Create a minimal list of dataframes to use as example data

n_df <- 300
symbols <- c(16, 15, 0, 1)
quadrats <- as.character(1:2)

df1 <- data.frame(
  tag = quadrats[1],
  lx = runif(n, 0, 20),
  ly = runif(n, 0, 20),
  symbol = base::sample(symbols, n, replace = TRUE)
)
df2 <- data.frame(
  tag = quadrats[2],
  lx = runif(n, 0, 20),
  ly = runif(n, 0, 20),
  symbol = base::sample(symbols, n, replace = TRUE)
)

toy_list <- setNames(list(df1, df2), quadrats)

use_data(toy_list)
