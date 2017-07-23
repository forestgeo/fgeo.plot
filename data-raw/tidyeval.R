# practice tidy evaluation

library(tidyverse)
# looks like rlang is loaded


# different datasets: pronoun .data$ --------------------------------------

df1 <- tibble(x = 1:3)
a <- 10



# dangerous because the context (here, "a") modifies the result
mutate_y <- function(df) {
  mutate(df, y = a + x)
}
mutate_y(df1)

# use rlang::.data$ to force variables in df
mutate_y <- function(df) {
  mutate(df, y = .data$a + .data$x)
}



# different expressions ---------------------------------------------------

df <- tibble(
  g1 = c(1, 1, 2, 2, 2),
  g2 = c(1, 2, 1, 2, 1),
  a = sample(5),
  b = sample(5)
)

# Ese enquo() inside functions environment; Interactively, use quo().
my_summarise <- function(df, group_by) {
  group_by <- enquo(group_by)
  print(group_by)

  df %>%
    group_by(!!group_by) %>%
    summarise(a = mean(a))
}

my_summarise(df, g1)  # not using quo() because I'm using enquo() inside fun



# Different input variable ------------------------------------------------

df

# Practice interactvively using quo()
my_var <- quo(a)
summarise(df, mean = mean(!!my_var), sum = sum(!!my_var), n = n())

# Write the function, replacing quo() by enquo()
my_summarise2 <- function(df, expr) {
  expr <- enquo(expr)

  summarise(df,
    mean = mean(!!expr),
    sum = sum(!!expr),
    n = n()
  )
}
my_summarise2(df, a)



# Different input and output variable -------------------------------------

# Vary the name of the output variables

#     quo_name() to convert the input expression to a string.
#     := to make valid the expression !!mean_name = mean(!!exp)
my_mutate <- function(df, expr) {
  expr <- enquo(expr)
  mean_name <- paste0("mean_", quo_name(expr))
  sum_name <- paste0("sum_", quo_name(expr))

  mutate(df,
    !!mean_name := mean(!!expr),
    !!sum_name := sum(!!expr)
  )
}

my_mutate(df, a)



# Capturing multiple variables --------------------------------------------

# Use ... in the function definition so our function can accept any number of
# arguments.

# Use quos() to capture all the ... as a list of formulas.

# Use !!! instead of !! to splice the arguments into group_by().

my_summarise <- function(df, ...) {
  group_by <- quos(...)

  df %>%
    group_by(!!!group_by) %>%
    summarise(a = mean(.data$a))  # .data$ is my addition, to be conservative.
}

my_summarise(df, g1, g2)

