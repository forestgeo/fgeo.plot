#' Add alternatives to symbol, that are easier to understand.
#'
#' @param x A data frame.
#'
#' @return The modified data frame.
#' @export
#'
#' @examples
#' sin_q20 <- map(sin_q20, codify_explicitely)
codify_explicitely <- function(x) {
  mutate(x,
    sym1 = case_when(
      symbol == 16 ~ "Alive in 4",
      symbol ==  1 ~ "Alive in 3, Dead in 4",
      symbol == 15 ~ "Alive in 2, Dead in 3",
      symbol ==  0 ~ "Alive in 1, Dead in 2"
    ),
    sym2 = case_when(
      symbol == 16 ~ "Alive",
      symbol ==  1 ~ "Dead",
      symbol == 15 ~ "Dead",
      symbol ==  0 ~ "Dead"
    )
  )
}
