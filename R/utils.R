collapse <- function(...) {
  paste0(..., collapse = ", ")
}

max0 <- function(...) {
  max(..., na.rm = TRUE)
}

min0 <- function(...) {
  min(..., na.rm = TRUE)
}
